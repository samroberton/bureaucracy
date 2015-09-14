(ns bureaucracy.core
  (:refer-clojure :exclude [defrecord])
  #?@(:clj
      [(:require [clojure.set :as set]
                 [schema.core :as s :refer [defschema defrecord]])]
      :cljs
      [(:require-macros bureaucracy.core
                        [schema.core :refer [defschema defrecord]])
       (:require [clojure.set :as set]
                 [schema.core :as s])]))


;;;;
;;;; State Machine protocol definition
;;;;

(defschema State
  "The enumerated states of a state machine are represented as keywords. While
  it's up to you, in general, there's no need for state keywords to be
  namespaced."
  s/Keyword)

(defschema Event
  "The events which (might) trigger state machine transitions are represented as
  keywords. When an event is dispatched by your application code, it is passed
  to every (active) machine in your state machine hierarchy, so to avoid
  accidental collisions between event names for different state machines, it is
  recommended that Event keywords be namespaced (eg `::update`, rather than
  `:update`)."
  s/Keyword)

(defschema AppDB
  "AppDB is the portion of a system's DB that holds the global state of the
  application: data which is not necessarily directly related to the current
  state of the system's state machine. It is opaque to `bureaucracy`: you can
  put whatever you want there."
  s/Any)

(defschema StateDB
  "StateDB is the portion of a system's DB that holds the current state of the
  system's state machine: the state machine's \"working memory\", in effect. It
  must contain a `:state` keyword, but otherwise, its contents are entirely the
  business of your state machine.

  That said, the StateDB a composed state machine sees is also composed. It
  combines the \"parent\" machine's \"working memory\" with the StateDBs of its
  child state machines."
  {:state State, s/Any s/Any})

(defschema DispatchFn
  "The application effects change in the state machine by calling the dispatch
  function, supplying an Event (keyword) and optionally some arguments for the
  state machine's transition function(s):
      `(dispatch ::my-event an-arg another-arg 3 ...)`

  Generally, you obtain a dispatch function by rendering your view with
  `bureaucracy.view/render-view`, but you can get one by calling
  `make-dispatch-fn` if you want.

  The dispatch function is called for side effects, and returns nil."
  (s/make-fn-schema (s/eq nil)
                    [[(s/one s/Keyword 'event-id) s/Any]]))

(defschema TransitionFn
  "State machines define (optional) transition functions which will be called
   when your state machine receives an event. A transition function is given a
   dispatch function, the current DB and whatever arguments your application
   passed to `dispatch` when dispatching the current event, and it returns the
   new DB:

      `(fn [dispatch-fn {:keys [app-db state-db]} & dispatch-args]
         {:app-db new-app-db, :state-db new-sm-db})`

  `dispatch-fn` can be used to dispatch the result of an asynchronous operation
  initiated by this transition, for example as an AJAX callback handler."
  (s/make-fn-schema {:app-db AppDB, :state-db StateDB}
                    [[(s/one {:app-db AppDB, :state-db StateDB} 'db)
                      (s/one DispatchFn 'dispatch)
                      s/Any]]))

(defprotocol StateMachine
  "A static (and immutable) description of the behaviour of a state machine,
  specifying the mechanism for deciding a start state (depending on the current
  value of the application DB) and the rules for transitioning between
  states (including effects on the values of the application DB and state
  machine state when those transitions occur).

  A StateMachine does NOT encapsulate the current state of the system it
  governs: that data lives in the `db` (`:app-db` and `:state-db`) which is
  passed to the protocol methods, and the protocol methods return the new DB
  values."
  (start [this app-db dispatch-queue dispatcher-path event-id event-args]
    "Given application state `app-db`, executes whatever might be needed to
    start the state machine, given that the start is being caused by event
    `event-id`, which has been dispatched with args `event-args`.

    Start functions which need to be able to effect subsequent events should be
    given a `dispatch` function `(make-dispatch-fn dispatch-queue
    dispatcher-path)`.

    Returns a map of `{:app-db app-db, :state-db state-db}`, which you should
    hold onto as the application's new DB value.")
  (event [this db dispatch-queue dispatcher-path event-id event-args]
    "Given a `db` containing the global application state (under `:app-db`), and
    the current state machine state (under `:state-db`), effects any transitions
    appropriate for `event-id` (which is being dispatched with the given
    `event-args`), returning the new values of `app-db` and `state-db`.

    Transition functions which need to be able to effect subsequent events
    should be given a `dispatch` function `(make-dispatch-fn dispatch-queue
    dispatcher-path)`.")
  (get-state [this state-db matching-states]
    "Return `state-db` if the current state matches `matching-states`, where
    `matching-states` is a path-like structure identifying a potentially nested
    state in the state machine hierarchy.

    StateMachine implementations which in some way wrap other StateMachines, for
    example to allow for sub-states, should implement this method to allow
    retrieval of their child machines in some meaningful way. Return nil if
    there is no such child, or for StateMachines which don't have a concept of
    children.")
  (describe-state [this state-db]
    "Describe the state in a path-like structure that could be used as a
    `matching-states` argument to `get-state`."))

(defn- resolve-dispatch-args [args]
  (let [[maps event-id event-args] (loop [maps ()
                                          args args]
                                     (if (keyword? (first args))
                                       [maps (first args) (rest args)]
                                       (recur (conj maps (first args)) (rest args))))]
    (loop [maps maps
           resolved-event-id event-id]
      (if (seq maps)
        (let [m (first maps)]
          (when-not (map? m)
            (throw (ex-info
                    (str "Only maps are allowed as arguments to `dispatch` before the event-id. "
                         "Perhaps you forgot to include the event-id you intended in your call to "
                         "`dispatch`? Or perhaps you tried to dispatch an event-id that's not a "
                         "keyword?")
                    {:dispatch-args     args
                     :original-event-id event-id
                     :resolved-event-id resolved-event-id
                     :not-a-map         m})))
          (let [next-event-id (get m resolved-event-id resolved-event-id)]
            (when next-event-id
              (recur (rest maps) next-event-id))))
        [resolved-event-id event-args]))))

(defn make-dispatch-fn
  "FIXME: document make-dispatch-fn."
  [queue-atom dispatcher-path]
  (fn [event-id & event-args]
    (when-let [[event-id event-args] (resolve-dispatch-args (cons event-id event-args))]
      (swap! queue-atom conj {:event-id        event-id
                              :event-args      event-args
                              :dispatcher-path dispatcher-path}))
    nil))

(defn- dequeue!
  "Dequeue an item from a persistent queue which is stored as the value of the
  given atom, setting the atom to the new queue (minus dequeued item) and
  returning the dequeued item. If the queue is empty, does not alter it and
  returns nil."
  [queue-atom]
  (let [queue @queue-atom]
    (when (seq queue)
      (if (compare-and-set! queue-atom queue (pop queue))
        (peek queue)
        (recur queue-atom)))))

(defn consume-dispatch-queue
  "FIXME: document consume-dispatch-queue."
  ([state-machine db dispatch-queue]
   (consume-dispatch-queue state-machine db dispatch-queue nil))
  ([state-machine db dispatch-queue ignored-fn]
   (loop []
     (when-let [{:keys [event-id event-args dispatcher-path]} (dequeue! dispatch-queue)]
       (swap! db (fn [db-val]
                   (let [result (event state-machine
                                       db-val
                                       dispatch-queue
                                       dispatcher-path
                                       event-id
                                       event-args)]
                     (when (and (= result db-val) ignored-fn)
                       (ignored-fn event-id event-args))
                     result)))
       (recur)))))

;;;;
;;;; Unit State Machine
;;;;

(def enter
  "A 'special' event ID which a Unit StateMachine can use to designate a
  transition that should be performed when the state machine is started, if no
  other event ID matches."
  ::enter)

(defschema StartFn
  (s/make-fn-schema {:app-db AppDB, :state-db StateDB}
                    [[(s/one AppDB 'app-db)
                      (s/one DispatchFn 'dispatch)
                      (s/one Event 'event-id)
                      s/Any]]))

(defrecord Unit [start       :- State
                 transitions :- {State {Event [State TransitionFn]}}]
  StateMachine
  (start [_ app-db dispatch-queue dispatcher-path event-id event-args]
    (let [db             {:app-db app-db}
          transition-map (get transitions start)
          dispatch       (make-dispatch-fn dispatch-queue dispatcher-path)]
      ;; if the event-id is in transition-map, use that, otherwise ::enter
      (if-let [[new-state transition-fn] (or (get transition-map event-id)
                                             (get transition-map enter))]
        (apply transition-fn (assoc-in db [:state-db :state] new-state) dispatch event-args)
        (assoc-in db [:state-db :state] start))))
  (event [this db dispatch-queue dispatcher-path event-id event-args]
    (let [transition-map (get transitions (:state (:state-db db)))]
      (when-not transition-map
        (throw (ex-info "Invalid state machine state" {:machine this
                                                       :state   (:state (:state-db db))})))
      (if-let [[new-state transition-fn] (get transition-map event-id)]
        (-> (apply transition-fn db (make-dispatch-fn dispatch-queue dispatcher-path) event-args)
            (assoc-in [:state-db :state] new-state))
        ;; this state-machine doesn't have a transition for this event
        db)))
  (get-state [this state-db matching-states]
    (let [matching-states (if (sequential? matching-states)
                            (if (< 1 (count matching-states))
                              (throw (ex-info (str "Can't get-state for nested states from unit "
                                                   "state machine")
                                              {:machine this
                                               :state-db state-db
                                               :matching-states matching-states}))
                              (first matching-states))
                            matching-states)]
      (when (cond (= :* matching-states)     true
                  (keyword? matching-states) (= matching-states (:state state-db))
                  (set? matching-states)     (matching-states (:state state-db))
                  :else                      (throw (ex-info (str "Unsupported matching-states "
                                                                  "argument for unit state machine")
                                                             {:machine this
                                                              :state-db state-db
                                                              :matching-states matching-states})))
        state-db)))
  (describe-state [_ state-db]
    (:state state-db)))


(defn- pure-transition [db _ & _] db)

(s/defn unit :- (s/protocol StateMachine)
  "Create a basic state machine defined by the given transitions.

  `start` is either a keyword defining the machine's start state, or a function
  to be called when the state machine starts.  The function must accept an AppDB
  and DispatchFn and return `{:app-db app-db, :state-db
  state-db}`, thereby defining both the start state (as a `:state` key
  in state-db) and the new value of the `app-db` once the machine's
  start state has been entered.

  `transitions` is a map of `State`s to transitions which can occur in those
  state. The transitions themselves are either bare keywords (defining the 'to'
  state) or [keyword function] pairs, where the function is a `TransitionFn` to
  be called when the transition occurs."
  [{:keys [start transitions]}]
  (map->Unit {:start       start
              :transitions (into {}
                                 (for [[from-state tmap] transitions]
                                   [from-state (into {}
                                                     (for [[event to-state-vec] tmap]
                                                       (if (keyword? to-state-vec)
                                                         [event [to-state-vec pure-transition]]
                                                         [event to-state-vec])))]))}))

#?
(:clj
 (defmacro defmachine
   "`def`s a 'unit' state machine with the given transitions. The arguments
   after the name are key-value pairs defining a transitions map (see `unit`).
   The first key-value pair defines the start state for the machine.

   FIXME: use `tools.macro/name-with-attributes` for docstring & metadata
   support."
   [machine-name start-state start-state-transitions & {:as states-and-transitions}]
   `(def ~machine-name
      (unit {:start       ~start-state
             :transitions ~(assoc states-and-transitions
                                  start-state start-state-transitions)}))))


;;;;
;;;; Lensed State Machine
;;;;

(defn- vectorize [x] (if (vector? x) x [x]))

;; Lenses with thanks to Christophe Grand: https://gist.github.com/cgrand/5683844
(defprotocol Lens
  (-get [lens data])
  (-put [lens data val]))

(defrecord PathLens [path-map]
  Lens
  (-get [_ data]
    (loop [[[src-path tgt-path] & rem] (seq path-map)
           result {}]
      (let [next-result (assoc-in result (vectorize src-path) (get-in data (vectorize tgt-path)))]
        (if (seq rem)
          (recur (seq rem) next-result)
          next-result))))
  (-put [_ data val]
    (loop [[[src-path tgt-path] & rem] (seq path-map)
           data     data]
      (let [result (assoc-in data (vectorize src-path) (get-in val (vectorize tgt-path)))]
        (if (seq rem)
          (recur (seq rem) result)
          result)))))

(def identity-lens
  (reify Lens
    (-get [_ data] data)
    (-put [_ data val] val)))

(defn path-lens [path-map] (PathLens. path-map))

(defrecord LensedStateMachine [lens :- Lens, machine :- StateMachine]
  StateMachine
  (start [_ app-db dispatch-queue dispatcher-path event-id event-args]
    (let [lensed-db (-get lens app-db)
          result    (start machine lensed-db dispatch-queue dispatcher-path event-id event-args)]
      (update-in result [:app-db] (partial -put lens app-db))))
  (event [_ db dispatch-queue dispatcher-path event-id event-args]
    (let [lensed-db (-get lens (:app-db db))
          result    (event machine
                           (assoc db :app-db lensed-db)
                           dispatch-queue
                           dispatcher-path
                           event-id
                           event-args)]
      (update-in result [:app-db] (partial -put lens (:app-db db)))))
  (get-state [_ state-db matching-states]
    (get-state machine state-db matching-states))
  (describe-state [_ state-db]
    (describe-state machine state-db)))

(defn with-lens
  "FIXME: with-lens docstring."
  [lens machine]
  (LensedStateMachine. lens machine))


;;;;
;;;; Sub-State Machines (parent machines with each state able to have submachines)
;;;;

(defrecord Submachine [machine     :- (s/protocol StateMachine)
                       submachines :- {s/Keyword {:submachine (s/protocol StateMachine)
                                                  :sublens    (s/protocol Lens)}}]
  StateMachine
  (start [_ app-db dispatch-queue dispatcher-path event-id event-args]
    (let [machine-result (start machine app-db dispatch-queue dispatcher-path event-id event-args)
          state          (:state (:state-db machine-result))]
      (if-let [{:keys [submachine sublens]} (get submachines state)]
        (let [submachine-result (start submachine
                                       (:app-db (-get sublens machine-result))
                                       dispatch-queue
                                       (conj dispatcher-path state)
                                       event-id
                                       event-args)]
          ;; FIXME if sublens results in child's app-db update being put in
          ;; state-db instead, it will be silently dropped
          (-> machine-result
              (assoc-in [:app-db] (:app-db (-put sublens machine-result submachine-result)))
              (assoc-in [:state-db ::submachine-db] (:state-db submachine-result))))
        machine-result)))
  (event [_ db dispatch-queue dispatcher-path event-id event-args]
    (let [machine-db                   (update-in db [:state-db] dissoc ::submachine-db)
          machine-result               (event machine
                                              machine-db
                                              dispatch-queue
                                              dispatcher-path
                                              event-id
                                              event-args)
          state                        (:state (:state-db machine-result))
          {:keys [submachine sublens]} (get submachines state)]
      (cond
        ;; no submachine for this state: no more to do
        (not submachine)
        machine-result

        ;; `machine` didn't transition: give the submachine a go instead
        (= machine-result machine-db)
        (let [lensed-db     (-get sublens machine-db)
              submachine-db {:app-db   (:app-db lensed-db)
                             :state-db (::submachine-db (:state-db db))}]
          (let [submachine-result (event submachine
                                         submachine-db
                                         dispatch-queue
                                         (conj dispatcher-path state)
                                         event-id
                                         event-args)]
            ;; FIXME if sublens results in child's app-db update being put in
            ;; state-db instead, it will be silently dropped
            (-> db
                (assoc-in [:state-db ::submachine-db] (:state-db submachine-result))
                (assoc-in [:app-db] (:app-db (-put sublens machine-db submachine-result))))))

        ;; `machine`'s state is unchanged after the transition: give the
        ;; submachine a go as well
        (= (:state (:state-db machine-result))
           (:state (:state-db db)))
        (let [lensed-db     (-get sublens machine-result)
              submachine-db {:app-db   (:app-db lensed-db)
                             :state-db (::submachine-db (:state-db db))}]
          (let [submachine-result (event submachine
                                         submachine-db
                                         dispatch-queue
                                         (conj dispatcher-path state)
                                         event-id
                                         event-args)]
            ;; FIXME if sublens results in child's app-db update being put in
            ;; state-db instead, it will be silently dropped
            (-> machine-result
                (assoc-in [:state-db ::submachine-db] (:state-db submachine-result))
                (assoc-in [:app-db] (:app-db (-put sublens machine-result submachine-result))))))

        ;; `machine`'s state changed: we're entering a new substate
        :else
        (let [submachine-result (start submachine
                                       (:app-db (-get sublens machine-result))
                                       dispatch-queue
                                       (conj dispatcher-path state)
                                       event-id
                                       event-args)]
          ;; FIXME if sublens results in child's app-db update being put in
          ;; state-db instead, it will be silently dropped
          (-> machine-result
              (assoc-in [:state-db ::submachine-db] (:state-db submachine-result))
              (assoc-in [:app-db] (:app-db (-put sublens machine-result submachine-result))))))))
  (get-state [this state-db matching-states]
    (let [[node-rule & rest-path] (if (sequential? matching-states)
                                    matching-states
                                    [matching-states])]
      ;; Whether the current node matches is determined by `machine` (which is
      ;; probably a unit machine, or will delegate to one).
      (when-let [machine-state (get-state machine (dissoc state-db ::submachine-db) node-rule)]
        (if rest-path
          (get-state (:submachine (get submachines (:state state-db)))
                     (::submachine-db state-db)
                     rest-path)
          machine-state))))
  (describe-state [_ state-db]
    (if-let [sm (:submachine (get submachines (:state state-db)))]
      (let [child-states (describe-state sm (::submachine-db state-db))
            child-states (if (sequential? child-states) child-states [child-states])]
        (cons (:state state-db) child-states))
      (:state state-db))))

(defn with-substates
  "FIXME: with-substates docstring."
  [machine submachines]
  (Submachine. machine (into {} (for [[k sm] submachines]
                                  [k (if (satisfies? StateMachine sm)
                                       {:submachine sm, :sublens identity-lens}
                                       sm)]))))


;;;;
;;;; Peer State Machines
;;;;

(defrecord Peer [submachines :- [[(s/one s/Any 'name) (s/one (s/protocol StateMachine) 'machine)]]]
  StateMachine
  (start [_ app-db dispatch-queue dispatcher-path event-id event-args]
    (let [results (reduce (fn [accum [name machine]]
                            (let [result (start machine
                                                (:app-db accum)
                                                dispatch-queue
                                                (conj dispatcher-path name)
                                                event-id
                                                event-args)]
                              {:app-db    (:app-db result)
                               :state-dbs (assoc (:state-dbs accum) name (:state-db result))}))
                          {:app-db app-db, :state-dbs {}}
                          submachines)]
      {:app-db   (:app-db results)
       :state-db {:state           :peer
                  ::submachine-dbs (:state-dbs results)}}))
  (event [_ db dispatch-queue dispatcher-path event-id event-args]
    (let [results (reduce (fn [accum [name machine]]
                            (let [result (event machine
                                                {:app-db   (:app-db accum)
                                                 :state-db (-> db :state-db ::submachine-dbs (get name))}
                                                dispatch-queue
                                                (conj dispatcher-path name)
                                                event-id
                                                event-args)]
                              {:app-db    (:app-db result)
                               :state-dbs (assoc (:state-dbs accum) name (:state-db result))}))
                          {:app-db (:app-db db), :state-dbs {}}
                          submachines)]
      {:app-db   (:app-db results)
       :state-db {:state           :peer
                  ::submachine-dbs (:state-dbs results)}}))
  (get-state [this state-db matching-states]
    (when-not (and (map? (first matching-states))
                   (= 1 (count (first matching-states))))
      (throw (ex-info (str "(get-state) on a 'peer' state machine must supply a map "
                           "`{:submachine-name :submachine-state}`")
                      {:machine this, :matching-states matching-states})))
    (let [[sub-name sub-rule] (ffirst matching-states)
          rest-path           (rest matching-states)
          submachine          (first (keep (fn [[n m]]
                                             (when (= sub-name n) m))
                                           submachines))]
      (when-not submachine
        (throw (ex-info "No such submachine"
                        {:machine this, :submachine-name sub-name, :matching-states matching-states})))
      (let [sub-state-db (-> state-db ::submachine-dbs (get sub-name))]
        (when-let [machine-state (get-state submachine sub-state-db sub-rule)]
          (if (seq rest-path)
            (get-state submachine sub-state-db (cons sub-rule rest-path))
            machine-state)))))
  (describe-state [_ state-db]
    (set (for [[name machine] submachines]
           (let [this-desc {name (-> state-db ::submachine-dbs (get name) :state)}
                 rest-desc (describe-state machine (-> state-db ::submachine-dbs (get name)))]
             (if (sequential? rest-desc)
               [this-desc (rest rest-desc)]
               this-desc))))))

(defn peer
  "FIXME: peer docstring."
  [& names-and-machines]
  (Peer. (partition 2 names-and-machines)))
