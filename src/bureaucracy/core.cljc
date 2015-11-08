(ns bureaucracy.core
  #?(:cljs (:require-macros bureaucracy.core))
  (:require [bureaucracy.util :as util]
            [clojure.set :as set]
            [schema.core :as s]))

;;;;
;;;; State Machine protocol definition
;;;;

(s/defschema State
  "State machine states are represented as keywords. While it's up to you, in
  general, there's no need for state keywords to be namespaced."
  s/Keyword)

(s/defschema Event
  "State machine transitions are triggered by events, which are also identified
  by keywords.  When an event is dispatched by your application code, it is
  passed to every (active) machine in your state machine hierarchy, so to avoid
  accidental collisions between event names for different state machines, it is
  recommended that Event keywords be namespaced (eg `::update`, rather than
  `:update`)."
  s/Keyword)

(defprotocol StateMachine
  "A static (and immutable) specification of the behaviour of a state machine,
  defining the mechanism for deciding a start state (depending on the current
  value of the application DB) and the rules for transitioning between
  states (including effects on the values of the application DB and state
  machine state DB when those transitions occur)."
  (machine-name [this]
    "The state machine's name (as a string) - for descriptive purposes only.")
  (start [this app-db dispatch-queue event-id dispatcher-arg event-arg]
    "Given application state `app-db`, executes whatever might be needed to
    start the state machine, given that the start is being caused by event
    `event-id`, which has been dispatched with args `event-args`.

    Start functions which need to be able to effect subsequent events should be
    given a `dispatcher` function `(make-dispatcher dispatch-queue)`.

    Returns a map of `{:app-db app-db, :state-db state-db}`, which you should
    hold onto as the application's new DB value.")
  (event [this db dispatch-queue event-id dispatcher-arg event-arg]
    "Given a `db` containing the global application state (under `:app-db`), and
    the current state machine state (under `:state-db`), effects any transitions
    appropriate for `event-id` (which is being dispatched with the given
    `event-args`), returning the new values of `app-db` and `state-db`.

    Transition functions which need to be able to effect subsequent events
    should be given a `dispatcher` function `(make-dispatcher
    dispatch-queue)`.")
  (current-state [this state-db]
    "FIXME: document current-state")
  (get-submachine [this path-component]
    "FIXME: document get-submachine")
  (get-substate-db [this state-db path-component]
    "FIXME: document get-substate-db

    Returns nil if the machine is not currently in the given substate.

    FIXME: 'Peer' has no choice but to allow the path-component it passes to its
    child to be anything acceptable to `(current-state child pc)`. Should the
    protocol require other state machines also to be this flexible? Ie should
    Submachine accept :* as the path component?"))

(defn- get-unit [machine]
  (let [next-m (get-submachine machine [])]
    (cond (= machine next-m)
          next-m
          (and (not (satisfies? StateMachine next-m))
               (map? next-m))
          (apply merge (for [[k m] next-m]
                         ;; FIXME remove WTF check
                         (if (satisfies? StateMachine m)
                           {k (get-unit m)}
                           (throw (ex-info "WTF?" {:k k :m m :next-m next-m})))))
          :else
          (recur next-m))))

(defn get-machine
  "FIXME document get-machine"
  [machine path]
  (some-> (reduce (fn [machine path-component]
                    (or (get-submachine machine path-component)
                        (throw (ex-info "Invalid path"
                                        {:machine        machine
                                         :path-component path-component}))))
                  machine
                  path)
          (get-unit)))

(defn- get-local-state [machine state-db]
  (let [next-m (get-submachine machine [])]
    (cond (= machine next-m)
          state-db
          (and (not (satisfies? StateMachine next-m))
               (map? next-m))
          (apply merge (let [next-s (get-substate-db machine state-db [])]
                         (for [[k m] next-m]
                           ;; FIXME remove WTF check
                           (if (satisfies? StateMachine m)
                             {k (get-local-state m (get next-s k))}
                             (throw (ex-info "WTF?" {:k k :m m :next-m next-m}))))))
          :else
          (recur next-m (get-substate-db machine state-db [])))))

(defn get-path
  "FIXME document get-path"
  [machine state-db path]
  (when-let [result (reduce (fn [{:keys [machine state-db]} path-component]
                              (let [next-m (or (get-submachine machine path-component)
                                               (throw (ex-info "Invalid path"
                                                               {:machine        machine
                                                                :path-component path-component})))
                                    next-s (get-substate-db machine state-db path-component)]
                                (if next-s
                                  {:machine next-m, :state-db next-s}
                                  (reduced nil))))
                            {:machine machine, :state-db state-db}
                            path)]
    (let [{:keys [machine state-db]} result]
      {:machine  (get-unit machine)
       :state-db (get-local-state machine state-db)})))

(defn matches-state?
  "FIXME document matches-state?"
  ([match-rule state]
   (cond (= :* match-rule)     (boolean state)
         (= match-rule state)  true
         (map? match-rule)     (every? (fn [[k r]] (matches-state? r (get state k))) match-rule)
         (ifn? match-rule)     (match-rule state)
         :else                 (throw (ex-info "Unsupported match-rule"
                                               {:match-rule match-rule
                                                :state      state}))))
  ([match-rule machine state-db path]
   (let [{:keys [machine state-db]} (get-path machine state-db path)]
     (when (and machine state-db)
       (matches-state? match-rule (current-state machine state-db))))))

(defn- translate-keycode [keycode]
  (case keycode
    13 :enter
    27 :escape
    keycode))

(defn- translate-dom-input-value [input]
  (case (.-type input)
    "file"     (seq (.-files input))
    "checkbox" (.-checked input)
    (.-value input)))

(defn extract-dispatched-value [maybe-js-event]
  #?(:clj
     ;; FIXME if it's a java.io.File, do some equivalent to JS file input?
     maybe-js-event
     :cljs
     (if (and maybe-js-event (or (instance? js/Event maybe-js-event)
                                 (instance? js/Event (.-nativeEvent maybe-js-event))))
       (case (.-type maybe-js-event)
         "blur"    (translate-dom-input-value (.-target maybe-js-event))
         "change"  (translate-dom-input-value (.-target maybe-js-event))
         "input"   (translate-dom-input-value (.-target maybe-js-event))
         "keydown" (translate-keycode (.-which maybe-js-event))
         "keyup"   (translate-keycode (.-which maybe-js-event))
         nil)
       maybe-js-event)))

(defn make-dispatcher
  [queue-atom]
  (s/fn dispatcher
    ([event-id :- Event]
     (dispatcher event-id nil))
    ([event-id :- Event dispatcher-arg]
     (fn dispatch
       ([]
        (dispatch nil))
       ([value]
        (swap! queue-atom conj {:event-id        event-id
                                :dispatcher-arg  dispatcher-arg
                                ;; FIXME perhaps parameterise with a
                                ;; user-supplied function to combine
                                ;; dispatcher-arg and event-arg, also plugging in
                                ;; extract-dispatched-value?  Or maybe
                                ;; dispatcher-arg is always either a keyword, a
                                ;; path, or a function, and keyword/path get you
                                ;; a map?
                                :event-arg       (extract-dispatched-value value)})
        nil)
       ([js-event & args]
        ;; React calls event handlers with the SyntheticEvent as the first arg
        ;; and DOM ID as the second (and sometime js/Event as the third?). We
        ;; don't want the dom-id, but not supplying the two-/three-arg function
        ;; will result in an arity error.
        (dispatch js-event))))))

(s/defn translate-dispatcher [dispatcher event-id-translations-map :- {Event Event}]
  (s/fn
    ([event-id :- Event]
     (if-let [event-id (get event-id-translations-map event-id event-id)]
       (dispatcher event-id)
       (fn [js-event] (println "translate-dispatcher suppressing event" event-id) nil)))
    ([event-id :- Event dispatcher-args]
     (if-let [event-id (get event-id-translations-map event-id event-id)]
       (dispatcher event-id dispatcher-args)
       (fn [js-event] (println "translate-dispatcher suppressing event" event-id "with dispatcher-args" dispatcher-args) nil)))))

(defn consume-dispatch-queue
  "FIXME: document consume-dispatch-queue."
  ([state-machine db dispatch-queue]
   (consume-dispatch-queue state-machine db dispatch-queue nil))
  ([state-machine db dispatch-queue ignored-fn]
   (loop []
     (when-let [the-event (util/dequeue! dispatch-queue)]
       (swap! db (fn [db-val]
                   (let [result (event state-machine
                                       db-val
                                       dispatch-queue
                                       (:event-id the-event)
                                       (:dispatcher-arg the-event)
                                       (:event-arg the-event))]
                     (when (and (= result db-val) ignored-fn)
                       ;; FIXME pass state-machine and db-val, for more
                       ;; informative goodness
                       (ignored-fn (:event-id the-event)
                                   (:dispatcher-arg the-event)
                                   (:event-arg the-event)))
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

(defn- pure-transition [db _ _ _] db)

(s/defrecord Unit [machine-name :- s/Str
                   start        :- State
                   transitions  :- {State {Event [State (s/pred fn?)]}}]
  StateMachine
  (machine-name [_]
    machine-name)
  (start [_ app-db dispatch-queue event-id dispatcher-arg event-arg]
    (let [db             {:app-db app-db}
          transition-map (get transitions start)
          dispatcher     (make-dispatcher dispatch-queue)]
      ;; if the event-id is in transition-map, use that, otherwise ::enter
      (if-let [[new-state transition-fn] (or (get transition-map event-id)
                                             (get transition-map enter))]
        (let [transition-fn (or transition-fn pure-transition)]
          (transition-fn (assoc-in db [:state-db :state] new-state)
                         dispatcher
                         dispatcher-arg
                         event-arg))
        (assoc-in db [:state-db :state] start))))
  (event [this db dispatch-queue event-id dispatcher-arg event-arg]
    (let [transition-map (get transitions (:state (:state-db db)))]
      (when-not transition-map
        (throw (ex-info "Invalid state machine state" {:machine this
                                                       :state   (:state (:state-db db))})))
      (if-let [[new-state transition-fn] (get transition-map event-id)]
        (let [transition-fn (or transition-fn pure-transition)]
          (-> (transition-fn db
                             (make-dispatcher dispatch-queue)
                             dispatcher-arg
                             event-arg)
              (assoc-in [:state-db :state] new-state)))
        ;; this state-machine doesn't have a transition for this event
        db)))
  (current-state [this state-db]
    (:state state-db))
  (get-submachine [this path-component]
    (when (= [] path-component)
      this))
  (get-substate-db [this state-db path-component]
    (when (= [] path-component)
      state-db)))


(s/defn unit :- (s/protocol StateMachine)
  "Create a basic state machine defined by the given transitions.

  `start` is either a keyword defining the machine's start state, or a function
  to be called when the state machine starts.  The function must accept an
  app-db and dispatch function and return `{:app-db app-db, :state-db
  state-db}`, thereby defining both the start state (as a `:state` key in
  state-db) and the new value of the `app-db` once the machine's start state has
  been entered.

  `transitions` is a map of `State`s to transitions which can occur in those
  state. The transitions themselves are either bare keywords (defining the 'to'
  state) or [keyword function] pairs, where the function is a transition
  function, to be called when the transition occurs."
  [{:keys [machine-name start transitions]}]
  (map->Unit {:machine-name machine-name
              :start        start
              :transitions  (into {}
                                  (for [[from-state tmap] transitions]
                                    [from-state (into {}
                                                      (for [[event to-state-vec] tmap]
                                                        (if (keyword? to-state-vec)
                                                          [event [to-state-vec nil]]
                                                          [event to-state-vec])))]))}))

(defn defmachine*
  [machine-name start-state start-state-transitions & {:as states-and-transitions}]
  (unit {:machine-name machine-name
         :start        start-state
         :transitions  (assoc states-and-transitions
                              start-state start-state-transitions)}))

#?
(:clj
 (defmacro defmachine
   "`def`s a 'unit' state machine with the given transitions. The arguments
   after the name are key-value pairs defining a transitions map (see `unit`).
   The first key-value pair defines the start state for the machine.

   FIXME: use `tools.macro/name-with-attributes` for docstring & metadata
   support."
   [machine-name start-state start-state-transitions & states-and-transitions]
   `(def ~machine-name
      (defmachine* ~(str *ns* "/" (name machine-name))
        ~start-state
        ~start-state-transitions
        ~@states-and-transitions))))


;;;;
;;;; Lensed State Machine
;;;;

(defn- vectorize [x] (if (vector? x) x [x]))

;; Lenses with thanks to Christophe Grand: https://gist.github.com/cgrand/5683844
(defprotocol Lens
  (-get [lens data])
  (-put [lens data val]))

(s/defrecord PathLens [path-map]
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

(s/defrecord LensedStateMachine [lens :- Lens, machine :- StateMachine]
  StateMachine
  (machine-name [_]
    (machine-name machine))
  (start [_ app-db dispatch-queue event-id dispatcher-arg event-arg]
    (let [lensed-db (-get lens app-db)
          result    (start machine
                           lensed-db
                           dispatch-queue
                           event-id
                           dispatcher-arg
                           event-arg)]
      (update-in result [:app-db] (partial -put lens app-db))))
  (event [_ db dispatch-queue event-id dispatcher-arg event-arg]
    (let [lensed-db (-get lens (:app-db db))
          result    (event machine
                           (assoc db :app-db lensed-db)
                           dispatch-queue
                           event-id
                           dispatcher-arg
                           event-arg)]
      (update-in result [:app-db] (partial -put lens (:app-db db)))))
  (current-state [_ state-db]
    (current-state machine state-db))
  (get-submachine [_ path-component]
    (get-submachine machine path-component))
  (get-substate-db [_ state-db path-component]
    (get-substate-db machine state-db path-component)))

(defn with-lens
  "FIXME: with-lens docstring."
  [lens machine]
  (LensedStateMachine. lens machine))


;;;;
;;;; Sub-State Machines (parent machines with each state able to have submachines)
;;;;

(s/defrecord Submachine [machine     :- (s/protocol StateMachine)
                         submachines :- {s/Keyword {:submachine (s/protocol StateMachine)
                                                    :sublens    (s/protocol Lens)}}]
  StateMachine
  (machine-name [_]
    (machine-name machine))
  (start [_ app-db dispatch-queue event-id dispatcher-arg event-arg]
    (let [machine-result (start machine
                                app-db
                                dispatch-queue
                                event-id
                                dispatcher-arg
                                event-arg)
          state          (:state (:state-db machine-result))]
      (if-let [{:keys [submachine sublens]} (get submachines state)]
        (let [submachine-result (start submachine
                                       (:app-db (if sublens
                                                  (-get sublens machine-result)
                                                  machine-result))
                                       dispatch-queue
                                       event-id
                                       dispatcher-arg
                                       event-arg)]
          ;; FIXME if sublens results in child's app-db update being put in
          ;; state-db instead, it will be silently dropped
          (-> machine-result
              (assoc-in [:app-db] (:app-db (-put sublens machine-result submachine-result)))
              (assoc-in [:state-db ::submachine-db] (:state-db submachine-result))))
        machine-result)))
  (event [_ db dispatch-queue event-id dispatcher-arg event-arg]
    (let [machine-db                   (update-in db [:state-db] dissoc ::submachine-db)
          machine-result               (event machine
                                              machine-db
                                              dispatch-queue
                                              event-id
                                              dispatcher-arg
                                              event-arg)
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
                                         event-id
                                         dispatcher-arg
                                         event-arg)]
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
                             :state-db (::submachine-db (:state-db db))}
              submachine-result (event submachine
                                       submachine-db
                                       dispatch-queue
                                       event-id
                                       dispatcher-arg
                                       event-arg)]
          ;; FIXME if sublens results in child's app-db update being put in
          ;; state-db instead, it will be silently dropped
          (-> machine-result
              (assoc-in [:state-db ::submachine-db] (:state-db submachine-result))
              (assoc-in [:app-db] (:app-db (-put sublens machine-result submachine-result)))))

        ;; `machine`'s state changed: we're entering a new substate
        :else
        (let [submachine-result (start submachine
                                       (:app-db (-get sublens machine-result))
                                       dispatch-queue
                                       event-id
                                       dispatcher-arg
                                       event-arg)]
          ;; FIXME if sublens results in child's app-db update being put in
          ;; state-db instead, it will be silently dropped
          (-> machine-result
              (assoc-in [:state-db ::submachine-db] (:state-db submachine-result))
              (assoc-in [:app-db] (:app-db (-put sublens machine-result submachine-result))))))))
  (current-state [_ state-db]
    (current-state machine (dissoc state-db ::submachine-db)))
  (get-submachine [this path-component]
    (cond (= [] path-component)
          machine
          (not (keyword? path-component))
          (throw (ex-info "Submachine StateMachine only supports keyword path components"
                          {:machine this, :path-component path-component}))
          :else
          (or (:submachine (get submachines path-component))
              (throw (ex-info "Not a valid submachine for this Submachine StateMachine"
                              {:machine this, :path-component path-component})))))
  (get-substate-db [this state-db path-component]
    (cond (= [] path-component)
          (dissoc state-db ::submachine-db)

          (not (keyword? path-component))
          (throw (ex-info "Submachine StateMachine only supports keyword path components"
                          {:machine this, :path-component path-component}))

          (not (contains? submachines path-component))
          (throw (ex-info "Not a valid submachine for this Submachine StateMachine"
                          {:machine this, :path-component path-component}))

          (= path-component (current-state machine state-db))
          (::submachine-db state-db))))

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

(s/defrecord Peer [machine-name :- s/Any
                   submachines  :- [[(s/one s/Any 'name) (s/one (s/protocol StateMachine) 'machine)]]]
  StateMachine
  (machine-name [_]
    machine-name)
  (start [_ app-db dispatch-queue event-id dispatcher-arg event-arg]
    (let [results (reduce (fn [accum [name machine]]
                            (let [result (start machine
                                                (:app-db accum)
                                                dispatch-queue
                                                event-id
                                                dispatcher-arg
                                                event-arg)]
                              {:app-db    (:app-db result)
                               :state-dbs (assoc (:state-dbs accum) name (:state-db result))}))
                          {:app-db app-db, :state-dbs {}}
                          submachines)]
      {:app-db   (:app-db results)
       :state-db {:state           :peer
                  ::submachine-dbs (:state-dbs results)}}))
  (event [_ db dispatch-queue event-id dispatcher-arg event-arg]
    (let [results (reduce (fn [accum [name machine]]
                            (let [result (event machine
                                                {:app-db   (:app-db accum)
                                                 :state-db (-> db :state-db ::submachine-dbs (get name))}
                                                dispatch-queue
                                                event-id
                                                dispatcher-arg
                                                event-arg)]
                              {:app-db    (:app-db result)
                               :state-dbs (assoc (:state-dbs accum) name (:state-db result))}))
                          {:app-db (:app-db db), :state-dbs {}}
                          submachines)]
      {:app-db   (:app-db results)
       :state-db {:state           :peer
                  ::submachine-dbs (:state-dbs results)}}))
  (current-state [this state-db]
    (apply merge (for [[k sm] submachines]
                   {k (current-state sm (get (::submachine-dbs state-db) k))})))
  (get-submachine [this path-component]
    (cond (= [] path-component)
          (apply merge (for [[k sm] submachines] {k sm}))

          (and (keyword? path-component)
               (first (keep #(when (= path-component (first %)) (second %)) submachines)))
          (first (keep #(when (= path-component (first %)) (second %)) submachines))

          (keyword? path-component)
          (throw (ex-info "Not a valid submachine for this Peer StateMachine"
                          {:machine this, :path-component path-component}))

          (and (map? path-component)
               (= 1 (count path-component)))
          (let [[k substate] (first path-component)]
            (if-let [submachine (first (keep #(when (= k (first %)) (second %)) submachines))]
              (get-submachine submachine substate)
              (throw (ex-info "Not a valid submachine for this Peer StateMachine"
                              {:machine this, :path-component path-component, :submachine-name k}))))

          :else
          (throw (ex-info (str "Peer StateMachine only supports `submachine-name` or "
                               "`{submachine-name submachine-state}` path components.")
                          {:machine this, :path-component path-component}))))
  (get-substate-db [this state-db path-component]
    (cond (= [] path-component)
          (::submachine-dbs state-db)

          (keyword? path-component)
          (get (::submachine-dbs state-db) path-component)

          (and (map? path-component)
               (= 1 (count path-component)))
          (let [[k substate] (first path-component)
                submachine   (get-submachine this k)
                substate-db  (get (::submachine-dbs state-db) k)]
            (get-substate-db submachine substate-db substate))

          :else
          (throw (ex-info (str "Peer StateMachine only supports `submachine-name` or "
                               "`{submachine-name submachine-state}` path components.")
                          {:machine this, :path-component path-component})))))

(defn peer
  "FIXME: peer docstring."
  [machine-name & names-and-machines]
  (Peer. machine-name (partition 2 names-and-machines)))
