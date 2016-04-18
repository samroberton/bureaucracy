(ns bureaucracy.core
  #?(:cljs (:require-macros bureaucracy.core))
  (:require [bureaucracy.util :as util]
            [schema.core :as s]))

;;;;
;;;; State Machine protocol definition
;;;;

(s/defschema StateId
  "State machine states are represented as keywords. While it's up to you, in
  general, there's no need for state keywords to be namespaced."
  s/Keyword)

(s/defschema EventId
  "State machine transitions are triggered by events, which are also identified
  by keywords.  When an event is dispatched by your application code, it is
  passed to every (active) machine in your state machine hierarchy, so to avoid
  accidental collisions between event names for different state machines, it is
  recommended that event keywords be namespaced (eg `::update`, rather than
  `:update`)."
  s/Keyword)

(s/defrecord Event [id :- EventId, dispatcher-arg :- s/Any, event-arg :- s/Any])

(defn single-event-arg [{:keys [dispatcher-arg event-arg]}]
  (if dispatcher-arg
    dispatcher-arg
    event-arg))

(s/defschema Output
  {:id                           s/Keyword
   (s/optional-key :handle-path) [s/Any]
   s/Any                         s/Any})

(defprotocol StateMachine
  "A static (and immutable) specification of the behaviour of a state machine,
  defining the mechanism for deciding a start state (depending on the current
  value of the application DB) and the rules for transitioning between
  states (including effects on the values of the application DB and state
  machine state DB when those transitions occur)."
  (machine-name [this]
    "The state machine's name (as a string) - for descriptive purposes only.")
  (start [this app-db event]
    "Given application state `app-db`, executes whatever might be needed to
    start the state machine, given that the start is being caused by event
    `event-id`, which has been dispatched with args `event-args`.

    Returns a map of `{:app-db app-db, :state-db state-db}`, which you should
    hold onto as the application's new DB value.")
  (transition [this db event]
    "Given a `db` containing the global application state (under `:app-db`), and
    the current state machine state (under `:state-db`), effects any transitions
    appropriate for `event-id` (which is being dispatched with the given
    `event-args`), returning the new values of `app-db` and `state-db`.")
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
  "Given a composed state machine hierarchy, retrieve the `StateMachine` in the
  hierarchy at `path` (which might itself still be a composed state machine
  hierarchy)."
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
  "Given a composed state machine hierarchy and a `state-db` which belongs to
  that machine, retrieve the `StateMachine` and `state-db` in the hierarchy at
  `path` (which might still identify a composed state machine hierarchy)."
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


;;;;
;;;; Event dispatching
;;;;

;; FIXME Introduce a notion of an 'event-arg translator' to handle eg
;; js/Event -> event-arg.

(defn- translate-keycode [keycode]
  (case keycode
    13 :enter
    27 :escape
    keycode))

#?(:cljs
   (defn- translate-dom-input-value [input]
     (case (.-type input)
       "file"     (array-seq (.-files input))
       "checkbox" (.-checked input)
       (.-value input))))

(defn extract-dispatched-value [maybe-js-event]
  #?(:clj
     ;; FIXME if it's a java.io.File, do some equivalent to JS file input?
     maybe-js-event
     :cljs
     (if (and maybe-js-event
              (exists? (aget js/window "Event"))
              (or (instance? (aget js/window "Event") maybe-js-event)
                  (instance? (aget js/window "Event") (aget maybe-js-event "nativeEvent"))))
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
    ([event-id :- EventId]
     (dispatcher event-id nil))
    ([event-id :- EventId dispatcher-arg]
     (fn dispatch
       ([]
        (dispatch nil))
       ([value]
        (swap! queue-atom conj {:id             event-id
                                :dispatcher-arg dispatcher-arg
                                ;; FIXME perhaps parameterise with a
                                ;; user-supplied function to combine
                                ;; dispatcher-arg and event-arg, also plugging in
                                ;; extract-dispatched-value?  Or maybe
                                ;; dispatcher-arg is always either a keyword, a
                                ;; path, or a function, and keyword/path get you
                                ;; a map?
                                :event-arg      (extract-dispatched-value value)})
        nil)
       ([js-event & args]
        ;; React calls event handlers with the SyntheticEvent as the first arg
        ;; and DOM ID as the second (and sometime js/Event as the third?). We
        ;; don't want the dom-id, but not supplying the two-/three-arg function
        ;; will result in an arity error.
        (dispatch js-event))))))

(s/defn translate-dispatcher [dispatcher event-id-translations-map :- {EventId EventId}]
  (s/fn
    ([event-id :- EventId]
     (if-let [event-id (get event-id-translations-map event-id event-id)]
       (dispatcher event-id)
       (fn [js-event] (println "translate-dispatcher suppressing event" event-id) nil)))
    ([event-id :- EventId dispatcher-arg]
     (if-let [event-id (get event-id-translations-map event-id event-id)]
       (dispatcher event-id dispatcher-arg)
       (fn [js-event] (println "translate-dispatcher suppressing event" event-id "with dispatcher-arg" dispatcher-arg) nil)))))

(defn handle-outputs [{:keys [outputs] :as db} dispatch-queue-atom output-fn]
  (when (seq outputs)
    (when-not (sequential? outputs)
      (throw (ex-info ":outputs result from a transition function should be a seq"
                      {:outputs outputs
                       :result  db})))
    (let [dispatcher (make-dispatcher dispatch-queue-atom)]
      (doseq [out outputs]
        (output-fn dispatcher (:app-db db) out))))
  (dissoc db :outputs))

(defn init!
  "FIXME: document init!"
  ([state-machine app-db dispatch-queue-atom output-fn]
   (init! state-machine app-db dispatch-queue-atom output-fn :init! nil))
  ([state-machine app-db dispatch-queue-atom output-fn event-id event-arg]
   (-> (start state-machine app-db {:id             event-id
                                    :dispatcher-arg nil
                                    :event-arg      event-arg})
       (handle-outputs dispatch-queue-atom output-fn))))

(defn consume-dispatch-queue
  "FIXME: document consume-dispatch-queue."
  ([state-machine db-atom dispatch-queue-atom output-fn]
   (consume-dispatch-queue state-machine db-atom dispatch-queue-atom output-fn nil))
  ([state-machine db-atom dispatch-queue-atom output-fn ignored-fn]
   (loop []
     (when-let [event (util/dequeue! dispatch-queue-atom)]
       (swap! db-atom
              (fn [db]
                (let [result (transition state-machine db event)]
                  (when (and (= result db) ignored-fn)
                    ;; FIXME pass state-machine and db, for more
                    ;; informative goodness
                    (ignored-fn event))
                  (handle-outputs result dispatch-queue-atom output-fn))))
       (recur)))))

;;;;
;;;; Unit State Machine
;;;;

(s/defschema StateChoiceFn
  (s/make-fn-schema StateId
                    [[(s/one s/Any 'db) (s/one Event 'event)]]))

(s/defschema TransitionFn
  (s/make-fn-schema {:app-db                   s/Any
                     :state-db                 s/Any
                     (s/optional-key :output)  [Output]}
                    [[(s/one s/Any 'db) (s/one Event 'event)]]))

(s/defrecord TransitionSpec [target-states   :- #{StateId}
                             state-choice-fn :- StateChoiceFn])

(s/defrecord Unit [machine-name  :- s/Str
                   start-state   :- TransitionSpec
                   transitions   :- {StateId {EventId TransitionSpec}}
                   transition-fn :- TransitionFn
                   outputs       :- #{s/Keyword}]
  StateMachine
  (machine-name [_]
    machine-name)
  (start [_ app-db event]
    (let [event (if (= ::start (:id event))
                  event
                  (merge {:id ::start, :dispatcher-arg nil, :event-arg nil}
                         (when event {:start-trigger event})))
          db    {:app-db   app-db
                 :state-db {:state ((:state-choice-fn start-state) {:app-db app-db} event)}}]
      (transition-fn db (assoc event :to-state start-state))))
  (transition [this db event]
    (let [orig-state     (:state (:state-db db))
          transition-map (get transitions orig-state)]
      (when-not transition-map
        (throw (ex-info (str "Invalid state machine state: " orig-state)
                        {:machine this, :state orig-state})))
      (if-let [{:keys [state-choice-fn]} (get transition-map (:id event))]
        (let [new-state (state-choice-fn db event)]
          (-> (transition-fn db (assoc event :to-state new-state))
              (assoc-in [:state-db :state] new-state)))
        ;; This state-machine doesn't have a transition for this event.
        db)))
  (current-state [this state-db]
    (:state state-db))
  (get-submachine [this path-component]
    (when (= [] path-component)
      this))
  (get-substate-db [this state-db path-component]
    (when (= [] path-component)
      state-db)))


(s/defn ^:private interpret-transition-spec :- TransitionSpec [spec]
  (cond (keyword? spec)
        (map->TransitionSpec {:target-states   #{spec}
                              :state-choice-fn (constantly spec)})
        (and (vector? spec) (set? (first spec)) (= 2 (count spec)))
        (let [[target-states state-choice-fn] spec]
          (map->TransitionSpec {:target-states   target-states
                                :state-choice-fn state-choice-fn}))
        (map? spec)
        (map->TransitionSpec spec)
        :else
        (throw (ex-info (str "Unable to interpret transition spec" (pr-str spec))
                        {:spec spec}))))


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
  [{:keys [machine-name start transitions transition-fn outputs]}]
  (map->Unit {:machine-name  machine-name
              :start         (interpret-transition-spec start)
              :transitions   (into {}
                                   (for [[from-state tmap] transitions]
                                     [from-state
                                      (into {}
                                            (for [[event-id spec] tmap]
                                              [event-id (interpret-transition-spec spec)]))]))
              :transition-fn transition-fn
              :outputs       (or outputs #{})}))

(defn defmachine* [machine-name machine-spec]
  (unit (assoc machine-spec :machine-name machine-name)))

#?
(:clj
 (defmacro defmachine
   "`def`s a 'unit' state machine with the given transitions. The arguments
   after the name are key-value pairs defining a transitions map (see `unit`).
   The first key-value pair defines the start state for the machine.

   FIXME: use `tools.macro/name-with-attributes` for docstring & metadata
   support."
   [machine-name machine-spec]
   `(def ~machine-name
      (defmachine* ~(str *ns* "/" (name machine-name)) ~machine-spec))))


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
  (start [_ app-db event]
    (let [lensed-db (-get lens app-db)
          result    (start machine lensed-db event)]
      (update-in result [:app-db] (partial -put lens app-db))))
  (transition [_ db event]
    (let [lensed-db (-get lens (:app-db db))
          result    (transition machine (assoc db :app-db lensed-db) event)]
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
  (start [_ app-db event]
    (let [machine-result (start machine app-db event)
          state          (:state (:state-db machine-result))]
      (if-let [{:keys [submachine sublens]} (get submachines state)]
        (let [submachine-result (start submachine
                                       (:app-db (if sublens
                                                  (-get sublens machine-result)
                                                  machine-result))
                                       event)]
          ;; FIXME if sublens results in child's app-db update being put in
          ;; state-db instead, it will be silently dropped
          (-> machine-result
              (assoc-in [:app-db] (:app-db (-put sublens machine-result submachine-result)))
              (assoc-in [:state-db ::submachine-db] (:state-db submachine-result))
              (update :outputs concat (:outputs submachine-result))))
        machine-result)))
  (transition [_ db event]
    (let [machine-db                   (update-in db [:state-db] dissoc ::submachine-db)
          machine-result               (transition machine machine-db event)
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
          (let [submachine-result (transition submachine submachine-db event)]
            ;; FIXME if sublens results in child's app-db update being put in
            ;; state-db instead, it will be silently dropped
            (-> db
                (assoc-in [:state-db ::submachine-db] (:state-db submachine-result))
                (assoc :app-db (:app-db (-put sublens machine-db submachine-result)))
                (update :outputs concat (:outputs submachine-result)))))

        ;; `machine`'s state is unchanged after the transition: give the
        ;; submachine a go as well
        (= (:state (:state-db machine-result))
           (:state (:state-db db)))
        (let [lensed-db     (-get sublens machine-result)
              submachine-db {:app-db   (:app-db lensed-db)
                             :state-db (::submachine-db (:state-db db))}
              submachine-result (transition submachine submachine-db event)]
          ;; FIXME if sublens results in child's app-db update being put in
          ;; state-db instead, it will be silently dropped
          (-> machine-result
              (assoc-in [:state-db ::submachine-db] (:state-db submachine-result))
              (assoc :app-db (:app-db (-put sublens machine-result submachine-result)))
              (update :outputs concat (:outputs submachine-result))))

        ;; `machine`'s state changed: we're entering a new substate
        :else
        (let [submachine-result (start submachine
                                       (:app-db (-get sublens machine-result))
                                       {:id             ::start
                                        :dispatcher-arg nil
                                        :event-arg      nil
                                        :start-trigger  event})]
          ;; FIXME if sublens results in child's app-db update being put in
          ;; state-db instead, it will be silently dropped
          (-> machine-result
              (assoc-in [:state-db ::submachine-db] (:state-db submachine-result))
              (assoc :app-db (:app-db (-put sublens machine-result submachine-result)))
              (update :outputs concat (:outputs submachine-result)))))))
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
  (start [_ app-db event]
    (let [results (reduce (fn [accum [name machine]]
                            (let [result (start machine (:app-db accum) event)]
                              {:app-db    (:app-db result)
                               :state-dbs (assoc (:state-dbs accum) name (:state-db result))
                               :outputs   (concat (:outputs accum) (:outputs result))}))
                          {:app-db app-db, :state-dbs {}, :outputs []}
                          submachines)]
      (merge {:app-db   (:app-db results)
              :state-db {:state           :peer
                         ::submachine-dbs (:state-dbs results)}}
             (when-let [outs (seq (:outputs results))]
               {:outputs outs}))))
  (transition [_ db event]
    (let [results (reduce (fn [accum [name machine]]
                            (let [result (transition machine
                                                     {:app-db   (:app-db accum)
                                                      :state-db (-> db
                                                                    :state-db
                                                                    ::submachine-dbs
                                                                    (get name))}
                                                     event)]
                              {:app-db    (:app-db result)
                               :state-dbs (assoc (:state-dbs accum) name (:state-db result))
                               :outputs   (concat (:outputs accum) (:outputs result))}))
                          {:app-db (:app-db db), :state-dbs {}, :outputs []}
                          submachines)]
      (merge {:app-db   (:app-db results)
              :state-db {:state           :peer
                         ::submachine-dbs (:state-dbs results)}}
             (when-let [outs (seq (:outputs results))]
               {:outputs outs}))))
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
