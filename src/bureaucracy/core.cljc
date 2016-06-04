(ns bureaucracy.core
  #?(:cljs (:require-macros bureaucracy.core))
  (:require [bureaucracy.util :as util]
            [schema.core :as s]))

;;;;
;;;; State Machine protocol definition
;;;;

(s/defschema State
  "State machine states are (non-namespaced) keywords."
  s/Keyword)

(s/defschema InputEventId
  "State machine input events are identified by keywords.  When an input event
  is dispatched by your application code, it is passed to every (active) machine
  in your state machine hierarchy, so to avoid accidental collisions between
  event names for different state machines, you may find it useful to use
  namespaced keywords (eg `::bar` or `:foo/bar`, rather than `:bar`)."
  s/Keyword)

(s/defschema InputEvent
  "An input into the state machine. `:id` is used to decide whether a state
  machine transition applies from this input. `:dispatcher-arg` and `:event-arg`
  are the arguments to the event (the difference is that `:dispatcher-arg` is
  supplied at the time the dispatcher was created, and `:event-arg` is supplied
  at the time the event fires. `:to-state` is supplied by the state machine
  before the event is passed to a transition function, to tell the transition
  function what state we're transitioning to. (You won't need `:to-state` very
  often.)"
  {:id InputEventId
   :dispatcher-arg            s/Any
   :event-arg                 s/Any
   (s/optional-key :to-state) State})

(defn single-event-arg
  "The distinction between `:dispatcher-arg` and `:event-arg` is often not
  relevant to the transition function: the function wants just one argument, and
  doesn't care whether it was supplied when the dispatcher was created, or when
  the event fired. Use this function to return that single argument."
  [{:keys [dispatcher-arg event-arg]}]
  (if dispatcher-arg
    dispatcher-arg
    event-arg))

(s/defschema OutputId
  "State machine outputs are also identified by keywords."
  s/Keyword)

(s/defschema Output
  "State machines publish `Output` events to interact with the outside world:
  they are a way for your pure, side-effect free transition functions to inform
  your application when a side-effect (such as an AJAX call) should occur."
  {:id   OutputId
   s/Any s/Any})

(s/defschema DB
  "A DB map contains all state machine state. `:app-db` contains state which is
  considered 'global' to the application, whereas `:state-db` contains state
  which is 'local' or 'private' to the state machine. When you have a state
  machine hierarchy, each state machine in the hierarchy will be given a `DB`
  with that machine's own `:state-db` -- it does not see other machines' 'local'
  state."
  {:app-db                   s/Any
   :state-db                 {:state State, s/Any s/Any}
   (s/optional-key :outputs) [Output]})

(s/defn init-db :- DB
  "Produce a starting db which satisfies the `DB` schema."
  ([]
   (init-db nil))
  ([app-db]
   {:app-db   app-db
    :state-db {:state ::start}
    :outputs  (util/queue)}))

(defprotocol StateMachine
  "A static (and immutable) specification of the behaviour of a state machine
  (which might in fact be a hierarchy of state machines)."
  (machine-name [this]
    "The state machine's name (as a string) - for descriptive purposes only.")
  (start [this db input-event]
    "Return the `DB` value to start the state machine, having constructed
    whatever initial `:state-db` is required, and including whatever `:outputs`
    should occur when the machine is started.

    `start` will invoke the transition function in the same was as `input` does,
    however the `input-event` will be an event with an `:id` of
    `:bureaucracy.core/start` and an `:event-arg` which is this `input-event`
    passed to this method.  If this is a top-level state machine, that's not
    very important, but for state machines in a hierarchy, `start` could be
    called as a result of a parent state machine transitioning. In that case,
    this rule allows the child state machine to know what event has caused it to
    be started.")
  (input [this db input-event]
    "Process the `input-event`, invoking any appropriate transition functions,
    and return the new `DB` value.")
  (current-state [this state-db]
    "Get the current `State` of this state machine from the `state-db`.")
  (get-submachine [this path-component]
    "If this is a state machine hierarchy, get the machine lower in the
    hierarchy at path `path-component`. An empty `path-component` should return
    this machine.")
  (get-substate-db [this state-db path-component]
    "If this is a state machine hierarchy, get the `state-db` of the machine
    lower in the hierarchy at path `path-component`. An empty `path-component`
    should return this machine's `state-db`."))


;;;;
;;;; Functions on hierarchical state machines
;;;;

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
                        (throw (ex-info (str "Invalid path: "
                                             (pr-str path-component))
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
                                               (throw (ex-info (str "Invalid path: "
                                                                    (pr-str path-component))
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
  "Does the state match the given match rule?

  The state can be given directly as `state`, or indirectly, as the current
  state of the machine located at path `path` in the state machine hierarchy
  `machine`, which has `state-db` as its current state DB.

  A match rule can be:
    - `:*` to match any state,
    - a State keyword, to match only that single state,
    - a set of State keywords to match any of those several states,
    - a function, which, given the state, should return truthy or falsey
    - a map, in which case the `state` must be a map of the same shape, and
      `matches-state?` will be called recursively on its contents."
  ([match-rule state]
   (cond (= :* match-rule)    (boolean state)
         (= match-rule state) true
         (map? match-rule)    (every? (fn [[k r]] (matches-state? r (get state k))) match-rule)
         (ifn? match-rule)    (match-rule state)
         :else                (throw (ex-info (str "Unsupported match-rule: " (pr-str match-rule))
                                              {:match-rule match-rule
                                               :state      state}))))
  ([match-rule machine state-db path]
   (let [{:keys [machine state-db]} (get-path machine state-db path)]
     (when (and machine state-db)
       (matches-state? match-rule (current-state machine state-db))))))


;;;;
;;;; Unit State Machine
;;;;

(s/defschema StateChoiceFn
  (s/make-fn-schema State [[(s/one s/Any 'db) (s/one InputEvent 'event)]]))

(s/defschema TransitionFn
  (s/make-fn-schema DB [[(s/one s/Any 'db) (s/one InputEvent 'event)]]))

(s/defschema TransitionSpec
  {:target-states   #{State}
   :state-choice-fn StateChoiceFn})

(s/defrecord Unit [machine-name  :- s/Str
                   start-state   :- TransitionSpec
                   transitions   :- {State {InputEventId TransitionSpec}}
                   transition-fn :- TransitionFn
                   outputs       :- #{s/Keyword}]
  StateMachine
  (machine-name [_]
    machine-name)
  (start [_ db input-event]
    (let [input-event (if (= ::start (:id input-event))
                        input-event
                        {:id ::start, :dispatcher-arg nil, :event-arg input-event})
          to-state ((:state-choice-fn start-state) db input-event)]
      (-> db
          (assoc-in [:state-db :state] to-state)
          (transition-fn (assoc input-event :to-state to-state)))))
  (input [this db input-event]
    (let [orig-state     (:state (:state-db db))
          transition-map (get transitions orig-state)]
      (when-not transition-map
        (throw (ex-info (str "Invalid state machine state: " orig-state)
                        {:machine this, :state orig-state})))
      (if-let [{:keys [state-choice-fn]} (get transition-map (:id input-event))]
        (let [new-state (state-choice-fn db input-event)]
          (-> (transition-fn db (assoc input-event :to-state new-state))
              (assoc-in [:state-db :state] new-state)))
        ;; This state-machine doesn't have a transition for this input-event.
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
        {:target-states   #{spec}
         :state-choice-fn (constantly spec)}
        (and (vector? spec) (set? (first spec)) (= 2 (count spec)))
        (let [[target-states state-choice-fn] spec]
          {:target-states   target-states
           :state-choice-fn state-choice-fn})
        (map? spec)
        spec
        :else
        (throw (ex-info (str "Unable to interpret transition spec: " (pr-str spec))
                        {:spec spec}))))


(s/defn unit :- (s/protocol StateMachine)
  "Create a basic state machine defined by the given transitions.

  `start` is either a keyword defining the machine's start state, or (set of
  States, choice function) pair, such as:
      [#{:state-A :state-B} (fn [db input-event]
                              (if (some-condition? input-event)
                                :state-A
                                :state-B))]

  `transitions` is a map from `State` to a map of transitions which can occur in
  that state. Each key/value pair in the transition map for a given state
  defines the `InputEventId` which causes the transition, and the transition's
  result `State` (which may be the same as its start state -- ie explicit
  self-transitions are allowed).  Like for the definition of `start`, the
  transition's result state can be defined as a (set of States, choice function)
  pair.

  `transition-fn` will be called whenever a transition in the map occurs.

  `outputs` is a set of `OutputId`s that the transition function may produce."
  [{:keys [machine-name start transitions transition-fn outputs]}]
  (map->Unit {:machine-name  machine-name
              :start-state   (interpret-transition-spec start)
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
  (start [_ db input-event]
    (let [lensed-app-db (-get lens (:app-db db))
          result        (start machine (assoc db :app-db lensed-app-db) input-event)]
      (update result :app-db (partial -put lens (:app-db db)))))
  (input [_ db input-event]
    (let [lensed-app-db (-get lens (:app-db db))
          result        (input machine (assoc db :app-db lensed-app-db) input-event)]
      (update result :app-db (partial -put lens (:app-db db)))))
  (current-state [_ state-db]
    (current-state machine state-db))
  (get-submachine [_ path-component]
    (get-submachine machine path-component))
  (get-substate-db [_ state-db path-component]
    (get-substate-db machine state-db path-component)))

(defn with-lens
  "Wrap another StateMachine, such that calls to `start` and `input` apply the
  given `lens` to the DB's `:app-db` value.  This allows us to write re-useable
  state machines which expect certain data to be available in a certain format
  in the `:app-db`, and to plug them into state machine hierarchies that in fact
  don't structure the data that way.  We simply wrap our re-useable machines
  with a lens which conforms to our machine's expectation."
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
  (start [_ db input-event]
    (let [machine-result (start machine db input-event)
          state          (:state (:state-db machine-result))]
      (if-let [{:keys [submachine sublens]} (get submachines state)]
        (let [submachine-result (start submachine
                                       (assoc-in (-get sublens machine-result)
                                                 [:state-db :state] ::start)
                                       input-event)]
          (-> machine-result
              (assoc :app-db (:app-db (-put sublens machine-result submachine-result)))
              (assoc-in [:state-db ::submachine-db] (:state-db submachine-result))
              (update :outputs concat (:outputs submachine-result))))
        machine-result)))
  (input [_ db input-event]
    (let [machine-db                   (update db :state-db dissoc ::submachine-db)
          machine-result               (input machine machine-db input-event)
          state                        (:state (:state-db machine-result))
          {:keys [submachine sublens]} (get submachines state)]
      (cond
        ;; There is no submachine for this state: no more to do.
        (not submachine)
        machine-result

        ;; `machine`'s state is unchanged after the transition: give the
        ;; submachine a go as well.
        (= (:state (:state-db machine-result))
           (:state (:state-db db)))
        (let [lensed-db         (-get sublens machine-result)
              submachine-db     {:app-db   (:app-db lensed-db)
                                 :state-db (::submachine-db (:state-db db))
                                 :outputs  (util/queue)}
              submachine-result (input submachine submachine-db input-event)]
          (-> machine-result
              (assoc :app-db (:app-db (-put sublens machine-result submachine-result)))
              (assoc-in [:state-db ::submachine-db] (:state-db submachine-result))
              (update :outputs concat (:outputs submachine-result))))

        ;; `machine`'s state changed: we're entering a new substate.
        :else
        (let [lensed-db         (-get sublens machine-result)
              submachine-db     {:app-db   (:app-db lensed-db)
                                 :state-db {}
                                 :outputs  (util/queue)}
              start-event       (if (= ::start (:id input-event))
                                  input-event
                                  {:id ::start, :dispatcher-arg nil, :event-arg input-event})
              submachine-result (start submachine submachine-db start-event)]
          (-> machine-result
              (assoc :app-db (:app-db (-put sublens machine-result submachine-result)))
              (assoc-in [:state-db ::submachine-db] (:state-db submachine-result))
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
  "Associates a child machine with one or more of the states of the primary
  StateMachine `machine`.

  `submachines` is a map from `State` to the child machine which is active while
  the parent is in that state.  When the primary machine transitions to a state
  which has a child machine, the child machine's `start` method will be called,
  with an `input-event` with `:id` `:bureaucracy.core/start`, and an
  `:event-arg` which is the event which has caused the primary machine to
  transition.

  Each child machine can be specified either as a bare `StateMachine`, or as
  `{:submachine <machine>, :sublens <lens>}`, to additionally specify a lens to
  apply to the DB when invoking the submachine.  Note that the lens here is used
  slightly differently than in a StateMachine wrapped by `with-lens`: here, the
  lens is applied to the whole DB, in order to allow that the parent's
  `:state-db` to be lensed into the child's `:app-db`."
  [machine submachines]
  (Submachine. machine (into {} (for [[k sm] submachines]
                                  [k (if (satisfies? StateMachine sm)
                                       {:submachine sm, :sublens identity-lens}
                                       sm)]))))


;;;;
;;;; Peer State Machines
;;;;

(defn- invoke-peer-input-fn [submachines invoke-fn db input-event]
  (let [results (reduce (fn [accum [name machine]]
                          (let [result (invoke-fn machine
                                                  (assoc (dissoc accum :state-dbs)
                                                         :state-db (-> (:state-db db)
                                                                       ::submachine-dbs
                                                                       (get name)))
                                                  input-event)]
                            (assoc (dissoc result :state-db)
                                   :state-dbs (assoc (:state-dbs accum)
                                                     name (:state-db result)))))
                        (assoc (dissoc db :state-db) :state-dbs {})
                        submachines)]
    (assoc results :state-db {:state :peer, ::submachine-dbs (:state-dbs results)})))

(s/defrecord Peer [machine-name :- s/Any
                   submachines  :- [[(s/one s/Any 'name)
                                     (s/one (s/protocol StateMachine) 'machine)]]]
  StateMachine
  (machine-name [_]
    machine-name)
  (start [_ db input-event]
    (let [db (assoc-in db
                       [:state-db ::submachine-dbs]
                       (apply merge (for [[k _] submachines]
                                      {k {:state ::start}})))]
       (invoke-peer-input-fn submachines start db input-event)))
  (input [_ db input-event]
    (invoke-peer-input-fn submachines input db input-event))
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
  "Create a single StateMachine which operates multiple child state machines
  independently of one another."
  [machine-name & names-and-machines]
  (Peer. machine-name (partition 2 names-and-machines)))
