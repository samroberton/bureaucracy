(ns bureaucracy.core
  #?(:cljs (:require-macros bureaucracy.core))
  (:require [bureaucracy.util :as util]
            [clojure.set :as set]
            [schema.core :as s]))

;;;;
;;;; State Machine protocol definition
;;;;

(s/defschema State
  "State machine states are keywords."
  s/Keyword)

(s/defschema StateMachineId
  "State machines should be identified have a unique symbol."
  s/Symbol)

(s/defschema InputEventId
  "State machine input events (arrows on a statechart) are identified by
  keywords.  They should be namespaced."
  s/Keyword)

(s/defschema InputEvent
  "An input into the state machine (an arrow on a statechart). `:id` is used to
  decide whether a state machine transition applies from this input.
  `:dispatcher-arg` and `:event-arg` are the arguments to the event (the
  difference is that `:dispatcher-arg` is supplied at the time the dispatcher
  was created, and `:event-arg` is supplied at the time the event
  fires. `:to-state` is supplied by the state machine before the event is passed
  to a transition function, to tell the transition function what state we're
  transitioning to. (You won't need `:to-state` very often.)"
  {:id                        InputEventId
   :dispatcher-arg            s/Any
   :event-arg                 s/Any
   (s/optional-key :to-state) State})

(s/defschema OutputId
  "State machine outputs are also identified by keywords.  They should also be
  namespaced."
  s/Keyword)

(s/defschema Output
  "State machines publish `Output` events to interact with the outside world:
  they are a way for your pure, side-effect free transition functions to drive
  side-effects (such as an AJAX calls)."
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
   :state-db                 {StateMachineId {:state State, s/Any s/Any}}
   (s/optional-key :outputs) [Output]})

(defprotocol StateMachine
  "A static (and immutable) specification of the behaviour of a state machine
  (which might in fact be a hierarchy of state machines)."
  (machine-id [this]
    "The state machine's name (as a string) - for descriptive purposes only.")
  (states [this]
    "The set of valid states for this StateMachine.")
  (transitions-for [this state]
    "The set of event IDs which cause transitions for the given `state`. Should
    return an empty set if `state` is not a valid state for this machine.")
  (children [this]
    "Return a seq of child machines (`nil` if there are none).")
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
  (exit [this db]
    "Exit the state machine, returning the new `DB` value."))


;;;;
;;;; Useful functions
;;;;

(defn single-event-arg
  "The distinction between `:dispatcher-arg` and `:event-arg` is often not
  relevant to the transition function: the function wants just one argument, and
  doesn't care whether it was supplied when the dispatcher was created, or when
  the event fired. Use this function to return that single argument."
  [{:keys [dispatcher-arg event-arg]}]
  (if dispatcher-arg
    dispatcher-arg
    event-arg))

(s/defn init-db :- DB
  "Produce a starting db which satisfies the `DB` schema."
  ([]
   (init-db nil))
  ([app-db]
   {:app-db   app-db
    :state-db {}
    :outputs  (util/queue)}))

(defn matches-state?
  "Does `state` match the given match rule?

  A match rule can be:
    - `:*` to match any state,
    - a State keyword, to match only that single state,
    - a set of State keywords to match any of those several states,
    - a function, which, given the state, should return truthy or falsey."
  [match-rule state]
  (cond (= :* match-rule)    (boolean state)
        (= match-rule state) true
        (ifn? match-rule)    (match-rule state)
        :else                (throw (ex-info (str "Unsupported match-rule: " (pr-str match-rule))
                                             {:match-rule match-rule
                                              :state      state}))))

(s/defn valid-event-ids
  "Returns a set of the available input event IDs for `state-machine`, for the
  given `db` value."
  [state-machine db :- DB]
  (letfn [(child-state-db [child-machine]
            (get-in db [:state-db (machine-id child-machine)]))
          (active? [child-machine]
            (not-empty (child-state-db child-machine)))
          (child-transitions [child-machine]
            (transitions-for child-machine (child-state-db child-machine)))]
    (apply set/union (map child-transitions
                          (tree-seq active? children state-machine)))))


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

(defn- invoke-transition-fn [machine-id transition-fn db input-event]
  (let [transition-fn-db (update db :state-db get machine-id)
        result           (transition-fn transition-fn-db input-event)]
    (assoc result :state-db (assoc (:state-db db) machine-id (:state-db result)))))

(defn- find-transition [{:keys [transitions]} state event-id]
  (or (get-in transitions [state event-id])
      ;; `state` could belong to a cluster.
      (some (fn [[state-or-clustered-states tmap]]
              (when (and (set? state-or-clustered-states)
                         (contains? state-or-clustered-states state))
                (get tmap event-id)))
            transitions)))

(s/defrecord Unit [machine-id    :- StateMachineId
                   start-state   :- TransitionSpec
                   transitions   :- {(s/cond-pre State #{State}) {InputEventId TransitionSpec}}
                   transition-fn :- TransitionFn
                   outputs       :- #{s/Keyword}]
  StateMachine
  (machine-id [_]
    machine-id)
  (states [_]
    (apply set/union
           (set (remove set? (keys transitions)))
           (filter set? (keys transitions))))
  (transitions-for [_ state]
    (apply set/union
           (set (keys (get transitions state)))
           (keep (fn [[state-or-clustered-states tmap]]
                   (when (and (set? state-or-clustered-states)
                              (contains? state-or-clustered-states state))
                     tmap))
                 transitions)))
  (children [this]
    nil)
  (start [this db input-event]
    (let [input-event (if (= ::start (:id input-event))
                        input-event
                        {:id ::start, :dispatcher-arg nil, :event-arg input-event})
          choice-db   (update db :state-db get machine-id)
          to-state    ((:state-choice-fn start-state) choice-db input-event)]
      (assert (contains? (states this) to-state)
              (str to-state " is not a valid state for " machine-id))
      (as-> db db
        (assoc-in db [:state-db machine-id :state] ::start)
        (invoke-transition-fn machine-id transition-fn db (assoc input-event :to-state to-state))
        (assoc-in db [:state-db machine-id :state] to-state))))
  (input [this db input-event]
    (let [orig-state (get-in db [:state-db machine-id :state])]
      (assert (contains? (states this) orig-state)
              (str orig-state " is not a valid state for " machine-id))
      (if-let [{:keys [state-choice-fn]} (find-transition this orig-state (:id input-event))]
        (let [choice-db (update db :state-db get machine-id)
              new-state (state-choice-fn choice-db input-event)
              new-state (if (#{::self ::self-internal} new-state)
                          orig-state
                          new-state)]
          (assert (contains? (states this) new-state)
                  (str new-state " is not a valid state for " machine-id))
          (-> (invoke-transition-fn machine-id transition-fn db (assoc input-event
                                                                       :to-state new-state))
              (assoc-in [:state-db machine-id :state] new-state)))
        ;; This state-machine doesn't have a transition from this state for this input-event.
        db)))
  (exit [this db]
    (update db :state-db dissoc machine-id)))


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
  [{:keys [machine-id start transitions transition-fn outputs]}]
  (assert (every? #(= 1 (val %)) (->> (keys transitions)
                                      (filter set?)
                                      (map seq)
                                      (apply concat)
                                      frequencies))
          "State cannot appear in more than one cluster")
  (map->Unit {:machine-id    machine-id
              :start-state   (interpret-transition-spec start)
              :transitions   (util/map-vals (partial util/map-vals interpret-transition-spec)
                                            transitions)
              :transition-fn transition-fn
              :outputs       (or outputs #{})}))

(defn defmachine* [machine-id machine-spec]
  (unit (assoc machine-spec :machine-id machine-id)))

#?
(:clj
 (defmacro defmachine
   "`def`s a 'unit' state machine with the given transitions. See `unit` for
   documentation.

   FIXME: use `tools.macro/name-with-attributes` for docstring & metadata
   support."
   [machine-id machine-spec]
   `(def ~machine-id
      (defmachine* (symbol ~(str *ns*) (str '~machine-id)) ~machine-spec))))


;;;;
;;;; Sub-State Machines (parent machines with each state able to have submachines)
;;;;

(s/defschema StartDispatcherArgFn
  (s/make-fn-schema s/Any [[(s/one s/Any 'state-db) (s/one InputEvent 'event)]]))

(defn- start-submachine [{:keys [start-dispatcher-arg-fn] :as machine} submachine db input-event]
  (->> (if start-dispatcher-arg-fn
         {:id             ::start
          :dispatcher-arg (start-dispatcher-arg-fn (get-in db [:state-db (machine-id machine)])
                                                   input-event)
          :event-arg      input-event}
         input-event)
       (start submachine db)))

(s/defrecord Submachine [machine                 :- (s/protocol StateMachine)
                         start-dispatcher-arg-fn :- StartDispatcherArgFn
                         submachines             :- {State (s/protocol StateMachine)}]
  StateMachine
  (machine-id [_]
    (machine-id machine))
  (states [_]
    (states machine))
  (transitions-for [_ state]
      (transitions-for machine state))
  (children [_]
    (vals submachines))
  (start [this db input-event]
    (let [machine-result (start machine db input-event)
          state          (get-in machine-result [:state-db (machine-id machine) :state])]
      (if-let [submachine (get submachines state)]
        (start-submachine this submachine machine-result input-event)
        machine-result)))
  (input [this db input-event]
    (let [orig-state          (get-in db [:state-db (machine-id machine) :state])
          orig-submachine     (get submachines orig-state)
          submachine-result   (if orig-submachine
                                (input orig-submachine db input-event)
                                db)
          orig-machine-result (input machine submachine-result input-event)
          new-state           (get-in orig-machine-result [:state-db (machine-id machine) :state])
          new-submachine      (get submachines new-state)
          machine-result      (if (and orig-submachine (not= orig-state new-state))
                                (exit orig-submachine orig-machine-result)
                                orig-machine-result)]
      (if (and new-submachine (not= orig-state new-state))
        ;; We've entered a new substate.
        (start-submachine this new-submachine machine-result input-event)
        machine-result)))
  (exit [this db]
    (let [state      (get-in db [:state-db (machine-id machine) :state])
          submachine (get submachines state)]
      (->> (if submachine
             (exit submachine db)
             db)
           (exit machine)))))

(defn with-substates
  "Associates a child machine with one or more of the states of the primary
  StateMachine `machine`.

  `submachines` is a map from `State` to the child machine which is active while
  the parent is in that state.  When the primary machine transitions to a state
  which has a child machine, the child machine's `start` method will be called.

  If a `start-dispatcher-arg-fn` is supplied, then when a submachine is started,
  its start event will contain a `:dispatcher-arg` which is the result of
  invoking that function on the parent's state-db. This allows you to establish
  submachines which are given some contextual information by their parent
  machines, even though the submachine operates in with its own private
  state-db."
  ([machine submachines]
   (with-substates machine nil submachines))
  ([machine start-dispatcher-arg-fn submachines]
   (Submachine. machine start-dispatcher-arg-fn submachines)))


;;;;
;;;; Concurrent State Machines
;;;;

(s/defrecord Concurrent [machine-id   :- StateMachineId
                         submachines  :- [(s/protocol StateMachine)]]
  StateMachine
  (machine-id [_]
    machine-id)
  (states [_]
    (apply set/union (map states submachines)))
  (transitions-for [_ state]
    (apply set/union (map #(transitions-for % state) submachines)))
  (children [_]
    submachines)
  (start [_ db input-event]
    (reduce (fn [db submachine]
              (start submachine db input-event))
            db
            submachines))
  (input [_ db input-event]
    (reduce (fn [db submachine]
              (input submachine db input-event))
            db
            submachines))
  (exit [_ db]
    (reduce (fn [db submachine]
              (exit submachine db))
            db
            submachines)))

(defn concurrent*
  "Create a single StateMachine which operates multiple child state machines
  independently of one another."
  [machine-id machine & machines]
  (Concurrent. machine-id (cons machine machines)))

#?
(:clj
 (defmacro concurrent
   [machine-id & machines]
   `(concurrent* (symbol ~(str *ns*) (str '~machine-id)) ~@machines)))

#?
(:clj
 (defmacro defconcurrent
   [machine-id & machines]
   `(def ~machine-id
      (concurrent ~machine-id ~@machines))))
