(ns bureaucracy.debug
  (:require [bureaucracy.core :as bcy]
            [bureaucracy.util :as util]))


;;;;
;;;; TracingStateMachine, for debugging purposes
;;;;

(defrecord Tracing [name machine]
  bcy/StateMachine
  (start [_ db input-event]
    (let [result (bcy/start machine db input-event)]
      (if (= (:app-db result) (:app-db db))
        (util/infof "StateMachine %s (start ...) returned unchanged app-db, state-db: %s"
                    name (pr-str (:state-db result)))
        (util/infof "StateMachine %s (start ...) returned: %s"
                    name (pr-str result)))
      result))
  (input [_ db input-event]
    (let [result (bcy/input machine db input-event)]
      (cond
        (not result)
        (util/errorf "StateMachine %s (input ...) returned a nil db for input-event-id %s."
                     name (pr-str (:id input-event)))

        (not (:app-db result))
        (util/errorf "StateMachine %s (input ...) returned a nil :app-db for input-event-id %s."
                      name (pr-str (:id input-event)))

        (not (:state-db result))
        (util/errorf "StateMachine %s (input ...) returned a nil :state-db for input-event-id %s."
                      name (pr-str (:id input-event)))

        (= result db)
        (util/debugf "StateMachine %s (input ...) took no action for input-event-id %s."
                     name (pr-str (:id input-event)))

        (= (:app-db result) (:app-db db))
        (util/debugf (str "StateMachine %s (input ...) returned unchanged app-db for "
                          "input-event-id %s with new state-db: %s")
                     name (pr-str (:id input-event)) (pr-str (:state-db result)))

        :else
        (util/debugf "StateMachine %s (input ...) returned a new db for input-event-id %s: %s."
                      name (pr-str (:id input-event)) (pr-str result)))
      result))
  (current-state [_ state-db]
    (bcy/current-state machine state-db))
  (get-submachine [_ path-component]
    (bcy/get-submachine machine path-component))
  (get-substate-db [_ state-db path-component]
    (bcy/get-substate-db machine state-db path-component)))

(defn tracing
  "FIXME: tracing docstring."
  [name machine]
  (Tracing. name machine))


(defrecord WarnOnIgnoredInputEvent [name machine]
  bcy/StateMachine
  (start [_ db input-event]
    (let [result (bcy/start machine db input-event)]
      (when (= result db)
        (util/infof (str "Ignored input event: StateMachine %s (start ...) returned an unchanged "
                         "DB for event %s.")
                    name (pr-str input-event)))
      result))
  (input [_ db input-event]
    (let [result (bcy/input machine db input-event)]
      (when (= result db)
        (util/infof (str "Ignored input event: StateMachine %s (input ...) returned an unchanged "
                         "DB for event %s.")
                    name (pr-str input-event)))
      result))
  (current-state [_ state-db]
    (bcy/current-state machine state-db))
  (get-submachine [_ path-component]
    (bcy/get-submachine machine path-component))
  (get-substate-db [_ state-db path-component]
    (bcy/get-substate-db machine state-db path-component)))

(defn warn-on-ignored-input
  ([machine]
   (warn-on-ignored-input (bcy/machine-name machine) machine))
  ([name machine]
   (WarnOnIgnoredInputEvent. name machine)))
