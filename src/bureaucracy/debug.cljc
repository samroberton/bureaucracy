(ns bureaucracy.debug
  (:require [bureaucracy.core :as bcy #?@(:cljs [:refer [StateMachine]])]
            [bureaucracy.util :as util])
  #?(:clj (:import bureaucracy.core.StateMachine)))


;;;;
;;;; TracingStateMachine, for debugging purposes
;;;;

(defrecord Tracing [name machine]
  StateMachine
  (start [_ db input-event]
    (let [result (bcy/start machine db input-event)]
      (if (= (:app-db result) (:app-db db))
        (util/info "StateMachine " name " (start ...) returned unchanged app-db, "
                   "state-db: " (:state-db result))
        (util/info "StateMachine " name " (start ...) returned: " result))
      result))
  (input [_ db input-event]
    (let [result (bcy/input machine db input-event)]
      (cond
        (not result)
        (util/error "StateMachine" name "(input ...) returned a nil db for input-event-id"
                    (:id input-event))

        (not (:app-db result))
        (util/error "StateMachine" name "(input ...) returned a nil :app-db for input-event-id"
                    (:id input-event))

        (not (:state-db result))
        (util/error "StateMachine" name "(input ...) returned a nil :state-db for input-event-id"
                    (:id input-event))

        (= result db)
        (util/debug "StateMachine" name "(input ...) took no action for input-event-id"
                    (:id input-event))

        (= (:app-db result) (:app-db db))
        (util/debug "StateMachine" name "(input ...) returned unchanged app-db for input-event-id"
                    (:id input-event) "with new state-db:" (:state-db result))

        :else
        (util/debug "StateMachine" name "(input ...) returned a new db for input-event-id"
                    (:id input-event) ":" result))
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
  StateMachine
  (start [_ db input-event]
    (let [result (bcy/start machine db input-event)]
      (when (= result db)
        (util/info (str "Ignored input event: StateMachine " name
                        " (start ...) returned an unchanged DB for event "
                        (pr-str input-event))))
      result))
  (input [_ db input-event]
    (let [result (bcy/input machine db input-event)]
      (when (= result db)
        (util/info (str "Ignored input event: StateMachine " name
                        " (input ...) returned an unchanged DB for event "
                        (pr-str input-event))))
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
