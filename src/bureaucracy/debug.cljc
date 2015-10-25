(ns bureaucracy.debug
  (:require [bureaucracy.core :refer [current-state get-submachine get-substate-db event start
                                      #?(:cljs StateMachine)]]
            [schema.core :as s])
  #?@(:clj
      [(:require [clojure.tools.logging :as log])
       (:import bureaucracy.core.StateMachine)]))

#?(:cljs
   (def debug (.-log js/console))
   :clj
   (defmacro debug [& args] `(log/debug ~@args)))

#?(:cljs
   (def info (.-log js/console))
   :clj
   (defmacro info [& args] `(log/info ~@args)))

#?(:cljs
   (def error (.-log js/console))
   :clj
   (defmacro error [& args] `(log/error ~@args)))


;;;;
;;;; TracingStateMachine, for debugging purposes
;;;;

(s/defrecord Tracing [name machine :- (s/protocol StateMachine)]
  StateMachine
  (start [_ app-db dispatch-queue event-id dispatcher-arg event-arg]
    (let [result (start machine app-db dispatch-queue event-id dispatcher-arg event-arg)]
      (if (= (:app-db result) app-db)
        (info "StateMachine " name " (start ...) returned unchanged app-db, "
              "state-db: " (:state-db result))
        (info "StateMachine " name " (start ...) returned: " result))
      result))
  (event [_ db dispatch-queue event-id dispatcher-arg event-arg]
    (when-not db
      (throw (ex-info "StateMachine/event is being called with a nil db."
                      {:state-machine-name name, :event-id event-id})))
    (when-not (:app-db db)
      (throw (ex-info "StateMachine/event is being called with a nil app-db."
                      {:state-machine-name name, :event-id event-id, :db db})))
    (let [result (event machine db dispatch-queue event-id dispatcher-arg event-arg)]
      (cond
        (not result)
        (error "StateMachine" name "(event ...) returned a nil db for event-id" event-id)

        (not (:app-db result))
        (error "StateMachine" name "(event ...) returned a nil :app-db for event-id" event-id)

        (not (:state-db result))
        (error "StateMachine" name "(event ...) returned a nil :state-db for event-id"
               event-id)

        (= result db)
        (debug "StateMachine" name "(event ...) took no action for event-id" event-id)

        (= (:app-db result) (:app-db db))
        (debug "StateMachine" name "(event ...) returned unchanged app-db for event-id" event-id
               "with new state-db:" (:state-db result))

        :else
        (debug "StateMachine" name "(event ...) returned a new db for event-id" event-id ":"
               result))
      result))
  (current-state [_ state-db]
    (current-state machine state-db))
  (get-submachine [_ path-component]
    (get-submachine machine path-component))
  (get-substate-db [_ state-db path-component]
    (get-substate-db machine state-db path-component)))

(defn tracing
  "FIXME: tracing docstring."
  [name machine]
  (Tracing. name machine))
