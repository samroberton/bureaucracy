(ns bureaucracy.test-support
  (:require [bureaucracy.core :as bcy]
            [bureaucracy.util :as util]
            [bureaucracy.view :as view]))

(defn make-event
  ([event-id]
   (make-event event-id nil nil))
  ([event-id dispatcher-arg]
   (make-event event-id dispatcher-arg nil))
  ([event-id dispatcher-arg event-arg]
   {:id             event-id
    :dispatcher-arg dispatcher-arg
    :event-arg      event-arg}))

(defn make-system [state-machine app-db]
  (let [db (atom (bcy/start state-machine
                            {:app-db   app-db
                             :state-db {:state ::bcy/start}
                             :outputs  []}
                            nil))]
    {:state-machine  state-machine
     :db             db
     :input!         (fn [& args]
                       (swap! db #(bcy/input state-machine % (apply make-event args))))
     :pop-output!    (fn []
                       (util/dequeue-in! db [:outputs]))
     :current-state  (fn [path]
                       (:state (:state-db (bcy/get-path state-machine (:state-db @db) path))))
     :matches-state? (fn [match-rule]
                       (bcy/matches-state? match-rule state-machine (:state-db @db) path))}))


(defn render [{:keys [db state-machine view-tree]}]
  (view/render-view-tree (bureaucracy.view.BasicViewRenderer.)
                         state-machine
                         dispatcher
                         view-tree
                         @db))
