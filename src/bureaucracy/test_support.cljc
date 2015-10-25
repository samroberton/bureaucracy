(ns bureaucracy.test-support
  (:require [bureaucracy.core :as bcy]
            [bureaucracy.util :as util]
            [bureaucracy.view :as view]))

(defn make-system [state-machine view-tree initial-db-val]
  (let [db (atom initial-db-val)
        q  (atom (util/queue))]
    (swap! db #(bcy/start state-machine (:app-db %) q :init! nil nil))
    {:db             db
     :dispatch-queue q
     :state-machine  state-machine
     :view-tree      view-tree
     :dispatcher     (bcy/make-dispatcher q)}))

(defn render [{:keys [db dispatch-queue state-machine view-tree]}]
  (view/render-view view-tree state-machine @db dispatch-queue))

(defn consume
  "Consumes the current `system`'s dispatch queue."
  [{:keys [db dispatch-queue state-machine]}]
  (bcy/consume-dispatch-queue state-machine db dispatch-queue))
