(ns bureaucracy.view
  (:require [bureaucracy.core :refer [get-state make-dispatch-fn]]))


(defn render-view
  "FIXME: document render-view."
  [the-view state-machine {:keys [state-db] :as db} dispatch-queue]
  (if (vector? the-view)
    (first (remove nil? (map #(render-view % state-machine db dispatch-queue) the-view)))
    (let [{:keys [view state subviews extra-data subview-item]} the-view]
      (when-let [view-state (get-state state-machine state-db state)]
        ;; FIXME dispatch-path
        (let [dispatch-path []]
          (apply view
                 {:dispatch        (fn [event-id & event-args]
                                     (apply (make-dispatch-fn dispatch-queue dispatch-path) event-id event-args)
                                     nil)
                  :render-subview  (fn [subview-id]
                                     (if-let [subview (get subviews subview-id)]
                                       (render-view subview state-machine db dispatch-queue)
                                       (throw (ex-info "No such subview" {:subview-id subview-id}))))
                  :render-subviews (fn [subview-id state-db-key coll]
                                     (if-let [subview (get subviews subview-id)]
                                       (map-indexed (fn [idx item]
                                                      (render-view (assoc subview
                                                                          :subview-item
                                                                          {:key state-db-key, :index idx, :item item})
                                                                   state-machine
                                                                   db
                                                                   dispatch-queue))
                                                    coll)
                                       (throw (ex-info "No such subview" {:subview-id subview-id}))))}
                 (:app-db db)
                 (if subview-item
                   (assoc view-state (:key subview-item) (dissoc subview-item :key))
                   view-state)
                 (map (fn [extra-path]
                        (get-state state-machine state-db extra-path))
                      extra-data)))))))

(defn with-path-prefix
  "FIXME: document with-path-prefix."
  [prefix the-view]
  (if (vector? the-view)
    (mapv (partial with-path-prefix prefix) the-view)
    (let [{:keys [view state subviews extra-data]} the-view]
      (assoc the-view
             :state      (concat prefix state)
             :subviews   (into {} (for [[k v] subviews]
                                    [k (with-path-prefix prefix v)]))
             :extra-data (map (partial concat prefix) extra-data)))))
