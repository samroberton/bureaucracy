(ns bureaucracy.view
  (:require [bureaucracy.core :refer [current-state get-path make-dispatcher matches-state?]]))


(defn render-view
  "FIXME: document render-view."
  [the-view state-machine {:keys [state-db] :as db} dispatch-queue]
  (if (sequential? the-view)
    (first (remove nil? (map #(render-view % state-machine db dispatch-queue) the-view)))
    (let [{:keys [view state subviews extra-data subview-item]} the-view
          [path state-match-rule] (first state)
          {view-machine  :machine
           view-state-db :state-db} (get-path state-machine state-db path)]
      (when (and view-machine
                 view-state-db
                 ;; FIXME If 'view-machine' is not a StateMachine (if it's a map
                 ;; of {:peer-name StateMachine}), don't try this - it won't
                 ;; work.
                 (matches-state? state-match-rule (current-state view-machine view-state-db)))
        (apply view
               {:dispatcher      (make-dispatcher dispatch-queue)
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
                 (assoc view-state-db (:key subview-item) (dissoc subview-item :key))
                 view-state-db)
               (map (fn [extra-path]
                      (cond (map? extra-path)
                            (let [[match-path match-rule] (first extra-path)]
                              (when-let [{matched-machine  :machine
                                          matched-state-db :state-db}
                                         (get-path state-machine state-db match-path)]
                                (when (matches-state? match-rule (current-state matched-machine matched-state-db))
                                  matched-state-db)))
                            (sequential? extra-path)
                            (:state-db (get-path state-machine state-db extra-path))
                            :else
                            ;; FIXME what error info goes here?
                            (throw (ex-info "WTF is this?"
                                            {:view-machine view-machine
                                             :view-state-db view-state-db
                                             :path path
                                             :view view
                                             :state state
                                             :extra-path extra-path}))))
                    extra-data))))))

(defn with-path-prefix
  "FIXME: document with-path-prefix."
  [prefix the-view]
  (if (vector? the-view)
    (mapv (partial with-path-prefix prefix) the-view)
    (let [{:keys [view state subviews extra-data]} the-view]
      (assoc the-view
             :state      (into {} (for [[path rule] state]
                                    [(concat prefix path) rule]))
             :subviews   (into {} (for [[k v] subviews]
                                    [k (with-path-prefix prefix v)]))
             :extra-data (map (fn [extra-path]
                                (if (map? extra-path)
                                  (let [[path rule] (first extra-path)]
                                    {(concat prefix path) rule})
                                  (concat prefix extra-path)))
                              extra-data)))))
