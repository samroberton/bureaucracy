(ns bureaucracy.view
  (:require [bureaucracy.core :as bcy]
            [bureaucracy.dispatch :as dispatch]
            [bureaucracy.util :as util]
            [clojure.set :as set]))

;;;;
;;;; Util functions
;;;;

(defn- as-machine-id [machine-or-machine-id]
  (if (satisfies? bcy/StateMachine machine-or-machine-id)
    (bcy/machine-id machine-or-machine-id)
    machine-or-machine-id))

(defn- db-matches-state? [{:keys [state-db] :as db} state-match-spec]
  (letfn [(matches? [[machine rule]]
            (let [id (as-machine-id machine)]
              (assert (symbol? id))
              (bcy/matches-state? rule (get-in state-db [id :state]))))]
    (every? matches? state-match-spec)))

(defn- get-state-dbs [state-dbs-spec state-db]
  (cond (satisfies? bcy/StateMachine state-dbs-spec)
        (get state-db (bcy/machine-id state-dbs-spec))
        (symbol? state-dbs-spec)
        (get state-db state-dbs-spec)
        (map? state-dbs-spec)
        (reduce-kv (fn [result k subspec]
                     (assoc result k (get-state-dbs subspec state-db)))
                   {}
                   state-dbs-spec)
        ;; A vector serves as a tuple of [machine {:keys paths-in-machine's-state-db}]
        (vector? state-dbs-spec)
        (let [machine-db (get state-db (as-machine-id (first state-dbs-spec)))]
          (reduce-kv (fn [result k path]
                       (assoc result k (get-in machine-db path)))
                     {}
                     (second state-dbs-spec)))))

(defn- get-app-db [app-db-spec app-db]
  ;; FIXME implement get-app-db
  app-db)

(defn- get-items [[machine path] state-db]
  (get-in state-db (cons (as-machine-id machine) path)))

(defn get-dbs [{:keys [match-rule app-db-spec state-dbs-spec items-spec]} db]
  (when (db-matches-state? db match-rule)
    {:app-db    (get-app-db app-db-spec (:app-db db))
     :state-dbs (get-state-dbs state-dbs-spec (:state-db db))
     :items     (get-items items-spec (:state-db db))}))


;;;;
;;;; View tree
;;;;

(defn make-view-tree
  ([node]
   (make-view-tree identity node))
  ([node-wrapper node]
   (letfn [(make-node [node]
             (node-wrapper
              (let [node (set/rename-keys node {:state     :match-rule
                                                :view      :view-fn
                                                :name      :view-name
                                                :abb-db    :app-db-spec
                                                :state-dbs :state-dbs-spec
                                                :items     :items-spec})]
                (if (find node :subviews)
                  (update node :subviews #(util/map-vals (partial make-view-tree node-wrapper) %))
                  node))))]
     (doall
      (map make-node (if (sequential? node)
                       node
                       [node]))))))


(defn apply-view-tree [view-tree db]
  (letfn [(do-apply [view-tree]
            (->> view-tree
                 (map (fn [{:keys [view-fn subviews] :as node}]
                        (when-let [{:keys [app-db state-dbs items]} (get-dbs node db)]
                          {:node           node
                           :view-app-db    app-db
                           :view-state-dbs state-dbs
                           :view-items     items
                           :subviews       (reduce-kv (fn [result k subview-tree]
                                                        (assoc result k (do-apply subview-tree)))
                                                      {}
                                                      subviews)})))
                 (some identity)))]
    (do-apply view-tree)))


(defn- invoke-view-fn [callbacks {:keys [view-fn dispatching]} subviews app-db state-dbs]
  (let [subview-fns (when subviews
                      {:render-subview  (fn [subview-id]
                                          (assert (find subviews subview-id)
                                                  (str "No such subview '" subview-id "'."))
                                          (get subviews subview-id))
                       :render-subviews (fn [subview-id]
                                          (assert (find subviews subview-id)
                                                  (str "No such subview '" subview-id "'."))
                                          (get subviews subview-id))})
        callbacks   (-> callbacks
                        (assoc :subviews subviews)
                        (update :dispatcher (fn [dispatcher]
                                              (if dispatching
                                                (dispatch/translate-dispatcher dispatcher
                                                                               dispatching)
                                                dispatcher)))
                        (merge subview-fns))]
    (view-fn callbacks app-db state-dbs)))


(defn render-applied-tree [callbacks {:keys [node view-app-db view-state-dbs view-items subviews]}]
  (let [{:keys [items-spec
                item-key]} node
        rendered-subviews  (when (not-empty subviews)
                             (util/map-vals (fn [subview]
                                              (when subview
                                                (render-applied-tree callbacks subview)))
                                            subviews))]
    (if items-spec
      (doall (map-indexed (fn [index item]
                            (invoke-view-fn callbacks
                                            node
                                            rendered-subviews
                                            view-app-db
                                            (assoc view-state-dbs item-key {:item  item
                                                                            :index index})))
                          view-items))
      (invoke-view-fn callbacks node rendered-subviews view-app-db view-state-dbs))))

(defn render [callbacks view-tree db]
  (assert (:dispatcher callbacks) "The :dispatcher callback is required.")
  (let [applied (apply-view-tree view-tree db)]
    (render-applied-tree callbacks applied)))
