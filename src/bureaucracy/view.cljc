(ns bureaucracy.view
  (:require [bureaucracy.core :as bcy]
            [bureaucracy.dispatch :as dispatch]))

(defrecord ViewItem [view state-dbs dispatching subviews])

(defprotocol ViewRenderer
  (render-view-item [this dispatcher app-db view-item]))

(defn- invoke-view-fn [renderer dispatcher app-db view-item]
  (let [{:keys [view dispatching state-dbs subviews subview-item]} view-item]
    (view {:dispatcher      (if dispatching
                              (dispatch/translate-dispatcher dispatcher dispatching)
                              dispatcher)
           :render-subview  (fn [subview-id]
                              ;; `find` since subview might exist but not
                              ;; currently be rendered.
                              (when-not (find subviews subview-id)
                                (throw (ex-info (str "No such subview '" (name subview-id) "'")
                                                {:subview-id (name subview-id)})))
                              (when-let [subview-item (get subviews subview-id)]
                                (render-view-item renderer dispatcher app-db subview-item)))
           :render-subviews (fn [subview-id state-db-key coll]
                              ;; `find` since subview might exist but not
                              ;; currently be rendered.
                              (when-not (find subviews subview-id)
                                (throw (ex-info (str "No such subview '" (name subview-id) "'")
                                                {:subview-id (name subview-id)})))
                              (when-let [subview-item (get subviews subview-id)]
                                (doall
                                 (map-indexed (fn [idx item]
                                                (render-view-item renderer
                                                                  dispatcher
                                                                  app-db
                                                                  (assoc subview-item
                                                                         :subview-item
                                                                         {:key   state-db-key
                                                                          :index idx
                                                                          :item  item})))
                                              coll))))}
      app-db
      (if subview-item
        (assoc state-dbs (:key subview-item) (dissoc subview-item :key))
        state-dbs))))

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

(defn render-view-tree [renderer state-machine dispatcher view-tree db]
  (letfn [(transform-views [view-items]
            (if (sequential? view-items)
              (first (remove nil? (map transform-views view-items)))
              (transform-view view-items)))
          (transform-view [{:keys [view name dispatching state state-dbs subviews]}]
            (when (db-matches-state? db state)
              (map->ViewItem
               {:view        view
                :state-dbs   (get-state-dbs state-dbs (:state-db db))
                :dispatching dispatching
                :subviews    (not-empty
                              (reduce-kv (fn [result k subview]
                                           (assoc result k (transform-views subview)))
                                         {}
                                         subviews))})))]
    (let [transformed-view-item (transform-views view-tree)]
      (render-view-item renderer dispatcher (:app-db db) transformed-view-item))))

(deftype BasicViewRenderer []
  ViewRenderer
  (render-view-item [this dispatcher app-db view-item]
    (invoke-view-fn this dispatcher app-db view-item)))

#?
(:cljs
 (defn- react-factory [react reactifier]
   (.createFactory react
    (.createClass react
     #js {:displayName
          "bureaucracy"
          :shouldComponentUpdate
          (fn [next-props _]
            (this-as this
              (let [curr-props (.-props this)
                    result     (or (not= (aget curr-props "appDb")
                                         (aget next-props "appDb"))
                                   (not= (aget curr-props "viewItem")
                                         (aget next-props "viewItem")))]
                #_
                (when (:view (.-viewItem curr-props))
                  (.log js/console (str (.-name (:view (aget curr-props "viewItem")))
                                        ": app-db is "
                                        (if (= (aget curr-props "appDb")
                                               (aget next-props "appDb"))
                                          "unchanged" "changed")
                                        ", view-item is "
                                        (if (= (aget curr-props "viewItem")
                                               (aget next-props "viewItem"))
                                          "unchanged" "changed")
                                        ". Returning " result "."
                                        "\ncurr-props .-view-item:"
                                        (pr-str (dissoc (aget curr-props "viewItem") :view))
                                        ".\nnext-props .-view-item:"
                                        (pr-str (dissoc (aget next-props "viewItem") :view)))))
                result)))
          :render
          (fn []
            (this-as this
              (let [start-time   (.getTime (js/Date.))
                    props        (.-props this)
                    renderer     (aget props "renderer")
                    dispatcher   (aget props "dispatcher")
                    app-db       (aget props "appDb")
                    view-item    (aget props "viewItem")]
                (when view-item
                  (let [result (reactifier (invoke-view-fn renderer dispatcher app-db view-item))]
                    #_
                    (.log js/console (str (.-name view-item) " rendered in "
                                          (- (.getTime (js/Date.)) start-time) "ms"))
                    result)))))}))))

#?
(:cljs
 (deftype ReactViewRenderer [factory]
   ViewRenderer
   (render-view-item [this dispatcher app-db view-item]
     (factory #js {"renderer"   this
                   "dispatcher" dispatcher
                   "appDb"      app-db
                   "viewItem"   view-item}))))

#?
(:cljs
 (defn react-view-renderer
   ([react]
    (react-view-renderer react identity))
   ([react reactifier]
    ;; FIXME factory per view function, so we can use the view function as the
    ;; display name.
    (let [factory (react-factory react reactifier)]
      (ReactViewRenderer. factory)))))
