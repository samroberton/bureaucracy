(ns bureaucracy.view
  (:require [bureaucracy.core :refer [current-state get-path matches-state?]]
            [bureaucracy.dispatch :refer [translate-dispatcher]]))

(defn- get-matching-state-db [state-machine db state-match-map]
  (let [[path state-match-rule]    (first state-match-map)
        {:keys [machine state-db]} (get-path state-machine (:state-db db) path)]
    (when (and machine
               state-db
               ;; FIXME If 'machine' is not a StateMachine (if it's a map of
               ;; {:peer-name StateMachine}), don't try this - it won't work.
               (matches-state? state-match-rule (current-state machine state-db)))
      state-db)))

(defn- get-extra-data [state-machine db extra-data]
  (map (fn [extra-path]
         (cond (map? extra-path)
               (get-matching-state-db state-machine db extra-path)
               (sequential? extra-path)
               (:state-db (get-path state-machine (:state-db db) extra-path))
               :else
               ;; FIXME what error info goes here?
               (throw (ex-info "WTF is this?"
                               {:state-machine state-machine
                                :db db
                                :extra-path extra-path}))))
       extra-data))

(defrecord ViewItem [view state-db extra-data dispatching subviews])

(defprotocol ViewRenderer
  (render-view-item [this dispatcher app-db view-item]))

(defn- invoke-view-fn
  [renderer
   dispatcher
   app-db
   {:keys [view dispatching state-db extra-data subviews subview-item] :as view-item}]
  (apply view
         {:dispatcher      (if dispatching
                             (translate-dispatcher dispatcher dispatching)
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
                               (map-indexed (fn [idx item]
                                              (render-view-item renderer
                                                                dispatcher
                                                                app-db
                                                                (assoc subview-item
                                                                       :subview-item
                                                                       {:key   state-db-key
                                                                        :index idx
                                                                        :item  item})))
                                            coll)))}
         app-db
         (if subview-item
           (assoc state-db (:key subview-item) (dissoc subview-item :key))
           state-db)
         extra-data))

(defn render-view-tree [renderer state-machine dispatcher view-tree db]
  (letfn [(transform-views [view-items]
            (if (sequential? view-items)
              (first (remove nil? (map transform-views view-items)))
              (transform-view view-items)))
          (transform-view [{:keys [view dispatching state extra-data subviews]}]
            (when-let [view-state-db (get-matching-state-db state-machine db state)]
              (map->ViewItem
               {:view        view
                :state-db    view-state-db
                :extra-data  (get-extra-data state-machine db extra-data)
                :dispatching dispatching
                :subviews    (not-empty
                              (reduce-kv (fn [result k sv]
                                           (assoc result k (transform-views sv)))
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
                    (.log js/console (str (.-name (:view view-item)) " rendered in "
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


(defn with-path-prefix
  "FIXME: document with-path-prefix."
  [prefix the-view]
  (if (sequential? the-view)
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
