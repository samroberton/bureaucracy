(ns bureaucracy.debug-view
  (:require [bureaucracy.core :as bcy]
            [bureaucracy.view :as bv]
            [clojure.string :as str]
            #?@(:clj
                [[clojure.pprint :as pprint]
                 [clojure.tools.logging :as log]]
                :cljs
                [[cljs.pprint :as pprint]])))

(defn- take-until
  "Loving stolen from CLJ-1451."
  [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (if (pred (first s))
       (cons (first s) nil)
       (cons (first s) (take-until pred (rest s)))))))

(defn- pprint-str [obj]
  (binding [pprint/*print-right-margin* 80]
    (with-out-str (pprint/pprint obj))))

(defn- show-state-details [machine debuggee-state-db dispatch]
  [:div.bcy-state-details
   "Transitions: "
   (into [:ul.bcy-transitions]
         (map (fn [[event [to-state tx-fn]]]
                [:li [:pre {:title (str "'" event "', "
                                        (if (= to-state (:state debuggee-state-db))
                                          "self-transition"
                                          (str "transitions to '" to-state "'"))
                                        (when tx-fn
                                          (str ", executing function '"
                                               #?(:clj tx-fn :cljs (.-name tx-fn)) "'")))}
                      (str event)]])
              (get (:transitions machine) (:state debuggee-state-db))))
   "Data: "
   [:pre (pprint-str (dissoc debuggee-state-db :state))]])

(defprotocol RenderDebugView
  "Returns `[:li ...]`."
  (render-debug-view [this debuggee-state-db renderer-state-db path next-path-fn dispatcher]))

(extend-protocol RenderDebugView
  bureaucracy.core.Unit
  (render-debug-view [this debuggee-state-db renderer-state-db path _ dispatcher]
    (let [has-data? (not-empty (dissoc debuggee-state-db :state))
          selected? (= path (:selected-path renderer-state-db))]
      [:li
       [:span.bcy-state
        (when selected? {:class "selected"})
        [:pre {:title (str "Path: " (pr-str path))}
         (str (bcy/machine-name this))]
        " = "
        [:pre (str (:state debuggee-state-db))]
        (cond selected?
              [:i.zmdi.zmdi-caret-down-circle]
              has-data?
              [:i.zmdi.zmdi-tag-more {:title    (str (pr-str path) " : The state-db at this path has data")
                                      :on-click (dispatcher ::select-sm-path path)}]
              :else
              [:i.zmdi.zmdi-tag-close {:title    (str (pr-str path) " : The state-db at this path is empty")
                                       :on-click (dispatcher ::select-sm-path path)}])]]))

  bureaucracy.core.LensedStateMachine
  (render-debug-view [this debuggee-state-db renderer-state-db path next-path-fn dispatcher]
    (render-debug-view (:machine this) debuggee-state-db renderer-state-db path next-path-fn dispatcher))

  bureaucracy.core.Submachine
  (render-debug-view [this debuggee-state-db renderer-state-db path next-path-fn dispatcher]
    (into
     (render-debug-view (:machine this)
                        (dissoc debuggee-state-db :bureaucracy.core/submachine-db)
                        renderer-state-db
                        path
                        next-path-fn
                        dispatcher)
     (when-let [submachine (:submachine (get (:submachines this) (:state debuggee-state-db)))]
       (let [next-path (next-path-fn (bcy/current-state (:machine this) debuggee-state-db))]
         (list [:ul.bcy-machine
                (render-debug-view submachine
                                   (:bureaucracy.core/submachine-db debuggee-state-db)
                                   renderer-state-db
                                   next-path
                                   (partial conj next-path)
                                   dispatcher)])))))

  bureaucracy.core.Peer
  (render-debug-view [this debuggee-state-db renderer-state-db path next-path-fn dispatcher]
    [:li.peer (str (bcy/machine-name this))
     (into [:ul.bcy-machine]
           (map (fn [[name submachine]]
                  (let [substate-db (get (:bureaucracy.core/submachine-dbs debuggee-state-db) name)]
                    (render-debug-view submachine
                                       substate-db
                                       renderer-state-db
                                       (next-path-fn name)
                                       (fn [next-path] (conj path {name next-path}))
                                       dispatcher)))
                (:submachines this)))]))

(defn- render-view-tree [{:keys [machine view-tree db-atom]}]
  (let [db @db-atom]
    (into [:ul.bcy-view-tree]
          (letfn [(single-node [name {:keys [state view subviews]}]
                    (let [[path match-rule]
                          (first state)

                          {view-machine :machine view-state-db :state-db}
                          (bcy/get-path machine (:state-db db) path)

                          active?
                          (and view-machine
                               view-state-db
                               (bcy/matches-state? match-rule view-state-db))]
                      [:li {:class (if active? "active" "inactive")}
                       [:span
                        (pr-str name)
                        [:pre {:title (if active?
                                        (pr-str state)
                                        (str "(not " (pr-str state) ")"))}
                         (.-name view)]]
                       (when (and active? (not-empty subviews))
                         (into [:ul]
                               (mapcat (fn [[name subview-or-subview-seq]]
                                         (node name subview-or-subview-seq))
                                       subviews)))]))
                  (node [name item-or-item-seq]
                    (if (sequential? item-or-item-seq)
                      (take-until #(= "active" (:class (second %)))
                                  (map #(single-node name %) item-or-item-seq))
                      (list (single-node name item-or-item-seq))))]
            (node "root" view-tree)))))

(defn render
  [{:keys [dispatcher]}
   {:keys [machine db-atom dispatch-queue] :as renderer-app-db}
   {:keys [state] :as renderer-state-db}]
  (let [debuggee-db @db-atom]
    [:div#bcy-debug
     [:div#bcy-debug-header
      [:h1 "bureaucracy"]
      [:span.bcy-icons
       [:i.zmdi.zmdi-window-minimize {:title    "Minimize debug window"
                                      :on-click (dispatcher ::minimise)}]
       [:i.zmdi.zmdi-close {:title    "Close"
                            :on-click (dispatcher ::close)}]]]
     [:h2 "State machine"]
     [:div.bcy-state-machine
      [:ul.bcy-machine
       (render-debug-view machine
                          (:state-db debuggee-db)
                          renderer-state-db
                          []
                          (partial conj [])
                          dispatcher)]
      (when-let [path (:selected-path renderer-state-db)]
        (let [{submachine :machine
               substate-db :state-db} (bcy/get-path machine (:state-db debuggee-db) path)]
          (show-state-details submachine substate-db dispatcher)))]
     [:h2 "View tree"]
     [:div.bcy-view-tree
      (render-view-tree renderer-app-db)]]))

(defn render-minimised [{:keys [dispatcher]} _ _]
  [:div#bcy-debug.minimised
   [:i.zmdi.zmdi-window-restore {:title    "Restore debug window"
                                 :on-click (dispatcher ::restore)}]
   [:i.zmdi.zmdi-close {:title    "Close debug window"
                        :on-click (dispatcher ::hide)}]])

;;;;
;;;; Debug state machine
;;;;

(defn- select-sm-path [db _ path _]
  (assoc-in db [:state-db :selected-path] path))

(bcy/defmachine state-machine
  :showing   {::hide           :hidden
              ::minimise       :minimised
              ::select-sm-path [:showing select-sm-path]}
  :minimised {::hide     :hidden
              ::restore  :showing}
  :hidden    {::show :showing})

(def view-tree
  [{:state {[] :showing}
    :view  render}
   {:state {[] :minimised}
    :view  render-minimised}])


(defn init-db []
  {:app-db   {:machine        nil
              :view-tree      nil
              :dispatch-queue nil
              :db-atom        nil}
   :state-db {}})

(defn dispatch-ignored [event-id dispatcher-arg event-arg]
  #?(:clj
     (log/debug (str "Dispatch had no effect: {:event-id " (pr-str event-id) ", "
                     ":dispatcher-arg " (pr-str dispatcher-arg) ", "
                     ":event-arg " (pr-str event-arg) "}"))
     :cljs
     (.log js/console (str "Dispatch had no effect: {:event-id " (pr-str event-id) ", "
                           ":dispatcher-arg " (pr-str dispatcher-arg) ", "
                           ":event-arg " (pr-str event-arg) "}"))))
