(ns bureaucracy.view.react
  (:require [bureaucracy.view :as view]))

(defn- should-component-update [next-props props]
  (or (not= (aget props "app_db")
            (aget next-props "app_db"))
      (not= (aget props "state_dbs")
            (aget next-props "state_dbs"))
      (not= (aget props "subviews")
            (aget next-props "subviews"))))

(defn- render [view-fn view-name props]
  (view-fn (aget props "callbacks") (aget props "app_db") (aget props "state_dbs")))

(defn- node->react-factory [react {:keys [view-name view-fn] :as node}]
  ((.-createFactory react)
   ((.-createClass react)
    #js {:displayName           view-name
         :shouldComponentUpdate (fn [next-props _]
                                  (this-as this
                                    (should-component-update next-props (.-props this))))
         :render                (fn []
                                  (this-as this
                                    (render view-fn view-name (.-props this))))})))

(defn wrap-node [react node]
  (let [react-factory (node->react-factory react node)]
    (update node :view-fn (fn [view-fn]
                            (fn [{:keys [subviews] :as callbacks} app-db state-dbs]
                              (let [props #js {"callbacks" callbacks
                                               "app_db"    app-db
                                               "state_dbs" state-dbs
                                               "subviews"  subviews}]
                                (react-factory props nil)))))))

(defn make-react-wrapper [react]
  (partial wrap-node react))
