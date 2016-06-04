(ns bureaucracy.test-support
  (:require [bureaucracy.core :as bcy]
            [bureaucracy.dispatch :as dispatch]
            [bureaucracy.util :as util]
            [bureaucracy.view :as view]))

(defn- make-event
  ([event-id]
   (make-event event-id nil nil))
  ([event-id dispatcher-arg]
   (make-event event-id dispatcher-arg nil))
  ([event-id dispatcher-arg event-arg]
   {:id             event-id
    :dispatcher-arg dispatcher-arg
    :event-arg      event-arg}))

(def ^:dynamic *delayed-dispatches* nil)

(defn- delayed-dispatcher [dispatcher]
  (fn
    ([event-id]
     (let [dispatch-fn (dispatcher event-id)]
       (fn dispatch
         ([]
          (dispatch nil))
         ([event]
          (swap! *delayed-dispatches* conj (delay (dispatch-fn event)))))))
    ([event-id dispatcher-arg]
     (let [dispatch-fn (dispatcher event-id dispatcher-arg)]
       (fn dispatch
         ([]
          (dispatch nil))
         ([event]
          (swap! *delayed-dispatches* conj (delay (dispatch-fn event)))))))))

(defn- swap-db-and-process-dispatches!
  "`(swap! db f)`, capturing any dispatches that result from processing
  `:outputs` and executing those dispatches (which might then result in further
  `:outputs` causing further dispatches, etc)."
  [{:keys [state-machine db]} f]
  (binding [*delayed-dispatches* (atom (util/queue))]
    (swap! db f)
    (loop []
      (when-let [delayed-dispatch (util/dequeue! *delayed-dispatches*)]
        (deref delayed-dispatch)
        (recur)))
    @db))

(defn input! [{:keys [state-machine] :as system} & args]
  (swap-db-and-process-dispatches! system #(bcy/input state-machine % (apply make-event args))))

(defn pop-output! [{:keys [db] :as system}]
  (util/dequeue-in! db [:outputs]))

(defn peek-output [{:keys [db] :as system}]
  (first (:outputs @db)))

(defn pause-outputs! [{:keys [db] :as system}]
  (dispatch/remove-auto-handle-outputs db))

(defn unpause-outputs! [{:keys [db dispatcher output-handler previous-outputs] :as system}]
  (dispatch/auto-handle-outputs db
                                (fn [dispatcher app-db output]
                                  (swap! previous-outputs conj output)
                                  (output-handler (delayed-dispatcher dispatcher) app-db output))
                                dispatcher)
  ;; Trigger the watch function to handle any outputs currently there.
  (swap-db-and-process-dispatches! system identity))

(defn current-state [{:keys [db state-machine]} path]
  (:state (:state-db (bcy/get-path state-machine (:state-db @db) path))))

(defn state-db [{:keys [db state-machine]} path]
  (:state-db (bcy/get-path state-machine (:state-db @db) path)))

(defn matches-state? [{:keys [db state-machine]} match-rule path]
  (bcy/matches-state? match-rule state-machine (:state-db @db) path))

(defn render [{:keys [db dispatcher state-machine view-renderer view-tree]}]
  (assert view-tree "You didn't supply a view tree -- we're not rendering anything.")
  (view/render-view-tree view-renderer state-machine dispatcher view-tree @db))

(defn last-rendered-view [{:keys [last-render]}]
  (assert last-render "You didn't supply a view tree -- we're not rendering anything.")
  @last-render)

(defn make-system
  "Create and start a self-running system, for testing purposes.

  When given a `view-tree`, the system will automatically re-render the view
  every time the `db` atom changes (the last rendered view is available
  from `(last-rendered-view system)`).

  When given an `output-handler`, the system will automatically handle
  `(:outputs @db)`."
  ([state-machine]
   (make-system state-machine {}))
  ([state-machine {:keys [app-db dispatcher output-handler outputs-paused? start-event view-renderer
                          view-tree]}]
   (let [db     (atom (bcy/init-db app-db))
         system (merge {:state-machine state-machine
                        :db            db
                        :dispatcher    (or dispatcher
                                           (dispatch/make-immediate-dispatcher state-machine db))}
                       (when view-tree
                         {:view-renderer (or view-renderer (bureaucracy.view.BasicViewRenderer.))
                          :view-tree     view-tree
                          :last-render   (atom nil)})
                       (when output-handler
                         {:output-handler   output-handler
                          :previous-outputs (atom [])}))]
     (when (and output-handler (not outputs-paused?))
       (unpause-outputs! system))
     (when view-tree
       (add-watch db ::render-view (fn [_ _ _ val]
                                     (reset! (:last-render system)
                                             (view/render-view-tree (:view-renderer system)
                                                                    state-machine
                                                                    (:dispatcher system)
                                                                    view-tree
                                                                    val)))))
     (swap! db #(bcy/start state-machine % start-event))
     system)))
