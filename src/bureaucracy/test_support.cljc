(ns bureaucracy.test-support
  (:require [bureaucracy.core :as bcy]
            [bureaucracy.dispatch :as dispatch]
            [bureaucracy.util :as util]
            [bureaucracy.view :as view]))

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

(def ^:dynamic *dispatcher-tracking-atom* nil)

(defn- track-dispatchers [dispatcher]
  (fn tracking-dispatcher
    ([event-id]
     (tracking-dispatcher event-id nil))
    ([event-id dispatcher-arg]
     (when *dispatcher-tracking-atom*
       (swap! *dispatcher-tracking-atom* update event-id #(conj (or % #{}) dispatcher-arg)))
     (dispatcher event-id dispatcher-arg))))

(defn- swap-db-and-process-dispatches!
  "`(swap! db f)`, capturing any dispatches that result from processing
  `:outputs` and executing those dispatches (which might then result in further
  `:outputs` causing further dispatches, etc)."
  [db f]
  (binding [*delayed-dispatches* (atom (util/queue))]
    (swap! db f)
    (loop []
      (when-let [delayed-dispatch (util/dequeue! *delayed-dispatches*)]
        (deref delayed-dispatch)
        (recur)))
    @db))

(defn dispatchable?
  ([{:keys [last-rendered-dispatchers previously-output-dispatchers]} event-id]
   (or (get @last-rendered-dispatchers event-id)
       (get @previously-output-dispatchers event-id)))
  ([{:keys [last-rendered-dispatchers previously-output-dispatchers]} event-id dispatcher-arg]
   (letfn [(contains-dispatcher? [tracking-atom-val]
             (some-> (get tracking-atom-val event-id)
                     (contains? dispatcher-arg)))]
     (or (contains-dispatcher? @last-rendered-dispatchers)
         (contains-dispatcher? @previously-output-dispatchers)))))

(defn- check-input-dispatchable!
  [{:keys [last-rendered-dispatchers previously-output-dispatchers]} event-id dispatcher-arg]
  (let [rendered-dispatcher-args (not-empty (get @last-rendered-dispatchers event-id))
        output-dispatcher-args   (not-empty (get @previously-output-dispatchers event-id))]
    (assert (or rendered-dispatcher-args output-dispatcher-args)
            (format (str "Event ID %s is not currently dispatchable. Dispatchable event IDs from "
                         "the last rendered view are: %s. Dispatchable event IDs from "
                         "previously processed output-handler calls are: %s.")
                    (pr-str event-id)
                    (if-let [coll (not-empty (keys @last-rendered-dispatchers))]
                      (pr-str coll)
                      "<none>")
                    (if-let [coll (not-empty (keys @previously-output-dispatchers))]
                      (pr-str coll)
                      "<none>")))
    (assert (or (contains? rendered-dispatcher-args dispatcher-arg)
                (contains? output-dispatcher-args dispatcher-arg))
            (format (str "Event ID %s is not currently dispatchable with dispatcher-arg %s. "
                         "Available dispatcher args for this event ID from the last rendered view "
                         "are: %s. Available dispatcher args for this event ID from previously "
                         "processed output-handler calls are: %s.")
                    (pr-str event-id)
                    (pr-str dispatcher-arg)
                    (if-let [coll (not-empty rendered-dispatcher-args)]
                      (pr-str coll)
                      "<none>")
                    (if-let [coll (not-empty output-dispatcher-args)]
                      (pr-str coll)
                      "<none>")))))

(defn input!
  ([app event-id]
   (input! app event-id nil nil))
  ([app event-id dispatcher-arg]
   (input! app event-id dispatcher-arg nil))
  ([{:keys [db output-handler state-machine view-tree] :as app} event-id dispatcher-arg event-arg]
   (when (and view-tree output-handler)
     (check-input-dispatchable! app event-id dispatcher-arg))
   (swap-db-and-process-dispatches! db
                                    #(bcy/input state-machine % {:id             event-id
                                                                 :dispatcher-arg dispatcher-arg
                                                                 :event-arg      event-arg}))))

(defn pop-output! [{:keys [db] :as app}]
  (util/dequeue-in! db [:outputs]))

(defn peek-output [{:keys [db] :as app}]
  (first (:outputs @db)))

(defn pause-outputs! [{:keys [db] :as app}]
  (dispatch/remove-auto-handle-outputs db))

(defn unpause-outputs! [{:keys [db dispatcher output-handler previous-outputs] :as app}]
  (dispatch/auto-handle-outputs db output-handler dispatcher)
  ;; Trigger the watch function to handle any outputs currently there.
  (swap-db-and-process-dispatches! db identity))

(defn current-state
  ([{:keys [state-machine] :as app}]
   (current-state app state-machine))
  ([{:keys [db]} machine]
   (let [id (if (satisfies? bcy/StateMachine machine)
              (bcy/machine-id machine)
              machine)]
     (get-in @db [:state-db id :state]))))

(defn state-db
  ([{:keys [state-machine] :as app}]
   (state-db app state-machine))
  ([{:keys [db]} machine]
   (let [id (if (satisfies? bcy/StateMachine machine)
              (bcy/machine-id machine)
              machine)]
     (get-in @db [:state-db id]))))

(defn in-state?
  "Returns true if any state machine is in a state `state`: probably mostly
  useful if you use namespaced keywords as states."
  [{:keys [db state-machine]} state]
  (boolean (some (partial = state) (map (comp :state second) (:state-db @db)))))

(defn- do-render
  [{:keys [dispatcher last-rendered-dispatchers view-tree view-callbacks]} db-val]
  (reset! last-rendered-dispatchers {})
  (binding [*dispatcher-tracking-atom* last-rendered-dispatchers]
    (view/render (merge view-callbacks {:dispatcher dispatcher}) view-tree db-val)))

(defn render [{:keys [view-tree db] :as app}]
  (assert view-tree "You didn't supply a view tree -- we're not rendering anything.")
  (do-render app @db))


(defn make-app
  "Create and start a self-running 'app', for testing purposes.

  When given a `view-tree`, the app will automatically re-render the view every
  time the `db` atom changes (the last rendered view is available from
  `@(:last-rendered-view app)`).

  When given an `output-handler`, the app will automatically handle
  `(:outputs @db)`."
  ([state-machine]
   (make-app state-machine {}))
  ([state-machine {:keys [app-db dispatcher output-handler outputs-paused? start-event view-tree
                          view-callbacks]}]
   (let [db  (atom (bcy/init-db app-db))
         app (merge {:state-machine state-machine
                     :db            db
                     :dispatcher    (track-dispatchers
                                     (or dispatcher
                                         (dispatch/make-immediate-dispatcher state-machine db)))}
                    (when view-tree
                      {:view-tree                 (view/make-view-tree view-tree)
                       :last-rendered-view        (atom nil)
                       :last-rendered-dispatchers (atom {})})
                    (when view-callbacks
                      {:view-callbacks view-callbacks})
                    (when output-handler
                      (let [prev-outputs     (atom [])
                            prev-dispatchers (atom {})]
                        (letfn [(wrapped-output-handler [dispatcher app-db output]
                                  (swap! prev-outputs conj output)
                                  (binding [*dispatcher-tracking-atom* prev-dispatchers]
                                    (output-handler (delayed-dispatcher dispatcher)
                                                    app-db
                                                    output)))]
                          {:output-handler                wrapped-output-handler
                           :previous-outputs              prev-outputs
                           :previously-output-dispatchers prev-dispatchers}))))]
     (when (and output-handler (not outputs-paused?))
       (unpause-outputs! app))
     (when view-tree
       (add-watch db ::render-view (fn [_ _ _ val]
                                     (reset! (:last-rendered-view app) (do-render app val)))))
     (swap-db-and-process-dispatches! db #(bcy/start state-machine % start-event))
     app)))
