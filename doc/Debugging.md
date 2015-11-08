# Debugging

One of the benefits of data-driven user interfaces is that you can inspect them.

`bureaucracy` includes a "debug view", which you can opt to render in your user
interface.  The debug view displays as a partially-transparent `div` overlay on
the right side of the UI, showing the current state of your UI's state machine
and view tree.  It's interactive, so that you can poke about in the state
machine and see more or less detail at each level, and it's live, so as you
interact with your application, it will update in sync.

Please note that the debug view is a work-in-progress at the moment, so it's
probably buggy, and it's certainly not as complete as I'd like.

The debug view is, of course, a state machine and a view tree.  You can render
it into a Reagent application, given a `div` with ID `debug-container`, with the
following code:

```
(defonce debug-db
  (reagent/atom (debug-view/init-db)))

(defonce debug-dispatch-queue
  (atom (bcy-util/queue)))

(defn debug-component [debug-view debug-machine db dispatch-queue]
  [bv/render-view debug-view debug-machine @db dispatch-queue])

(defn mount-debug
  [debugged-machine debugged-view-tree debugged-db-atom debugged-dispatch-queue]
  (swap! debug-db assoc :app-db {:machine        debugged-machine
                                 :view-tree      debugged-view-tree
                                 :db             @debugged-db-atom
                                 :dispatch-queue debugged-dispatch-queue})
  (when-not (:state (:state-db @debug-db))
    (swap! debug-db #(bcy/start debug-view/state-machine (:app-db %) debug-dispatch-queue :init! nil nil)))
  (add-watch debugged-db-atom :debug-view (fn [_ _ _ new-val]
                                              (swap! debug-db assoc-in [:app-db :db] new-val)))
  (reagent/render-component [(reagent/create-class
                              {:reagent-render
                               (fn [] [debug-component
                                       debug-view/view-tree
                                       debug-view/state-machine
                                       debug-db
                                       debug-dispatch-queue])})]
                            (.getElementById js/document "debug-container")))

(add-watch debug-dispatch-queue
           :raf-dispatch-queue
           (fn [_ _ old-val new-val]
             (when (> (count new-val) (count old-val))
               (reagent/next-tick
                #(bcy/consume-dispatch-queue debug-view/state-machine
                                             debug-db
                                             debug-dispatch-queue
                                             debug-view/dispatch-ignored)))))
```

You'll need to include
[`bureaucracy-debug.css`](resources/bureaucracy-debug.css) in your page for it
to display like anything other than a dog's breakfast.
