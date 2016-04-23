(ns bureaucracy.dispatch
  (:require [bureaucracy.core :as bcy]
            [schema.core :as s]))

;;;;
;;;; InputEvent dispatching
;;;;

;; FIXME Introduce a notion of an 'event-arg translator' to handle eg
;; js/Event -> event-arg.

(defn- translate-keycode [keycode]
  (case keycode
    13 :enter
    27 :escape
    keycode))

#?(:cljs
   (defn- translate-dom-input-value [input]
     (case (.-type input)
       "file"     (array-seq (.-files input))
       "checkbox" (.-checked input)
       (.-value input))))

(defn extract-dispatched-value [maybe-js-event]
  #?(:clj
     ;; FIXME if it's a java.io.File, do some equivalent to JS file input?
     maybe-js-event
     :cljs
     (if (and maybe-js-event
              (exists? (aget js/window "Event"))
              (or (instance? (aget js/window "Event") maybe-js-event)
                  (instance? (aget js/window "Event") (aget maybe-js-event "nativeEvent"))))
       (case (.-type maybe-js-event)
         "blur"    (translate-dom-input-value (.-target maybe-js-event))
         "change"  (translate-dom-input-value (.-target maybe-js-event))
         "input"   (translate-dom-input-value (.-target maybe-js-event))
         "keydown" (translate-keycode (.-which maybe-js-event))
         "keyup"   (translate-keycode (.-which maybe-js-event))
         nil)
       maybe-js-event)))

(defn input-event-fn [state-machine db-atom]
  (s/fn [input-event :- bcy/InputEvent]
    (swap! db-atom (s/fn :- bcy/DB
                     [db :- bcy/DB]
                     (bcy/input state-machine db input-event)))))

(defn- make-dispatcher [dispatch-fn]
  (s/fn dispatcher
    ([event-id :- bcy/InputEventId]
     (dispatcher event-id nil))
    ([event-id :- bcy/InputEventId dispatcher-arg]
     (fn dispatch
       ([]
        (dispatch nil))
       ([value]
        (dispatch-fn {:id             event-id
                      :dispatcher-arg dispatcher-arg
                      ;; FIXME perhaps parameterise with a
                      ;; user-supplied function to combine
                      ;; dispatcher-arg and event-arg, also plugging in
                      ;; extract-dispatched-value?  Or maybe
                      ;; dispatcher-arg is always either a keyword, a
                      ;; path, or a function, and keyword/path get you
                      ;; a map?
                      :event-arg      (extract-dispatched-value value)})
        nil)
       ([js-event & args]
        ;; React calls event handlers with the SyntheticEvent as the first arg
        ;; and DOM ID as the second (and sometime js/Event as the third?). We
        ;; don't want the dom-id, but not supplying the two-/three-arg function
        ;; will result in an arity error.
        (dispatch js-event))))))

(defn make-immediate-dispatcher [state-machine db-atom]
  (make-dispatcher (input-event-fn state-machine db-atom)))

(s/defn translate-dispatcher
  [dispatcher event-id-translations-map :- {bcy/InputEventId bcy/InputEventId}]
  (s/fn
    ([event-id :- bcy/InputEventId]
     (if-let [event-id (get event-id-translations-map event-id event-id)]
       (dispatcher event-id)
       (fn [js-event]
         (println "translate-dispatcher suppressing event" event-id)
         nil)))
    ([event-id :- bcy/InputEventId dispatcher-arg]
     (if-let [event-id (get event-id-translations-map event-id event-id)]
       (dispatcher event-id dispatcher-arg)
       (fn [js-event]
         (println "translate-dispatcher suppressing event" event-id
                  "with dispatcher-arg" dispatcher-arg)
         nil)))))

(s/defn handle-outputs [output-handler dispatcher {:keys [app-db outputs] :as db} :- bcy/DB]
  (doseq [out outputs]
    (output-handler dispatcher app-db out)))

(defn auto-handle-outputs [db-atom output-handler dispatcher]
  (add-watch db-atom
             ::output-handler
             (fn [_ _ _ db]
               (when-let [outputs (seq (:outputs db))]
                 (when (compare-and-set! db-atom db (assoc db :outputs []))
                   (handle-outputs output-handler dispatcher db))))))
