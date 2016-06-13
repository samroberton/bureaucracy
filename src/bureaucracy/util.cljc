(ns bureaucracy.util
  (:require [schema.core :as s]))

(defn queue []
  #?(:clj clojure.lang.PersistentQueue/EMPTY :cljs #queue []))

(defn queue? [q]
  #?(:clj  (instance? clojure.lang.PersistentQueue q)
     :cljs (instance? cljs.core.PersistentQueue q)))

(s/defschema Queue
  (s/pred queue?))

(defn dequeue!
  "Dequeue an item from a persistent queue which is stored as the value of the
  given atom, setting the atom to the new queue (minus dequeued item) and
  returning the dequeued item. If the queue is empty, does not alter it and
  returns nil."
  [queue-atom]
  (let [queue @queue-atom]
    (when (seq queue)
      (if (compare-and-set! queue-atom queue (pop queue))
        (peek queue)
        (recur queue-atom)))))

(defn dequeue-in!
  "Like `dequeue!`, except that the queue is at `path` in the atom."
  [map-atom path]
  (let [m @map-atom]
    (when-let [queue (not-empty (get-in m path))]
      (if (compare-and-set! map-atom m (assoc-in m path (pop queue)))
        (peek queue)
        (recur map-atom path)))))
