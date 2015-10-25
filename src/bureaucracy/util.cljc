(ns bureaucracy.util)

(defn queue []
  #?(:clj clojure.lang.PersistentQueue/EMPTY :cljs #queue []))

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
