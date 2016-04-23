(ns bureaucracy.util
  #?(:cljs (:require-macros bureaucracy.util)))

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

(defn dequeue-in!
  "Like `dequeue!`, except that the queue is at `path` in the atom."
  [map-atom path]
  (let [m @map-atom]
    (when-let [queue (seq (get-in m path))]
      (if (compare-and-set! map-atom m (assoc-in m path (pop queue)))
        (peek queue)
        (recur map-atom path)))))

(defn cljs-env?
  "The accepted way of using a macro's `&env` implicit arg to determine whether
  we're expanding to JVM Clojure or ClojureScript.

  Call within a macro as `(cljs-env? &env)`.

  See https://groups.google.com/forum/#!msg/clojurescript/iBY5HaQda4A/w1lAQi9_AwsJ"
  [env]
  (boolean (:ns env)))

#?
(:clj
 (defmacro debug [fmt & args]
   (if (cljs-env? &env)
     `(.log js/console ~fmt ~@args)
     `(clojure.tools.logging/debugf ~fmt ~@args))))

#?
(:clj
 (defmacro info
   [fmt & args]
   (if (cljs-env? &env)
     `(.info js/console ~fmt ~@args)
     `(clojure.tools.logging/infof ~fmt ~@args))))

#?
(:clj
 (defmacro warn [fmt & args]
   (if (cljs-env? &env)
     `(.warn js/console ~fmt ~@args)
     `(clojure.tools.logging/warnf ~fmt ~@args))))

#?
(:clj
 (defmacro error [fmt & args]
   (if (cljs-env? &env)
     `(.error js/console ~fmt ~@args)
     `(clojure.tools.logging/errorf ~fmt ~@args))))
