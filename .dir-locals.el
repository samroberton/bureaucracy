;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((clojure-mode
  (eval put-clojure-indent 's/defrecord 2)
  (eval put 's/defrecord 'clojure-backtracking-indent '(4 4 (2)))))
