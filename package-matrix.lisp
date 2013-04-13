(defpackage #:graph-matrix
  (:use :common-lisp
        :alexandria
        :metabang-bind
        :curry-compose-reader-macros
        :graph
        :fl.function)
  ;; shadow functions defined in alexandria, fl.function, and graph
  (:shadow :copy :factorial :standard-deviation :variance :median :mean :degree)
  (:export :to-adjacency-matrix-new :to-reachability-matrix :make-universal-matrix :make-identity-matrix :to-distance-matrix :reachablep :strong-components))
