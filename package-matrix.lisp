(defpackage #:graph-matrix
  (:use :common-lisp
        :alexandria
        :metabang-bind
        :curry-compose-reader-macros
        :graph)
  ;; shadow functions defined in both alexandria and gsll
  (:shadow :copy :factorial :standard-deviation :variance :median :mean)
  (:export :to-adjacency-matrix-new :to-reachability-matrix :make-universal-matrix :make-identity-matrix :to-distance-matrix :reachablep :strong-components))
