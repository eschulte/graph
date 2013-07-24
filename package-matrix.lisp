(defpackage #:graph-matrix
  (:use :common-lisp
        :alexandria
        :metabang-bind
        :curry-compose-reader-macros
        :graph
        :fl.function)
  ;; shadow functions defined in alexandria, fl.function, and graph
  (:shadow :copy :factorial :standard-deviation :variance :median :mean :degree)
  (:export
   :matrix
   :fast-matrix
   :matrix-ref
   :matrix-n-rows
   :matrix-n-cols
   :matrix-same-size-p
   :matrix-symmetric-p
   :matrix-entries-different-p
   :matrix-copy
   :matrix-transpose
   :make-universal-matrix
   :make-identity-matrix
   :make-zeros-matrix
   :to-adjacency-matrix
   :to-reachability-matrix
   :reachablep
   :reachable-from
   :to-strong-component-matrix
   :strong-component-of
   :to-distance-matrix
   :distance-from-to))
