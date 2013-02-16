(defpackage #:graph-matrix
  (:use :common-lisp
        :alexandria
        :metabang-bind
        :curry-compose-reader-macros
        :graph
        :gsll)
  ;; shadow functions defined in both alexandria and gsll
  (:shadow :copy :factorial :standard-deviation :variance :median :mean)
  (:export ))
