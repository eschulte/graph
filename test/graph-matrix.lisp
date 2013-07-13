;;; test/graph-matrix.lisp --- tests for the graph matrix library

;; Copyright (C) Eric Schulte and Tom Dye 2013

;; Licensed under the Gnu Public License Version 3 or later

;;; Code:
(in-package :graph-matrix-test)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

(defsuite test)
(in-suite test)

(defvar *graph* nil
  "Variable for use in graph tests.")

(defixture basic-graph
  (:setup (setf *graph*
                (populate (make-instance 'graph)
                  :nodes '(a b c d e f)
                  :edges '((a b)
                           (b c)
                           (c d)
                           (d e)
                           (e c)
                           (e f)
                           (f b)))))
  (:teardown (setf *graph* nil)))


;;; Tests
(deftest adjacency-matrix-new-through-matrix-ref ()
  (with-fixture basic-graph
    (let ((m (to-adjacency-matrix-new *graph* (make-instance 'matrix))))
      (is (equal (to-adjacency-matrix *graph*)
                 (graph-matrix::self m))))))
