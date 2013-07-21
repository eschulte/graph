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

;;; Hage and Harary 1983, Figure 5.2
(defixture hh-5-2
    (:setup (setf *graph*
                  (populate (make-instance 'graph)
                            :nodes '(1 2 3 4)
                            :edges '((1 2)
                                     (1 3)
                                     (1 4)
                                     (2 3)
                                     (3 4)))))
  (:teardown (setf *graph* nil)))

;;; Hage and Harary 1983, Figure 5.3, p. 97
(defixture hh-5-3
    (:setup (setf *graph*
                  (populate (make-instance 'digraph)
                            :nodes '(1 2 3 4)
                            :edges '((1 3)
                                     (1 4)
                                     (2 1)
                                     (2 3)
                                     (3 2)
                                     (4 3)
                                     (4 1)))))
  (:teardown (setf *graph* nil)))

;;; Hage and Harary 1983, Figure 5.10, p. 107
(defixture hh-5-10
    (:setup (setf *graph*
                  (populate (make-instance 'digraph)
                            :nodes '(1 2 3 4)
                            :edges '((1 2)
                                     (2 3)
                                     (2 4)
                                     (3 2)
                                     (3 4)))))
  (:teardown (setf *graph* nil)))

;;; Hage and Harary 1983, Figure 5.11, p. 110
(defixture hh-5-11
    (:setup (setf *graph*
                  (populate (make-instance 'digraph)
                            :nodes '(1 2 3 4 5)
                            :edges '((1 2)
                                     (1 3)
                                     (2 3)
                                     (3 1)
                                     (4 3)
                                     (5 4)
                                     (5 1)))))
  (:teardown (setf *graph* nil)))

;;; Hage and Harary 1983, Figure 4.18
;;; 2' -> 22, 2" -> 222, etc.
(defixture hh-4-18
    (:setup (setf *graph*
                  (populate (make-instance 'digraph)
                            :nodes '(0 1 11 2 22 222 3 33)
                            :edges '((0 1)
                                     (0 11)
                                     (1 2)
                                     (1 22)
                                     (1 222)
                                     (1 3)
                                     (1 33)
                                     (11 2)
                                     (11 22)
                                     (11 222)
                                     (11 3)
                                     (11 33)
                                     (2 22)
                                     (2 222)
                                     (2 3)
                                     (2 33)
                                     (22 2)
                                     (22 222)
                                     (22 3)
                                     (22 33)
                                     (222 2)
                                     (222 22)
                                     (222 3)
                                     (222 33)))))
  (:teardown (setf *graph* nil)))



;;; Tests

;;; Test whether matrix comparison works as expected

(deftest matrix-entries-are-not-different ()
  (with-fixture basic-graph
    (let ((m (to-adjacency-matrix-new *graph* (make-instance 'matrix))))
      (is (not (matrix-entries-different-p m m))))))

(deftest matrix-entries-are-different ()
  (let ((z (make-zeros-matrix (make-instance 'matrix) 3 3))
        (u (make-universal-matrix (make-instance 'matrix) 3 3)))
    (is (matrix-entries-different-p u z))))

(deftest matrix-entries-are-different-sizes ()
  (let ((z (make-zeros-matrix (make-instance 'matrix) 3 4))
        (u (make-universal-matrix (make-instance 'matrix) 3 3)))
    (is (eql 1 (matrix-entries-different-p u z)))))

;;; Tests comparing matrix and fast-matrix results

(deftest adjacency-matrix-vs-fast-matrix ()
  (with-fixture basic-graph
    (let ((m (to-adjacency-matrix-new *graph* (make-instance 'matrix)))
          (f (to-adjacency-matrix-new *graph* (make-instance 'fast-matrix))))
      (is (not (matrix-entries-different-p m f))))))

(deftest reachability-matrix-vs-fast-matrix ()
  (with-fixture basic-graph
    (let ((m (to-reachability-matrix *graph* (make-instance 'matrix)))
          (f (to-reachability-matrix *graph* (make-instance 'fast-matrix))))
      (is (not (matrix-entries-different-p m f))))))

(deftest strong-component-matrix-vs-fast-matrix ()
  (with-fixture basic-graph
    (let ((m (to-reachability-matrix *graph* (make-instance 'matrix)))
          (f (to-reachability-matrix *graph* (make-instance 'fast-matrix))))
      (is (not (matrix-entries-different-p
                (to-strong-component-matrix m)
                (to-strong-component-matrix f)))))))

(deftest distance-matrix-vs-fast-matrix ()
  (with-fixture basic-graph
    (let ((m (to-distance-matrix *graph* (make-instance 'matrix)))
          (f (to-distance-matrix *graph* (make-instance 'fast-matrix))))
      (is (not (matrix-entries-different-p m f))))))

;;; Tests comparing results to Hage and Harary's book

(deftest digraph-and-adjacency-matrix ()
  (let ((m (make-instance 'matrix)))
    (setf (graph-matrix::self m)
          (make-array '(4 4)
                      :initial-contents
                      '((0 0 1 1) (1 0 1 0) (0 1 0 0) (1 0 1 0))))
    (with-fixture hh-5-3
      (is (not (matrix-entries-different-p
                (to-adjacency-matrix-new *graph* (make-instance 'matrix))
                m))))))

(deftest digraph-and-reachability-matrix ()
  (let ((m (make-instance 'matrix)))
    (setf (graph-matrix::self m)
          (make-array '(4 4)
                      :initial-contents
                      '((1 1 1 1)(0 1 1 1)(0 1 1 1)(0 0 0 1))))
    (with-fixture hh-5-10
      (is (not (matrix-entries-different-p
                (to-reachability-matrix *graph* (make-instance 'matrix))
                m))))))

(deftest digraph-and-distance-matrix ()
  (let ((m (make-instance 'matrix)))
    (setf (graph-matrix::self m)
          (make-array
           '(5 5)
           :initial-contents
           '((0 1 1 graph-matrix::infinity graph-matrix::infinity)
             (2 0 1 graph-matrix::infinity graph-matrix::infinity)
             (1 2 0 graph-matrix::infinity graph-matrix::infinity)
             (2 3 1 0 graph-matrix::infinity)
             (1 2 2 1 0))))
    (with-fixture hh-5-11
      (is (not (matrix-entries-different-p
                (to-distance-matrix *graph* (make-instance 'matrix))
                m))))))

(deftest digraph-and-strong-component-matrix ()
  (let ((m (make-instance 'matrix)))
    (setf (graph-matrix::self m)
          (make-array '(8 8)
                      :initial-contents
                      '((1 0 0 0 0 0 0 0)
                        (0 1 0 0 0 0 0 0)
                        (0 0 1 0 0 0 0 0)
                        (0 0 0 1 1 1 0 0)
                        (0 0 0 1 1 1 0 0)
                        (0 0 0 1 1 1 0 0)
                        (0 0 0 0 0 0 1 0)
                        (0 0 0 0 0 0 0 1))))
    (with-fixture hh-4-18
      (is (not (matrix-entries-different-p
                (to-strong-component-matrix
                 (to-reachability-matrix *graph* (make-instance 'matrix)))
                m))))))
