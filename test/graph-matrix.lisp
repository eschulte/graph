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


;;; Hage and Harary 1991, Figure 4.5, p. 121
(defixture hh-4-5
  (:setup (setf *graph*
                (populate (make-instance 'digraph)
                          :nodes '(1 2 3 4)
                          :edges '((1 2)
                                   (1 3)
                                   (1 4)
                                   (2 3)
                                   (3 1)
                                   (4 3)))))
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

(defixture relation
  (:setup (setf *graph*
                (populate (make-instance 'digraph)
                          :nodes '(a b c d)
                          :edges '((a c)
                                   (b b)
                                   (c c)
                                   (c b)
                                   (c a)
                                   (d a)
                                   (d c)))))
  (:teardown (setf *graph* nil)))

(defixture digraph
  (:setup (setf *graph*
                (populate (make-instance 'digraph)
                          :nodes '(a b c d)
                          :edges '((a c)
                                   (a d)
                                   (c b)
                                   (c a)
                                   (d c)))))
  (:teardown (setf *graph* nil)))

(defixture graph
  (:setup (setf *graph*
                (populate (make-instance 'digraph)
                          :nodes '(a b c d)
                          :edges '((a c)
                                   (a d)
                                   (b c)
                                   (c b)
                                   (c a)
                                   (c d)
                                   (d c)
                                   (d a)))))
  (:teardown (setf *graph* nil)))

(defixture oriented-graph
  (:setup (setf *graph*
                (populate (make-instance 'digraph)
                          :nodes '(a b c d)
                          :edges '((a d)
                                   (b c)
                                   (c a)
                                   (c d)))))
  (:teardown (setf *graph* nil)))

(defixture similarity-relation
  (:setup (setf *graph*
                (populate (make-instance 'digraph)
                          :nodes '(a b c d)
                          :edges '((a a)
                                   (a c)
                                   (a d)
                                   (b b)
                                   (b c)
                                   (c c)
                                   (c a)
                                   (c b)
                                   (c d)
                                   (d d)
                                   (d a)
                                   (d c)))))
  (:teardown (setf *graph* nil)))

(defixture equivalence-relation
  (:setup (setf *graph*
                (populate (make-instance 'digraph)
                          :nodes '(a b c d)
                          :edges '((a a)
                                   (a c)
                                   (a d)
                                   (b b)
                                   (c c)
                                   (c a)
                                   (c d)
                                   (d d)
                                   (d c)
                                   (d a)))))
  (:teardown (setf *graph* nil)))

(defixture partial-order
  (:setup (setf *graph*
                (populate (make-instance 'digraph)
                          :nodes '(a b c d)
                          :edges '((a c)
                                   (a d)
                                   (b c)
                                   (d c)))))
  (:teardown (setf *graph* nil)))

(defixture complete-order
  (:setup (setf *graph*
                (populate (make-instance 'digraph)
                          :nodes '(a b c d)
                          :edges '((a b)
                                   (a c)
                                   (a d)
                                   (b c)
                                   (d b)
                                   (d c)))))
  (:teardown (setf *graph* nil)))

(defixture tournament
  (:setup (setf *graph*
                (populate (make-instance 'digraph)
                          :nodes '(a b c d)
                          :edges '((a c)
                                   (a d)
                                   (b a)
                                   (c b)
                                   (d b)
                                   (d c)))))
  (:teardown (setf *graph* nil)))

(defixture parity-relation
  (:setup (setf *graph*
                (populate (make-instance 'digraph)
                          :nodes '(a b c d)
                          :edges '((a c)
                                   (a d)
                                   (c a)
                                   (c d)
                                   (d a)
                                   (d c)))))
  (:teardown (setf *graph* nil)))

(defixture antiequivalence-relation
  (:setup (setf *graph*
                (populate (make-instance 'digraph)
                          :nodes '(a b c d)
                          :edges '((a d)
                                   (b a)
                                   (c b)
                                   (d c)))))
  (:teardown (setf *graph* nil)))

(defixture antiparity-relation
  (:setup (setf *graph*
                (populate (make-instance 'digraph)
                          :nodes '(a b c d)
                          :edges '((a a)
                                   (a d)
                                   (b b)
                                   (b a)
                                   (c c)
                                   (c b)
                                   (d d)
                                   (d c)))))
  (:teardown (setf *graph* nil)))


;;; Tests

;;; Test simple functions

;;; Test whether matrix entry comparisons work as expected

(deftest matrix-entries-are-not-different ()
  (with-fixture basic-graph
    (let ((m (to-adjacency-matrix *graph* (make-instance 'matrix))))
      (is (not (matrix-entries-different-p m m))))))

(deftest fast-matrix-entries-are-not-different ()
  (with-fixture basic-graph
    (let ((m (to-adjacency-matrix *graph* (make-instance 'fast-matrix))))
      (is (not (matrix-entries-different-p m m))))))

(deftest matrix-entries-are-different ()
  (let ((z (make-zeros-matrix (make-instance 'matrix) 3 3))
        (u (make-universal-matrix (make-instance 'matrix) 3 3)))
    (is (matrix-entries-different-p u z))))

(deftest fast-matrix-entries-are-different ()
  (let ((z (make-zeros-matrix (make-instance 'fast-matrix) 3 3))
        (u (make-universal-matrix (make-instance 'fast-matrix) 3 3)))
    (is (matrix-entries-different-p u z))))

(deftest matrix-entries-are-different-sizes ()
  (let ((z (make-zeros-matrix (make-instance 'matrix) 3 4))
        (u (make-universal-matrix (make-instance 'matrix) 3 3)))
    (is (eql 1 (matrix-entries-different-p u z)))))

(deftest fast-matrix-entries-are-different-sizes ()
  (let ((z (make-zeros-matrix (make-instance 'fast-matrix) 3 4))
        (u (make-universal-matrix (make-instance 'fast-matrix) 3 3)))
    (is (eql 1 (matrix-entries-different-p u z)))))

(deftest lisp-and-fast-matrix-entries-are-not-different ()
  (let ((f (make-zeros-matrix (make-instance 'fast-matrix) 3 3))
        (l (make-zeros-matrix (make-instance 'matrix) 3 3)))
    (is (not (matrix-entries-different-p f l)))))

;;; Tests for comparability of lisp and fast matrix operations

(deftest lisp-vs-fast-difference ()
  (let ((f0 (make-zeros-matrix (make-instance 'fast-matrix) 3 3))
        (f1 (make-universal-matrix (make-instance 'fast-matrix) 3 3))
        (l0 (make-zeros-matrix (make-instance 'matrix) 3 3))
        (l1 (make-universal-matrix (make-instance 'matrix) 3 3)))
    (is (not (matrix-entries-different-p
              (graph-matrix::matrix-difference f0 f1)
              (graph-matrix::matrix-difference l0 l1))))))

(deftest lisp-vs-fast-sum ()
  (let ((f0 (make-zeros-matrix (make-instance 'fast-matrix) 3 3))
        (f1 (make-universal-matrix (make-instance 'fast-matrix) 3 3))
        (l0 (make-zeros-matrix (make-instance 'matrix) 3 3))
        (l1 (make-universal-matrix (make-instance 'matrix) 3 3)))
    (is (not (matrix-entries-different-p (graph-matrix::matrix-sum f0 f1)
                                         (graph-matrix::matrix-sum l0 l1))))))

(deftest lisp-vs-fast-elementwise-product ()
  (let ((f0 (make-zeros-matrix (make-instance 'fast-matrix) 3 3))
        (f1 (make-universal-matrix (make-instance 'fast-matrix) 3 3))
        (l0 (make-zeros-matrix (make-instance 'matrix) 3 3))
        (l1 (make-universal-matrix (make-instance 'matrix) 3 3)))
    (is (not (matrix-entries-different-p
              (graph-matrix::matrix-elementwise-product f0 f1)
              (graph-matrix::matrix-elementwise-product l0 l1))))))

(deftest lisp-vs-fast-sum-boolean ()
  (let ((f0 (make-zeros-matrix (make-instance 'fast-matrix) 3 3))
        (f1 (make-universal-matrix (make-instance 'fast-matrix) 3 3))
        (l0 (make-zeros-matrix (make-instance 'matrix) 3 3))
        (l1 (make-universal-matrix (make-instance 'matrix) 3 3)))
    (is (not (matrix-entries-different-p
              (graph-matrix::matrix-sum f0 f1 :boolean t)
              (graph-matrix::matrix-sum l0 l1 :boolean t))))))

(deftest lisp-vs-fast-copy ()
  (let ((f (make-zeros-matrix (make-instance 'fast-matrix) 3 3))
        (l (make-zeros-matrix (make-instance 'matrix) 3 3)))
    (is (not (matrix-entries-different-p (matrix-copy f) (matrix-copy l))))))


;;; Tests comparing matrix and fast-matrix results

(deftest adjacency-matrix-vs-fast-matrix ()
  (with-fixture basic-graph
    (let ((m (to-adjacency-matrix *graph* (make-instance 'matrix)))
          (f (to-adjacency-matrix *graph* (make-instance 'fast-matrix))))
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
                      :element-type 'fixnum
                      :initial-contents
                      '((0 0 1 1) (1 0 1 0) (0 1 0 0) (1 0 1 0))))
    (with-fixture hh-5-3
      (is (not (matrix-entries-different-p
                (to-adjacency-matrix *graph* (make-instance 'matrix))
                m))))))

(deftest digraph-and-reachability-matrix ()
  (let ((m (make-instance 'matrix)))
    (setf (graph-matrix::self m)
          (make-array '(4 4)
                      :element-type 'fixnum
                      :initial-contents
                      '((1 1 1 1)(0 1 1 1)(0 1 1 1)(0 0 0 1))))
    (with-fixture hh-5-10
      (is (not (matrix-entries-different-p
                (to-reachability-matrix *graph* (make-instance 'matrix))
                m))))))

;;; This is the matrix R2 on p. 126
(deftest digraph-and-reachability-matrix-with-limit-2 ()
  (let ((m (make-instance 'matrix)))
    (setf (graph-matrix::self m)
          (make-array '(4 4)
                      :element-type 'fixnum
                      :initial-contents
                      '((1 1 1 1)(1 1 1 0)(1 1 1 1)(1 0 1 1))))
    (with-fixture hh-4-5
      (is (not (matrix-entries-different-p
                (to-reachability-matrix
                 *graph* (make-instance 'matrix) :limit 2)
                m))))))

(deftest digraph-and-distance-matrix ()
  (let* ((m (make-instance 'matrix))
         (i (graph-matrix::infinite m)))
    (setf (graph-matrix::self m)
          (make-array
           '(5 5)
           :initial-contents
           `((0 1 1 ,i ,i)
             (2 0 1 ,i ,i)
             (1 2 0 ,i ,i)
             (2 3 1 0 ,i)
             (1 2 2 1 0))))
    (with-fixture hh-5-11
      (let ((d (to-distance-matrix *graph* (make-instance 'matrix))))
        (is (not (matrix-entries-different-p d m)))))))

(deftest digraph-and-strong-component-matrix ()
  (let ((m (make-instance 'matrix)))
    (setf (graph-matrix::self m)
          (make-array '(8 8)
                      :element-type 'fixnum
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

(deftest relation-test ()
  (with-fixture relation
    (is (not (relational-structure *graph* (make-instance 'matrix))))))

(deftest digraph-test ()
  (with-fixture digraph
    (is (relational-structure *graph* (make-instance 'matrix)) "digraph")))

(deftest graph-test ()
  (with-fixture graph
    (is (relational-structure *graph* (make-instance 'matrix)) "graph")))

(deftest oriented-graph-test ()
  (with-fixture oriented-graph
    (is (relational-structure *graph* (make-instance 'matrix))
        "oriented graph")))

(deftest similarity-relation-test ()
  (with-fixture similarity-relation
    (is (relational-structure *graph* (make-instance 'matrix))
        "similarity relation")))

(deftest equivalence-relation-test ()
  (with-fixture equivalence-relation
    (is (relational-structure *graph* (make-instance 'matrix))
        "equivalence relation")))

(deftest complete-order-test ()
  (with-fixture complete-order
    (is (relational-structure *graph* (make-instance 'matrix))
        "complete order")))

(deftest tournament-test ()
  (with-fixture tournament
    (is (relational-structure *graph* (make-instance 'matrix))
        "tournament")))

(deftest parity-relation-test ()
  (with-fixture parity-relation
    (is (relational-structure *graph* (make-instance 'matrix))
        "parity relation")))

(deftest antiequivalence-relation-test ()
  (with-fixture antiequivalence-relation
    (is (relational-structure *graph* (make-instance 'matrix))
        "antiequivalence relation")))

(deftest antiparity-relation-test ()
  (with-fixture antiparity-relation
    (is (relational-structure *graph* (make-instance 'matrix))
        "antiparity relation")))
