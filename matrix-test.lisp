;;; test/graph-matrix.lisp --- tests for the graph matrix library

;; Copyright (C) Eric Schulte and Tom Dye 2013

;; Licensed under the Gnu Public License Version 3 or later

;;; Code:
(defpackage #:graph/matrix-test
  (:use :common-lisp
        :alexandria
        :metabang-bind
        :graph
        :graph/matrix
        :stefil
        :named-readtables
        :curry-compose-reader-macros)
  (:export :test))
(in-package :graph/matrix-test)
(in-readtable :curry-compose-reader-macros)

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

;;; Structural Models in Anthropology, Hage and Harary 1983, Figure
;;; 5.2, p. 96

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

;;; Structural Models in Anthropology, Hage and Harary 1983, Figure
;;; 5.3, p. 97

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

;;; Structural Models in Anthropology, Hage and Harary 1983, Figure
;;; 5.10, p. 107

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


;;; Exchange in Oceania: A Graph Theoretic Analysis, Hage and Harary
;;; 1991, Figure 4.5, p. 121

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

;;; Structural Models in Anthropology, Hage and Harary 1983, Figure
;;; 5.11, p. 110

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

;;; Structural Models in Anthropology, Hage and Harary 1983, Figure
;;; 4.18, p. 86. 2' -> 22, 2" -> 222, etc.

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

(deftest same-size-p ()
 (let ((f1 (make-zeros-matrix (make-instance 'fast-matrix) 2 4))
       (l1 (make-zeros-matrix (make-instance 'matrix) 2 4))
       (f2 (make-zeros-matrix (make-instance 'fast-matrix) 4 2))
       (l2 (make-zeros-matrix (make-instance 'matrix) 4 2))
       (l3 (make-zeros-matrix (make-instance 'matrix) 2 4))
       (f3 (make-zeros-matrix (make-instance 'fast-matrix) 2 4)))
   (is (not (matrix-same-size-p f1 f2)))
   (is (not (matrix-same-size-p l1 l2)))
   (is (not (matrix-same-size-p f1 l2)))
   (is (matrix-same-size-p f1 f3))
   (is (matrix-same-size-p l1 l3))
   (is (matrix-same-size-p l1 f1))
   (is (matrix-same-size-p f1 l3))))

(deftest symmetric-p ()
  (with-fixture hh-5-10
    (let ((m1 (make-identity-matrix (make-instance 'matrix) 3))
          (f1 (make-identity-matrix (make-instance 'fast-matrix) 3))
          (m2 (to-adjacency-matrix *graph* (make-instance 'matrix)))
          (f2 (to-adjacency-matrix *graph* (make-instance 'fast-matrix))))
      (is (matrix-symmetric-p m1))
      (is (matrix-symmetric-p f1))
      (is (not (matrix-symmetric-p m2)))
      (is (not (matrix-symmetric-p f2))))))


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
    (is (= 1 (matrix-entries-different-p u z)))))

(deftest fast-matrix-entries-are-different-sizes ()
  (let ((z (make-zeros-matrix (make-instance 'fast-matrix) 3 4))
        (u (make-universal-matrix (make-instance 'fast-matrix) 3 3)))
    (is (= 1 (matrix-entries-different-p u z)))))

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
              (graph/matrix::matrix-difference f0 f1)
              (graph/matrix::matrix-difference l0 l1))))))

(deftest lisp-vs-fast-sum ()
  (let ((f0 (make-zeros-matrix (make-instance 'fast-matrix) 3 3))
        (f1 (make-universal-matrix (make-instance 'fast-matrix) 3 3))
        (l0 (make-zeros-matrix (make-instance 'matrix) 3 3))
        (l1 (make-universal-matrix (make-instance 'matrix) 3 3)))
    (is (not (matrix-entries-different-p (graph/matrix::matrix-sum f0 f1)
                                         (graph/matrix::matrix-sum l0 l1))))))

(deftest lisp-vs-fast-elementwise-product ()
  (let ((f0 (make-zeros-matrix (make-instance 'fast-matrix) 3 3))
        (f1 (make-universal-matrix (make-instance 'fast-matrix) 3 3))
        (l0 (make-zeros-matrix (make-instance 'matrix) 3 3))
        (l1 (make-universal-matrix (make-instance 'matrix) 3 3)))
    (is (not (matrix-entries-different-p
              (graph/matrix::matrix-elementwise-product f0 f1)
              (graph/matrix::matrix-elementwise-product l0 l1))))))

(deftest lisp-vs-fast-sum-boolean ()
  (let ((f0 (make-zeros-matrix (make-instance 'fast-matrix) 3 3))
        (f1 (make-universal-matrix (make-instance 'fast-matrix) 3 3))
        (l0 (make-zeros-matrix (make-instance 'matrix) 3 3))
        (l1 (make-universal-matrix (make-instance 'matrix) 3 3)))
    (is (not (matrix-entries-different-p
              (graph/matrix::matrix-sum f0 f1 :boolean t)
              (graph/matrix::matrix-sum l0 l1 :boolean t))))))

(deftest lisp-vs-fast-product ()
  (let ((f0 (make-zeros-matrix (make-instance 'fast-matrix) 3 3))
        (f1 (make-universal-matrix (make-instance 'fast-matrix) 3 3))
        (l0 (make-zeros-matrix (make-instance 'matrix) 3 3))
        (l1 (make-universal-matrix (make-instance 'matrix) 3 3)))
    (is (not (matrix-entries-different-p
              (graph/matrix::matrix-product f0 f1)
              (graph/matrix::matrix-product l0 l1))))))

(deftest lisp-vs-fast-transpose ()
  (let ((f0 (make-zeros-matrix (make-instance 'fast-matrix) 3 3))
        (f1 (make-universal-matrix (make-instance 'fast-matrix) 3 3))
        (l0 (make-zeros-matrix (make-instance 'matrix) 3 3))
        (l1 (make-universal-matrix (make-instance 'matrix) 3 3)))
    (is (not (matrix-entries-different-p
              (matrix-transpose (graph/matrix::matrix-elementwise-product f0 f1))
              (matrix-transpose
               (graph/matrix::matrix-elementwise-product l0 l1)))))))

(deftest lisp-vs-fast-power-0 ()
  (let ((f0 (make-zeros-matrix (make-instance 'fast-matrix) 3 3))
        (f1 (make-universal-matrix (make-instance 'fast-matrix) 3 3))
        (l0 (make-zeros-matrix (make-instance 'matrix) 3 3))
        (l1 (make-universal-matrix (make-instance 'matrix) 3 3)))
    (is (not (matrix-entries-different-p
              (graph/matrix::matrix-power
               (graph/matrix::matrix-elementwise-product f0 f1) 0)
              (graph/matrix::matrix-power
               (graph/matrix::matrix-elementwise-product l0 l1) 0))))))

(deftest lisp-vs-fast-power-1 ()
  (let ((f0 (make-zeros-matrix (make-instance 'fast-matrix) 3 3))
        (f1 (make-universal-matrix (make-instance 'fast-matrix) 3 3))
        (l0 (make-zeros-matrix (make-instance 'matrix) 3 3))
        (l1 (make-universal-matrix (make-instance 'matrix) 3 3)))
    (is (not (matrix-entries-different-p
              (graph/matrix::matrix-power
               (graph/matrix::matrix-elementwise-product f0 f1) 1)
              (graph/matrix::matrix-power
               (graph/matrix::matrix-elementwise-product l0 l1) 1))))))

(deftest lisp-vs-fast-power-2 ()
  (let ((f0 (make-zeros-matrix (make-instance 'fast-matrix) 3 3))
        (f1 (make-universal-matrix (make-instance 'fast-matrix) 3 3))
        (l0 (make-zeros-matrix (make-instance 'matrix) 3 3))
        (l1 (make-universal-matrix (make-instance 'matrix) 3 3)))
    (is (not (matrix-entries-different-p
              (graph/matrix::matrix-power
               (graph/matrix::matrix-elementwise-product f0 f1) 2)
              (graph/matrix::matrix-power
               (graph/matrix::matrix-elementwise-product l0 l1) 2))))))

(deftest lisp-vs-fast-power-3 ()
  (let ((f0 (make-zeros-matrix (make-instance 'fast-matrix) 3 3))
        (f1 (make-universal-matrix (make-instance 'fast-matrix) 3 3))
        (l0 (make-zeros-matrix (make-instance 'matrix) 3 3))
        (l1 (make-universal-matrix (make-instance 'matrix) 3 3)))
    (is (not (matrix-entries-different-p
              (graph/matrix::matrix-power
               (graph/matrix::matrix-elementwise-product f0 f1) 3)
              (graph/matrix::matrix-power
               (graph/matrix::matrix-elementwise-product l0 l1) 3))))))

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

(deftest adjacency-matrix-vs-fast-matrix-digraph ()
  (with-fixture hh-4-18
    (let ((m (to-adjacency-matrix *graph* (make-instance 'matrix)))
          (f (to-adjacency-matrix *graph* (make-instance 'fast-matrix))))
      (is (not (matrix-entries-different-p m f))))))

(deftest reachability-matrix-vs-fast-matrix ()
  (with-fixture basic-graph
    (let ((m (to-reachability-matrix *graph* (make-instance 'matrix)))
          (f (to-reachability-matrix *graph* (make-instance 'fast-matrix))))
      (is (not (matrix-entries-different-p m f))))))

(deftest reachablep-lisp-vs-fast ()
  (with-fixture hh-5-10
    (let ((m (to-reachability-matrix *graph* (make-instance 'matrix)))
          (f (to-reachability-matrix *graph* (make-instance 'fast-matrix))))
      (is (and (reachablep *graph* m 1 2) (reachablep *graph* f 1 2)))
      (is (and (reachablep *graph* m 2 3) (reachablep *graph* f 2 3)))
      (is (not (or (reachablep *graph* f 2 1) (reachablep *graph* f 2 1)))))))

(deftest reachable-from-lisp-vs-fast ()
  (with-fixture hh-5-10
    (let ((m (to-reachability-matrix *graph* (make-instance 'matrix)))
          (f (to-reachability-matrix *graph* (make-instance 'fast-matrix))))
      (is (equal (reachable-from *graph* m 1) (reachable-from *graph* f 1)))
      (is (equal (reachable-from *graph* m 2) (reachable-from *graph* f 2)))
      (is (equal (reachable-from *graph* m 3) (reachable-from *graph* f 3)))
      (is (equal (reachable-from *graph* m 4) (reachable-from *graph* f 4)))
      (is (equal (reachable-from *graph* m 1) '(1 2 3 4)))
      (is (equal (reachable-from *graph* m 2) '(2 3 4)))
      (is (equal (reachable-from *graph* m 3) '(2 3 4)))
      (is (equal (reachable-from *graph* m 4) '(4))))))

(deftest strong-component-matrix-vs-fast-matrix ()
  (with-fixture hh-5-10
    (let ((m (to-reachability-matrix *graph* (make-instance 'matrix)))
          (f (to-reachability-matrix *graph* (make-instance 'fast-matrix))))
      (is (not (matrix-entries-different-p
                (to-strong-component-matrix m)
                (to-strong-component-matrix f)))))))

(deftest strong-component-of-lisp-vs-fast-matrix ()
  (with-fixture hh-5-10
    (let ((m (to-strong-component-matrix
              (to-reachability-matrix *graph* (make-instance 'matrix))))
          (f (to-strong-component-matrix
              (to-reachability-matrix *graph* (make-instance 'fast-matrix)))))
      (is (equal (strong-component-of 1 *graph* m)
                 (strong-component-of 1 *graph* f)))
      (is (equal (strong-component-of 2 *graph* m)
                 (strong-component-of 2 *graph* f)))
      (is (equal (strong-component-of 3 *graph* m)
                 (strong-component-of 3 *graph* f)))
      (is (equal (strong-component-of 4 *graph* m)
                 (strong-component-of 4 *graph* f))))))

(deftest distance-matrix-vs-fast-matrix ()
  (with-fixture basic-graph
    (let ((m (to-distance-matrix *graph* (make-instance 'matrix)))
          (f (to-distance-matrix *graph* (make-instance 'fast-matrix))))
      (is (not (matrix-entries-different-p m f))))))

(deftest distance-from-to-lisp-vs-fast-matrix ()
  (with-fixture basic-graph
    (let ((m (to-distance-matrix *graph* (make-instance 'matrix)))
          (f (to-distance-matrix *graph* (make-instance 'fast-matrix))))
      (is (= (distance-from-to *graph* m 'a 'b)
             (distance-from-to *graph* f 'a 'b)))
      (is (= (distance-from-to *graph* m 'a 'c)
             (distance-from-to *graph* f 'a 'c)))
      (is (= (distance-from-to *graph* m 'a 'd)
             (distance-from-to *graph* f 'a 'd)))
      (is (= (distance-from-to *graph* m 'a 'e)
             (distance-from-to *graph* f 'a 'e)))
      (is (= (distance-from-to *graph* m 'a 'f)
             (distance-from-to *graph* f 'a 'f))))))

;;; Tests comparing results of lisp matrix routines to Hage and Harary's book

(deftest digraph-and-adjacency-matrix ()
  (let ((m (make-instance 'matrix)))
    (setf (graph/matrix::self m)
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
    (setf (graph/matrix::self m)
          (make-array '(4 4)
                      :element-type 'fixnum
                      :initial-contents
                      '((1 1 1 1)(0 1 1 1)(0 1 1 1)(0 0 0 1))))
    (with-fixture hh-5-10
      (is (not (matrix-entries-different-p
                (to-reachability-matrix *graph* (make-instance 'matrix))
                m))))))

;;; This is the matrix R2 on p. 126 of Exchange in Oceania: A Graph
;;; Theoretic Analysis by Per Hage and Frank Harary

(deftest digraph-and-reachability-matrix-with-limit-2 ()
  (let ((m (make-instance 'matrix)))
    (setf (graph/matrix::self m)
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
         (i (graph/matrix::infinite m)))
    (setf (graph/matrix::self m)
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
    (setf (graph/matrix::self m)
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
