;;; graph.lisp --- because its easier to write than to learn such a library

;; Copyright (C) Eric Schulte 2012

;; Licensed under the Gnu Public License Version 3 or later

;;; Code:
(in-package :graph)
(require :stefil)
(use-package :stefil)
(defsuite graph)
(in-suite graph)

(defvar *graph* nil
  "Variable for use in graph tests.")

(defixture small-graph
  (:setup (setf *graph*
                (make-graph
                 :nodes '(:foo :bar :baz :qux)
                 :edges '((:foo :bar)
                          (:foo :baz)
                          (:bar :baz)))))
  (:teardown (setf *graph* nil)))

(defixture graph
  (:setup (setf *graph*
                (make-graph
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
(deftest make-graph-sets-nodes ()
  (with-fixture small-graph
    (is (tree-equal (nodes *graph*)
                    '(:FOO :BAR :BAZ :QUX)))))

(deftest make-graph-sets-edges ()
  (with-fixture small-graph
    (is (tree-equal (edges *graph*)
                    '((:FOO :BAR) (:FOO :BAZ) (:BAR :BAZ))))))

(deftest node-edge-for-foo ()
  (with-fixture small-graph
    (is (tree-equal (node-edges *graph* :foo)
                    '((:FOO :BAZ) (:FOO :BAR))))))

(deftest edge-value-for-foo-bar ()
  (with-fixture small-graph
    (is (null (edge-value *graph* '(:foo :bar))))
    (setf (edge-value *graph* '(:foo :bar)) 22)
    (is (= 22 (edge-value *graph* '(:foo :bar))))))

(deftest dir-neighbors-on-graph ()
  (with-fixture graph
    (is (tree-equal (dir-neighbors *graph* 'e)
                    '(C F)))))

(deftest cycles ()
  (with-fixture graph
    (is (tree-equal (cycles *graph*)
                    '((C D E F B) (D E C))))))

(deftest cycle-connected-components-1 ()
  (with-fixture graph
    (is (tree-equal (cycle-connected-components *graph*)
                    '((D E C C D E F B))))))

(deftest cycle-connected-components-2 ()
  (with-fixture graph
    (is (tree-equal (cycle-connected-components *g2*)
                    '((:BAR :BAZ :FOO) (:ZAP :ZAF :QUX) (:FIZ))))))

(deftest shortest-path ()
  (with-fixture small-graph
    (is (tree-equal (shortest-path *g* '(:foo) '(:baz :qux))
                    '(:FOO :BAZ)))))
