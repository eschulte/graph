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

(defixture less-small-graph
  (:setup (setf *graph*
                (make-graph
                 :nodes '(:foo :bar :baz :qux :zap :zaf :fiz)
                 :edges '((:foo :bar)
                          (:bar :baz)
                          (:baz :foo)
                          (:zap :zaf)
                          (:zaf :qux)
                          (:qux :zap)
                          (:fiz :fiz)))))
  (:teardown (setf *graph* nil)))

(defixture normal-graph
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

(deftest delete-an-edge-from-small-graph ()
  (with-fixture small-graph
    (is (null (delete-edge *graph* '(:foo :bar))))
    (is (= 2 (length (edges *graph*))))
    (is (= 1 (length (node-edges *graph* :foo))))))

(deftest edge-value-for-foo-bar ()
  (with-fixture small-graph
    (is (null (edge-value *graph* '(:foo :bar))))
    (setf (edge-value *graph* '(:foo :bar)) 22)
    (is (= 22 (edge-value *graph* '(:foo :bar))))))

(deftest neighbors-of-c-on-graph ()
  (with-fixture normal-graph
    (is (every (lambda (it) (member it (neighbors *graph* 'b)))
               '(a b c)))))

(deftest dir-neighbors-on-graph ()
  (with-fixture normal-graph
    (is (set-equal (dir-neighbors *graph* 'e)
                   '(C F)))))

(deftest dis-step-on-graph-from-e ()
  (with-fixture normal-graph
    (is (set-equal (dir-step *graph* '(e))
                   '((c e) (f e)) :test #'tree-equal))))

(deftest cycles-of-graph ()
  (with-fixture normal-graph
    (is (tree-equal (cycles *graph*)
                    '((C D E F B) (D E C))))))

(deftest cycle-connected-components-1 ()
  (with-fixture normal-graph
    (is (set-equal (cycle-connected-components *graph*)
                   '((C D E F B)) :test #'set-equal))))

(deftest cycle-connected-components-2 ()
  (with-fixture less-small-graph
    (is (set-equal (cycle-connected-components *graph*)
                   '((:BAR :BAZ :FOO) (:ZAP :ZAF :QUX) (:FIZ))
                   :test #'set-equal))))

(deftest shortest-path-between-foo-and-baz-or-qux ()
  (with-fixture less-small-graph
    (is (tree-equal (shortest-path *graph* '(:foo) '(:baz :qux))
                    '(:FOO :BAZ)))))
