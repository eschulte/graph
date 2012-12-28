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
                    '((:FOO :BAZ) (:FOO :BAR))))
    (is (tree-equal (setf (node-edges *graph* :foo) '((:foo :qux)))
                    '((:FOO :BAZ) (:FOO :BAR))))
    (is (tree-equal (node-edges *graph* :foo)
                    '((:FOO :QUX))))
    (is (set-equal (edges *graph*)
                   '((:FOO :QUX) (:BAR :BAZ))
                   :test #'tree-equal))))

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

(deftest merge-nodes-in-two-graphs ()
  (with-fixture small-graph
    (is (set-equal (merge-nodes *graph* :bar :baz :zap) '((:foo) (:foo))
                   :test #'tree-equal))
    (is (set-equal (nodes *graph*) '(:FOO :QUX :ZAP)))
    (is (set-equal (edges *graph*) '((:ZAP :FOO))
                   :test #'tree-equal))))

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

(deftest connected-to-e-in-normal-graph ()
  (with-fixture normal-graph
    (is (set-equal (connected-to *graph* 'e)
                   (nodes *graph*)))))

(deftest dir-connected-to-e-in-normal-graph ()
  (with-fixture normal-graph
    (is (set-equal (dir-connected-to *graph* 'e)
                   '(C F B D E)))))

(deftest dir-connected-to-foo-in-small-graph ()
  (with-fixture small-graph
    (is (set-equal (dir-connected-to *graph* :foo)
                   '(:bar :baz)))))

(deftest connectedp-of-multiple-graphs ()
  (with-fixture small-graph (is (not (connectedp *graph*))))
  (with-fixture less-small-graph (is (not (connectedp *graph*))))
  (with-fixture normal-graph (is (connectedp *graph*))))

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
