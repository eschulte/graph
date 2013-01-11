;;; graph.lisp --- because its easier to write than to learn such a library

;; Copyright (C) Eric Schulte 2012

;; Licensed under the Gnu Public License Version 3 or later

;;; Code:
(in-package :graph)
(require :stefil)
(use-package :stefil)
(defsuite graph-test)
(in-suite graph-test)

(defvar *graph* nil
  "Variable for use in graph tests.")

(defvar *network* nil
  "Variable for use in graph tests.")

(defvar *cycle* nil
  "Variable for use in graph tests.")

(defixture small-graph
  (:setup (setf *graph*
                (populate (make-instance 'graph)
                  :nodes '(:foo :bar :baz :qux)
                  :edges '((:foo :bar)
                           (:foo :baz)
                           (:bar :baz)))))
  (:teardown (setf *graph* nil)))

(defixture less-small-graph
  (:setup (setf *graph*
                (populate (make-instance 'graph)
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

(defixture small-network
  (:setup (setf *network*
                (populate (make-instance 'graph :edge-comb #'+)
                  :nodes '(:a :b :s :t)
                  :edges-w-values
                  '(((:a :b) . 1)
                    ((:s :a) . 2)
                    ((:s :b) . 1)
                    ((:a :t) . 4)
                    ((:b :t) . 2)))))
  (:teardown (setf *network* nil)))

(defixture cycle
  (:setup (setf *cycle*
                (populate (make-instance 'graph :edge-comb #'+)
                  :nodes '(:a :b :s :t)
                  :edges-w-values
                  '(((:s :a) . 1)
                    ((:s :b) . 3)
                    ((:b :a) . 1)
                    ((:a :t) . 2)
                    ((:b :t) . 2)
                    ((:t :s) . 2)))))
  (:teardown (setf *cycle* nil)))


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

(deftest copy-of-a-graph ()
  (with-fixture small-graph
    (let ((c (copy *graph*)))
      (is (set-equal (nodes *graph*) (nodes c)))
      (is (tree-equal (edges *graph*) (edges c) :test #'tree-equal))
      (delete-node c :foo)
      (is (not (set-equal (nodes *graph*) (nodes c))))
      (is (not (tree-equal (edges *graph*) (edges c) :test #'tree-equal))))))

(deftest merge-nodes-in-small-graph ()
  (with-fixture small-graph
    (setf *graph* (merge-nodes *graph* :bar :baz :new :zap))
    (is (set-equal (nodes *graph*) '(:FOO :QUX :ZAP)))
    (is (set-equal (edges *graph*) '((:FOO :ZAP))
                   :test #'tree-equal))))

(deftest merge-nodes-in-small-network ()
    (with-fixture small-network
      (setf *network* (merge-nodes *network* :a :b :new :ab))
      (is (set-equal (nodes *network*) '(:S :T :AB)))
      (is (set-equal (edges-w-values *network*)
                     '(((:S :AB) . 3) ((:AB :T) . 6))
                     :test #'tree-equal))))

(with-fixture small-network
  (nodes *network*))

(deftest merge-edges-in-small-graph ()
  (with-fixture small-graph
    (merge-edges *graph* '(:foo :bar) '(:foo :baz))
    (is (set-equal (edges *graph*) '((:BAR :BAZ) (:BAR :FOO :BAZ))
                   :test #'tree-equal))))

(deftest edge-neighbors-of-c-on-graph ()
  (with-fixture less-small-graph
    (is (set-equal (edge-neighbors *graph* '(:foo :bar))
                   '((:BAZ :FOO) (:FOO :BAR) (:BAR :BAZ) (:FOO :BAR))
                   :test #'tree-equal))))

(deftest neighbors-of-c-on-graph ()
  (with-fixture normal-graph
    (is (every (lambda (it) (member it (neighbors *graph* 'b)))
               '(a b c)))))

(deftest neighbors-of-e-on-digraph ()
  (with-fixture normal-graph
    (is (set-equal (neighbors (digraph-of *graph*) 'e)
                   '(C F)))))

(deftest conected-component-of-e-on-digraph ()
  (with-fixture normal-graph
    (is (set-equal (connected-component (digraph-of *graph*) 'e)
                   '(C E D B F)))))

(deftest is-normal-digraph-connected ()
  (with-fixture normal-graph
    (is (not (connectedp (digraph-of *graph*))))))

(deftest connected-component-e-in-normal-graph ()
  (with-fixture normal-graph
    (is (set-equal (connected-component *graph* 'e)
                   (nodes *graph*)))))

(deftest connected-component-of-foo-in-small-digraph ()
  (with-fixture small-graph
    (is (set-equal (connected-component (digraph-of *graph*) :foo)
                   '(:bar :baz)))))

(deftest connectedp-of-multiple-graphs ()
  (with-fixture small-graph (is (not (connectedp *graph*))))
  (with-fixture less-small-graph (is (not (connectedp *graph*))))
  (with-fixture normal-graph (is (connectedp *graph*))))

(deftest connected-components-of-less-small-graph ()
  (with-fixture less-small-graph
    (is (set-equal (connected-components *graph*)
                   '((:ZAP :ZAF :QUX) (:FIZ) (:BAZ :FOO :BAR))
                   :test #'tree-equal))))

(deftest strongly-connected-components-of-small-graph ()
  (with-fixture small-graph
    (is (set-equal (strongly-connected-components *graph*)
                   '((:QUX) (:BAR :BAZ :FOO))
                   :test #'set-equal))))

(deftest basic-cycles-of-small-graph ()
  (with-fixture small-graph
    (is (set-equal (basic-cycles *graph*)
                   '((:BAR :BAZ :FOO))
                   :test #'set-equal))))

(deftest basic-cycles-of-less-small-graph ()
  (with-fixture less-small-graph
    (is (set-equal (basic-cycles *graph*)
                   '((:ZAF :ZAP :QUX) (:BAR :BAZ :FOO))
                   :test #'set-equal))))

(deftest basic-cycles-of-graph ()
  (with-fixture normal-graph
    (is (set-equal (basic-cycles *graph*)
                   '((C E F B) (D C E))
                   :test #'set-equal))))

(deftest cycles-of-graph ()
  (with-fixture normal-graph
    (is (set-equal (cycles *graph*)
                   '((D C E) (B F E C E D C) (C E F B))
                   :test #'set-equal))))

(deftest shortest-path-between-foo-and-baz-or-qux ()
  (with-fixture less-small-graph
    (is (tree-equal (shortest-path *graph* :foo :baz)
                    '((:FOO :BAR) (:BAR :BAZ))))))

(deftest residual-of-a-small-network ()
  (with-fixture small-network
    (let ((orig-edges (copy-tree (edges-w-values *network*))))
      (is (set-equal
           (edges-w-values (residual (digraph-of *network*)
                                     '(((:s :a) . 2)
                                       ((:s :b) . 1)
                                       ((:a :b) . 1)
                                       ((:a :t) . 1)
                                       ((:b :t) . 2))))
           '(((:T :B) . 2)
             ((:T :A) . 1)
             ((:A :T) . 3)
             ((:B :S) . 1)
             ((:A :S) . 2)
             ((:B :A) . 1))
           :test #'tree-equal))
      (is (tree-equal orig-edges (edges-w-values *network*))))))

(deftest max-flow-on-a-small-network ()
  (with-fixture small-network
    (multiple-value-bind (path flow) (max-flow *network* :s :t)
      (is (set-equal path
                     '(((:A :T) . 2) ((:S :A) . 2) ((:B :T) . 1) ((:S :B) . 1))
                     :test #'tree-equal))
      (is (= flow 3)))))

(deftest max-flow-with-a-cycle ()
  (with-fixture cycle
    (multiple-value-bind (flow value) (max-flow *cycle* :s :t)
      (is (set-equal flow
                     '(((:B :A) . 1)
                       ((:A :T) . 2)
                       ((:S :A) . 1)
                       ((:B :T) . 2)
                       ((:S :B) . 3))
                     :test #'tree-equal))
      (is (= value 4)))))

(deftest min-s-t-cut-on-small-networks ()
  (with-fixture small-network
    (multiple-value-bind (cut weight) (min-s-t-cut *network*)
      (is (member :s cut))
      (is (= 3 weight))))
  (with-fixture small-network
    (setf (edge-value *network* '(:s :a)) 5)
    (multiple-value-bind (cut weight) (min-s-t-cut *network*)
      (is (member :b cut))
      (is (= 4 weight)))))
