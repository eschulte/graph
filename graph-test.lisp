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

(defixture small-network
  (:setup (setf *network*
                (let ((n (make-instance 'graph)))
                  (mapc (curry #'add-node n) '(:a :b :s :t))
                  (mapc (lambda (edge-w-value)
                          (add-edge n (cdr edge-w-value) (car edge-w-value)))
                        '((1 :a :b)
                          (2 :s :a)
                          (1 :s :b)
                          (4 :a :t)
                          (2 :b :t)))
                  n)))
  (:teardown (setf *network* nil)))


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
      (is (tree-equal (nodes *graph*) (nodes c) :test #'tree-equal))
      (delete-node c :foo)
      (is (not (set-equal (nodes *graph*) (nodes c))))
      (is (not (tree-equal (nodes *graph*) (nodes c) :test #'tree-equal))))))

(deftest merge-nodes-in-small-graph ()
  (with-fixture small-graph
    (is (set-equal (merge-nodes *graph* :bar :baz :zap)
                   '((:ZAP :FOO) (:ZAP :FOO))
                   :test #'tree-equal))
    (is (set-equal (nodes *graph*) '(:FOO :QUX :ZAP)))
    (is (set-equal (edges *graph*) '((:ZAP :FOO))
                   :test #'tree-equal))))

(deftest merge-edges-in-small-graph ()
  (with-fixture small-graph
    (merge-edges *graph* '(:foo :bar) '(:foo :baz))
    (is (set-equal (edges *graph*) '((:BAR :BAZ) (:BAR :FOO :BAZ))
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
    (is (tree-equal (shortest-path *graph* :foo :baz)
                    '((:FOO :BAR) (:BAR :BAZ))))))

(deftest residual-of-a-small-network ()
  (with-fixture small-network
    (let ((orig-edges (copy-tree (edges-w-values *network*))))
      (is (set-equal
           (edges-w-values (residual *network*
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

(deftest test-path-addition ()
  (is (set-equal
       (add-paths
        '(((:foo :bar) . 4)
          ((:bar :baz) . 2))
        '(((:foo :bar) . 2)
          ((:baz :bar) . 1)
          ((:baz :qux) . 1)))
       '(((:BAZ :QUX) . 1) ((:FOO :BAR) . 6) ((:BAR :BAZ) . 1))
       :test #'tree-equal)))

(deftest test-max-flow ()
  (with-fixture small-network
    (multiple-value-bind (path flow) (max-flow *network* :s :t)
      (is (set-equal path
                     '(((:A :T) . 2) ((:S :A) . 2) ((:B :T) . 1) ((:S :B) . 1))
                     :test #'tree-equal))
      (is (= flow 3)))))
