;;; graph.lisp --- because its easier to write than to learn such a library

;; Copyright (C) Eric Schulte 2012

;; Licensed under the Gnu Public License Version 3 or later

;;; Code:
(in-package :graph-test)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

(defsuite test)
(in-suite test)

(defvar *graph* nil
  "Variable for use in graph tests.")

(defvar *network* nil
  "Variable for use in graph tests.")

(defvar *cycle* nil
  "Variable for use in graph tests.")

(defvar *halfs* nil
  "Variable for use in graph tests.")

(defvar *star* nil
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
                (populate (make-instance 'graph)
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
                (populate (make-instance 'digraph)
                  :nodes '(:a :b :s :t)
                  :edges-w-values
                  '(((:s :a) . 1)
                    ((:s :b) . 3)
                    ((:b :a) . 1)
                    ((:a :t) . 2)
                    ((:b :t) . 2)
                    ((:t :s) . 2)))))
  (:teardown (setf *cycle* nil)))

(defixture halfs
  (:setup (setf *halfs*
                (populate (make-instance 'graph)
                  :edges-w-values
                  '(((:a :b) . 10)
                    ((:b :c) . 10)
                    ((:c :a) . 10)

                    ((:q :r) . 20)
                    ((:r :s) . 20)
                    ((:s :q) . 20)

                    ((:c :s) . 2)))))
  (:teardown (setf *halfs* nil)))

(defixture star
  (:setup (setf *star*
                (populate (make-instance 'graph)
                  :edges '((:a :s)
                           (:b :s)
                           (:c :s)
                           (:d :s)
                           (:e :s)
                           (:f :s)
                           (:g :s)
                           (:h :s)))))
  (:teardown (setf *star* nil)))


;;; Tests
(deftest make-graph-sets-nodes ()
  (with-fixture small-graph
    (is (set-equal (nodes *graph*)
                   '(:FOO :BAR :BAZ :QUX)))))

(deftest make-graph-sets-edges ()
  (with-fixture small-graph
    (is (set-equal (edges *graph*)
                   '((:FOO :BAR) (:FOO :BAZ) (:BAR :BAZ))
                   :test 'tree-equal))))

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

(deftest add-duplicate-edge-to-small-graph ()
  (with-fixture small-graph
    (add-edge *graph* '(:bar :foo))
    (is (not (member '(:bar :foo) (edges *graph*) :test #'tree-equal)))
    (is (not (member '(:bar :foo) (node-edges *graph* :bar))))))

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

(deftest copy-of-a-graph-w-graph-equal ()
  (with-fixture less-small-graph
    (is (graph-equal *graph* (copy *graph*)))))

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
                   '(:foo :bar :baz)))
    (is (set-equal (connected-component (digraph-of *graph*) :bar)
                   '(:bar :baz)))))

(deftest connected-component-of-a-cycle ()
  (with-fixture cycle
    (is (set-equal (connected-component *cycle* :s) (nodes *cycle*)))))

(deftest connectedp-of-multiple-graphs ()
  (with-fixture small-graph (is (not (connectedp *graph*))))
  (with-fixture less-small-graph (is (not (connectedp *graph*))))
  (with-fixture normal-graph (is (connectedp *graph*))))

(deftest connected-components-of-less-small-graph ()
  (with-fixture less-small-graph
    (is (set-equal (connected-components *graph*)
                   '((:ZAP :ZAF :QUX) (:FIZ) (:BAZ :FOO :BAR))
                   :test #'set-equal))))

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
    (is (tree-equal (shortest-path (digraph-of *graph*) :foo :baz)
                    '((:FOO :BAR) (:BAR :BAZ))))))

(deftest shortest-path-through-a-residual ()
  (with-fixture cycle
    (let* ((flow '(((:A :T) . 1) ((:S :A) . 1) ((:B :T) . 2) ((:S :B) . 2)))
           (residual (residual *cycle* flow)))
      (is (shortest-path residual :s :t)))))

(deftest shortest-path-against-undirected-edge ()
  (with-fixture star
    (is (tree-equal (shortest-path *star* :a :g)
                    '((:a :s) (:g :s))))))

(deftest residual-of-a-small-network ()
  (with-fixture small-network
    (let ((orig-edges (copy-tree (edges-w-values *network*)))
          (resi-edges (edges-w-values
                       (residual (digraph-of *network*)
                                 '(((:s :a) . 2) ((:a :t) . 2))))))
      (is (= (cdr (assoc '(:a :s) resi-edges :test 'tree-equal))) 2)
      (is (= (cdr (assoc '(:a :t) resi-edges :test 'tree-equal))) 2)
      (is (tree-equal orig-edges (edges-w-values *network*))))))

(deftest max-flow-on-a-small-network ()
  (with-fixture small-network
    (multiple-value-bind (path flow) (max-flow (digraph-of *network*) :s :t)
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

(deftest min-cut-on-a-small-network ()
  (with-fixture small-network
    (multiple-value-bind (cut weight) (min-cut *network*)
      (is (set-equal cut '((:S) (:T :B :A)) :test 'set-equal))
      (is (= 3 weight)))))

(deftest min-cut-on-a-graph-of-two-halfs ()
  (with-fixture halfs
    (multiple-value-bind (cut weight) (min-cut *halfs*)
      (is (set-equal cut '((:a :b :c) (:q :r :s)) :test 'set-equal))
      (is (= 2 weight)))))

(deftest small-graph-to-plist ()
  (with-fixture small-graph
    (let ((plist (to-plist *graph*)))
      (is (set-equal (getf plist :nodes)
                     '((:NAME :FOO) (:NAME :BAR) (:NAME :BAZ) (:NAME :QUX))
                     :test 'tree-equal))
      (is (set-equal (getf plist :edges)
                     '((:EDGE (0 1) :VALUE NIL)
                       (:EDGE (0 2) :VALUE NIL)
                       (:EDGE (1 2) :VALUE NIL))
                     :test 'tree-equal)))))

(deftest two-way-plist-conversion-on-multiple-graphs ()
  (with-fixture small-graph
    (is (graph-equal *graph* (from-plist (make-instance 'graph)
                                         (to-plist *graph*)))))
  (with-fixture less-small-graph
    (is (graph-equal *graph* (from-plist (make-instance 'graph)
                                         (to-plist *graph*)))))
  (with-fixture small-network
    (is (graph-equal *network* (from-plist (make-instance 'graph)
                                           (to-plist *network*))))))

(deftest test-preferential-attachment-population ()
  (let ((graph (make-instance 'graph))
        (many 1000))
    (preferential-attachment-populate
     graph (loop :for i :below many :collect i))
    (is (= many (length (nodes graph))))
    (is (= (1- many) (length (edges graph))))))

(deftest farness-of-s-in-network ()
  (with-fixture small-network
    (is (= 4 (farness *network* :s)))))

(deftest betweenness-of-center-of-a-star ()
  (with-fixture star
    (is (= 1 (betweenness *star* :s)))
    (is (= 0 (betweenness *star* :a)))))

(deftest conversion-to-adjacency-matrix ()
  (with-fixture normal-graph
    (is (equalp (to-adjacency-matrix *graph*)
                #2A((nil T   nil nil nil nil)
                    (nil nil T   nil nil nil)
                    (nil nil nil T   nil nil)
                    (nil nil nil nil T   nil)
                    (nil nil T   nil nil T)
                    (nil T   nil nil nil nil)))))
  (with-fixture small-network
    (is (equalp (to-adjacency-matrix *network*)
                #2A((nil 1   nil 4)
                    (nil nil nil 2)
                    (2   1   nil nil)
                    (nil nil nil nil))))))

(deftest conversion-from-adjacency-matrix ()
  (is (tree-equal (edges-w-values (from-adjacency-matrix (make-instance 'graph)
                                                         #2A((nil 1 nil)
                                                             (nil nil 2)
                                                             (nil nil nil))))
                  '(((1 2) . 2) ((0 1) . 1)))))
