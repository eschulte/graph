;;; graph.lisp --- because its easier to write than to learn such a library

;; Copyright (C) Eric Schulte 2012

;; Licensed under the Gnu Public License Version 3 or later

;;; Commentary

;; Graphs are composed of two hash tables, one for nodes and one for
;; edges.  Each node may hold an arbitrary value.  Each edge will be a
;; list of nodes.  When a node is stored in the node hash table, its
;; value will be a list of the edges holding the node.  Similarly the
;; value of every edge will be a list of the nodes the edge contains.

;; What if we only used 1 hash to hold all nodes and edges..., and
;; what if nodes could hold other nodes in their edge list, and edges
;; could hold other edges in their node list, what sort of structure
;; would this be?  A useful generalization of a graph?

;;; Code:
(in-package :modularize)

(defclass graph ()
  ((test   :initarg :test   :accessor test   :initform #'eql)
   (node-h :initarg :node-h :accessor node-h :initform (make-hash-table))
   (edge-h :initarg :edge-h :accessor edge-h :initform (make-hash-table))))

(defun gother (node-or-edge) (case node-or-edge (:edge :node) (:node :edge)))
(defmethod ghash ((graph graph) node-or-edge)
  (case node-or-edge
    (:node (node-h graph))
    (:edge (edge-h graph))))

(defmethod has-it-p ((graph graph) node-or-edge it)
  (multiple-value-bind (val contains) (gethash it (ghash graph node-or-edge))
    (declare (ignorable val)) contains))

(defmethod has-node-p ((graph graph) node)
  (has-it-p graph :node node))

(defmethod has-edge-p ((graph graph) edge)
  (has-it-p graph :edge edge))

(defmethod add-it ((graph graph) node-or-edge it &optional elements)
  (mapcar (lambda (el) (pushnew it (gethash el (ghash graph (gother node-or-edge)))
                           :test (test graph)))
          elements)
  (setf (gethash it (ghash graph node-or-edge)) elements))

(defmethod add-node ((graph graph) node &optional elements)
    (add-it graph :node node elements))

(defmethod add-edge ((graph graph) edge &optional elements)
  (add-it graph :edge edge elements))

(defmethod elements ((graph graph) node-or-edge it)
  (multiple-value-bind (edges hasp) (gethash it (ghash graph node-or-edge))
    (if hasp
        edges
        (error 'missing-node "~s doesn't have ~a ~s" graph node-or-edge it))))

(defmethod node-edges ((graph graph) node)
  (elements graph :node node))

(defmethod edge-nodes ((graph graph) edge)
  (elements graph :edge edge))

(defun make-graph (&key (nodes nil) (edges nil) (test #'eql))
  (let ((g (make-graph :test test)))
    (mapc (lambda (node) (add-node g (car node) (cdr node))) nodes)
    (mapc (lambda (edge) (add-edge g (car edge) (cdr edge))) edges)
    g))


;;; Complex graph methods

(defmethod merge-nodes ((graph graph) node1 node2 val)
  "Combine NODE1 and NODE2 in GRAPH into new node VAL.
All edges of NODE1 and NODE2 in GRAPH will be combined into a new node
holding VALUE."
  (edges ))

(defmethod neighbors ((graph graph) node)
  (let ((id (position node (nodes graph))))
    (mapcar (lambda (id) (nth id (nodes graph)))
            (apply #'append
                   (remove-if-not
                    (curry #'member id)
                    (mapcar (curry #'aget :nodes) (edges graph)))))))

(defmethod dir-neighbors ((graph graph) node)
  (let ((id (position node (nodes graph))))
    (mapcar (lambda (id) (nth id (nodes graph)))
            (apply #'append
                   (mapcar (compose
                            #'cdr ;; remove the original element
                            (curry #'member id) ;; only after orig in an edge
                            (curry #'aget :nodes))
                           (edges graph))))))

(defun dir-step (graph path)
  "Take all steps forward from a path through a graph.
Returns a new path for each possible next step."
  (mapcar (lambda (next) (cons next path)) (dir-neighbors graph (car path))))

(defun cycle (path)
  "Return any cycle that may exist in path."
  (loop :for i :below (length path) :do
     (let* ((rest (subseq path (1+ i)))
            (pos  (position (nth i path) rest)))
       (when pos (return (cons (nth i path) (subseq rest 0 pos)))))))

(defun cycle- (graph front seen cycles &aux next-front)
  "Helper function for recursive portion of `cycles'."
  ;; take a step from every path
  (loop :for path :in (mapcan (curry #'dir-step graph) front) :do
     ;; detect cycles
     (let ((cycle-point (position (car path) (cdr path))))
       ;; remove cycles from the front and collection them
       (if cycle-point
           (push (reverse (subseq path 0 (1+ cycle-point))) cycles)
           (unless (member (car path) seen) (push path next-front))))
     (push (car path) seen))
  ;; recurse
  (if next-front
      ;; continue with front
      (cycle- graph next-front seen cycles)
      ;; or restart with any previously unseen node, or return
      (let ((remaining (remove-if (lambda (it) (member it seen)) (nodes graph))))
        (if remaining
            (cycle- graph
                    (list (list (car remaining)))
                    (cons (car remaining) seen)
                    cycles)
            cycles))))

(defmethod cycles ((graph graph))
  (cycle- graph nil nil nil))

(defun cycle-connected-components (graph &optional (cycles (cycles graph)))
  "Return the groups nodes of GRAPH are connected by cycles."
  (mapcar
   (lambda (cc) (remove-duplicates cc :test #'tree-equal))
   (reduce (lambda (acc cycle)
             (let ((has (curry #'intersection cycle)))
               (cons (apply #'append (cons cycle (remove-if-not has acc)))
                     (remove-if has acc))))
           cycles :initial-value nil)))

(defun shortest-path- (graph paths dest seen)
  (catch 'done
    (dolist (path paths)
      (when (member (car path) dest) (throw 'done (reverse path))))
    (let ((next (mapcan
                 (lambda (path)
                   (mapcar (lambda (n)
                             (unless (member n seen)
                               (push n seen)
                               (cons n path)))
                           (neighbors graph (car path))))
                 paths)))
      (unless (null next)
        (shortest-path- graph next dest seen)))))

(defmethod shortest-path ((graph graph) a b)
  "Return the shortest in-GRAPH path from any member of A any member of B.
Dijkstra's algorithm."
  (shortest-path- graph (mapcar #'list a) b nil))


;;; tests
#+testing
(progn
  (defvar *g* (make-instance 'graph
                :nodes '(:foo :bar :baz :qux)
                :edges '(((:nodes . (0 1)))
                         ((:nodes . (0 2)))
                         ((:nodes . (1 2))))))

  (defvar *graph*
    (make-instance 'graph
      :nodes '(a b c d e f)
      :edges '(((:nodes 0 1))
               ((:nodes 1 2))
               ((:nodes 2 3))
               ((:nodes 3 4))
               ((:nodes 4 2))
               ((:nodes 4 5))
               ((:nodes 5 1)))))

  (is (tree-equal (dir-neighbors *graph* 'e)
                  '(C F)))

  (is (tree-equal (cycles *graph*)
                  '((C D E F B) (D E C))))

  ;; need to test not duplicating cycles


  (is (tree-equal (cycle-connected-components *graph*)
                  '((D E C C D E F B))))

  (is (tree-equal (cycle-connected-components *g2*)
                  '((:BAR :BAZ :FOO) (:ZAP :ZAF :QUX) (:FIZ))))

  (is (tree-equal (shortest-path *g* '(:foo) '(:baz :qux))
                  '(:FOO :BAZ))))


;;; utility
(defun aget (item alist) (cdr (assoc item alist)))

(defun partition (func list &aux results)
  (loop :for el :in list :do
     (let ((val (funcall func el)))
       (if (assoc val results)
           (push el (cdr (assoc val results)))
           (push (list val el) results))))
  results)
