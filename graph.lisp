;;; graph.lisp --- because its easier to write than to learn such a library

;; Copyright (C) Eric Schulte 2012

;; Licensed under the Gnu Public License Version 3 or later

;;; Commentary

;; Graphs are composed of two hash tables, nodes and edges.
;;
;; node hash
;;  key -- node value
;;  val -- edge list
;;
;; edge hash
;;  key -- node list
;;  val -- edge value

;; What if we only used 1 hash to hold all nodes and edges..., and
;; what if nodes could hold other nodes in their edge list, and edges
;; could hold other edges in their node list, what sort of structure
;; would this be?  A useful generalization of a graph?

;;; Code:
(in-package :graph)

(defclass graph ()
  ((node-h :initarg :node-h :accessor node-h :initform (make-hash-table))
   (edge-h :initarg :edge-h :accessor edge-h :initform (make-hash-table :test 'equalp))
   (test   :initarg :test   :accessor test   :initform #'eql)
   (edge-comb :initarg :edge-comb :accessor edge-comb :initform nil)
   (node-comb :initarg :node-comb :accessor node-comb :initform nil)))

(defclass digraph (graph) ())

(defun copy-hash (hash)
  (let ((copy (make-hash-table :test (hash-table-test hash))))
    (maphash (lambda (k v) (setf (gethash k copy) v)) hash)
    copy))

(defmethod digraph-of ((graph graph))
  (make-instance 'digraph
    :node-h    (node-h graph)
    :edge-h    (edge-h graph)
    :test      (test graph)
    :edge-comb (edge-comb graph)
    :node-comb (node-comb graph)))

(defmethod graph-of ((digraph digraph))
  (make-instance 'digraph
    :node-h    (node-h digraph)
    :edge-h    (edge-h digraph)
    :test      (test digraph)
    :edge-comb (edge-comb digraph)
    :node-comb (node-comb digraph)))

(defmethod edges ((graph graph))
  "Return a list of the edges in GRAPH."
  (loop :for key :being :each :hash-key :of (edge-h graph) :collect key))

(defmethod edges-w-values ((graph graph) &aux alist)
  "Return an alist of edges of GRAPH with their values."
  (maphash (lambda (edge value) (push (cons edge value) alist)) (edge-h graph))
  alist)

(defmethod nodes ((graph graph))
  "Return a list of the nodes in GRAPH."
  (loop :for key :being :each :hash-key :of (node-h graph) :collect key))

(defmethod nodes-w-values ((graph graph) &aux alist)
  "Return an alist of nodes of GRAPH with their values."
  (maphash (lambda (node value) (push (cons node value) alist)) (node-h graph))
  alist)

(defmethod has-node-p ((graph graph) node)
  "Return `true' if GRAPH has node NODE."
  (multiple-value-bind (value included) (gethash node (node-h graph))
    (declare (ignorable value)) included))

(defmethod has-edge-p ((graph graph) edge)
  "Return `true' if GRAPH has edge EDGE."
  (multiple-value-bind (value included) (gethash edge (edge-h graph))
    (declare (ignorable value)) included))

(defmethod add-node ((graph graph) node)
  "Add NODE to GRAPH."
  (unless (has-node-p graph node)
    (setf (gethash node (node-h graph)) nil)
    node))

(defmethod add-edge ((graph graph) edge &optional value)
  "Add EDGE to GRAPH with optional VALUE."
  (mapc (lambda (node)
          (add-node graph node)
          (pushnew (case (type-of graph)
                     (graph (remove-duplicates edge))
                     (digraph edge))
                   (gethash node (node-h graph))))
        edge)
  (setf (gethash edge (edge-h graph)) value)
  edge)

(defmethod node-edges ((graph graph) node)
  "Return the value of NODE in GRAPH."
  (multiple-value-bind (edges included) (gethash node (node-h graph))
    (unless included (error 'missing-node "~S doesn't include ~S" graph node))
    edges))

(defmethod (setf node-edges) (new (graph graph) node)
  "Set the edges of NODE in GRAPH to NEW.
Delete and return the old edges of NODE in GRAPH."
  (prog1 (mapc (curry #'delete-edge graph) (gethash node (node-h graph)))
    (mapc (curry #'add-edge graph) new)))

(defmethod delete-node ((graph graph) node)
  "Delete NODE from GRAPH.
Delete and return the old edges of NODE in GRAPH."
  (prog1 (mapcar (lambda (edge) (cons edge (delete-edge graph edge)))
                 (node-edges graph node))
    (remhash node (node-h graph))))

(defmethod edge-value ((graph graph) edge)
  "Return the value of EDGE in GRAPH."
  (multiple-value-bind (value included) (gethash edge (edge-h graph))
    (unless included (error 'missing-edge "~S doesn't include ~S" graph edge))
    value))

(defmethod (setf edge-value) (new (graph graph) edge)
  "Set the value of EDGE in GRAPH to NEW."
  (setf (gethash edge (edge-h graph)) new))

(defmethod delete-edge ((graph graph) edge)
  "Delete EDGE from GRAPH.
Return the old value of EDGE."
  (prog1 (edge-value graph edge)
    (mapc (lambda (node) (setf (gethash node (node-h graph))
                          (delete edge (gethash node (node-h graph))
                                  :test #'equalp)))
          edge)
    (remhash edge (edge-h graph))))

(defun make-graph (&key (nodes nil) (edges nil) (test #'eql))
  "Make a graph."
  (let ((g (make-instance 'graph :test test)))
    (mapc (curry #'add-node g) nodes)
    (mapc (curry #'add-edge g) edges)
    g))

(defmethod copy ((graph graph))
  "Return a copy of GRAPH."
  (make-instance 'graph
    :test (test graph)
    :node-h (copy-hash (node-h graph))
    :edge-h (copy-hash (edge-h graph))))


;;; Complex graph methods
(defmethod merge-nodes ((graph graph) node1 node2 new)
  "Combine NODE1 and NODE2 in GRAPH into the node NEW.
All edges of NODE1 and NODE2 in GRAPH will be combined into a new node
holding VALUE."
  (add-node graph new)
  (mapc (lambda-bind ((edge . val))
          (let ((edge (mapcar (lambda (node)
                                (if (member node (list node1 node2))
                                    new node))
                              edge)))
            (if (has-edge-p graph edge)
                (when (edge-comb graph)
                  (setf (edge-value graph edge)
                        (funcall (edge-comb graph)
                                 (edge-value graph edge) val)))
                (add-edge graph edge val))))
        (append (delete-node graph node1)
                (delete-node graph node2)))
  graph)

(defmethod merge-edges ((graph graph) edge1 edge2)
  "Combine EDGE1 and EDGE2 in GRAPH into a new EDGE.
Optionally provide a value for the new edge, otherwise if `edge-comb'
is defined for GRAPH it will be used or no value will be assigned."
  (add-edge graph (remove-duplicates (append edge1 edge2))
            (when (edge-comb graph)
              (funcall (edge-comb graph)
                       (edge-value graph edge1) (edge-value graph edge2))))
  (append (delete-edge graph edge1)
          (delete-edge graph edge2)))

(defmethod neighbors ((graph graph) node)
  "Return all nodes which share an edge with NODE in GRAPH."
  (apply (curry #'concatenate 'list) (node-edges graph node)))

(defmethod outgoing-neighbors ((digraph digraph) node)
  "Return all nodes after NODE in DIGRAPH along directed edges."
  (mapcan (compose #'cdr (curry #'member node))
          (node-edges digraph node)))

(defmethod incoming-neighbors ((digraph digraph) node)
  "Return all nodes before NODE in DIGRAPH along directed edges."
  (mapcan (compose #'cdr (curry #'member node) #'reverse)
          (copy-tree (node-edges digraph node))))

(defmethod path-step ((graph graph) path)
  "∀ edge e leaving PATH in GRAPH return a new path of (cons e PATH)."
  (mapcar (lambda (next) (cons next path))
          (case (type-of graph)
            (graph   (neighbors graph (car path)))
            (digraph (outgoing-neighbors graph (car path))))))

(defmethod connected-by ((graph graph) node func)
  "Return the components of GRAPH connected to NODE by FUNC."
  (let ((from (list node)) (seen))
    (loop :until (null from) :do
       (let ((next (remove-duplicates (mapcan func from))))
         (setf from (remove node (set-difference next seen)))
         (setf seen (union next seen))))
    (reverse seen)))

(defmethod connected-component ((graph graph) node)
  "Return all nodes reachable from NODE."
  (connected-by graph node (curry #'neighbors graph)))

(defmethod reachable-from ((digraph digraph) node)
  "Return all node reachable along directed edges from NODE in DIGRAPH."
  (connected-by digraph node (curry #'outgoing-neighbors digraph)))

(defmethod connectedp ((graph graph))
  "Return true if the graph is connected."
  (let ((nodes (nodes graph)))
    (subsetp (nodes graph) (connected-component graph (car nodes)))))

(defmethod fully-reachable ((digraph digraph))
  "Return true if directed edges connect every pair of nodes."
  (every (compose (curry #'subsetp (nodes digraph))
                  (curry #'reachable-from digraph))
         (nodes digraph)))

(defun connected-components- (graph nodes ccs)
  (if (null nodes) ccs
      (let ((cc (connected-component graph (car nodes))))
        (connected-components- graph (set-difference nodes cc) (cons cc ccs)))))

(defmethod connected-components ((graph graph))
  "Return a list of the connected components of GRAPH."
  (connected-components- graph (nodes graph) nil))

(defun cycle- (graph front seen cycles &aux next-front)
  "Helper function for recursive portion of `cycles'."
  ;; take a step from every path
  (loop :for path :in (mapcan (curry #'path-step graph) front) :do
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
  "Return all directed `cycles' in GRAPH."
  (cycle- graph nil nil nil))

(defmethod cycle-connected-components
    ((graph graph) &optional (cycles (cycles graph)))
  "Return the groups of nodes of GRAPH which are connected by cycles."
  (mapcar
   (lambda (cc) (remove-duplicates cc :test #'tree-equal))
   (reduce (lambda (acc cycle)
             (let ((has (curry #'intersection cycle)))
               (cons (apply #'append (cons cycle (remove-if-not has acc)))
                     (remove-if has acc))))
           cycles :initial-value nil)))


;;; Shortest Path
(defmethod shortest-path ((graph graph) a b &aux seen)
  "Return the shortest path in GRAPH from A to B.
GRAPH must be a directed graph.  Dijkstra's algorithm is used."
  (block nil
    (let ((next (list (list a))))
      (loop :until (null next) :do
         (setf next
               (mapcan
                (lambda-bind ((from . rest))
                  (mapcan
                   (lambda (edge)
                     (if (member b (cdr (member from edge)))
                         (return (reverse (cons edge rest)))
                         (unless (member edge seen :test #'tree-equal)
                           (push edge seen)
                           (mapcar (lambda (n) (cons n (cons edge rest)))
                                   ;; nodes after from in edge
                                   (cdr (member from edge))))))
                   (node-edges graph from)))
                next))))))


;;; Max Flow
;; - Must be a "network" (digraph in which each edge has a positive weight)
;; - Ford-Fulkerson is used
(defmethod residual ((graph graph) flow)
  "Return the residual graph of GRAPH with FLOW.
Each edge in the residual has a value equal to the original capacity
minus the current flow, or equal to the negative of the current flow."
  (flet ((flow-value (edge) (or (cdr (assoc edge flow :test #'tree-equal)) 0)))
    (let ((residual (make-instance 'graph :test (test graph))))
      (mapc (lambda (edge)
              (let ((left (- (edge-value graph edge) (flow-value edge))))
                (when (not (zerop left))
                  (add-edge residual edge left)))
              (when (not (zerop (flow-value edge)))
                (add-edge residual (reverse edge) (flow-value edge))))
            (edges graph))
      residual)))

(defun add-paths (path1 path2)
  "Return the combination of numerically valued paths PATH1 and PATH2.
Each element of path has the form (cons edge value)."
  (let ((comb (copy-tree path1)))
    (mapc (lambda (edge-w-val)
            (let ((e (car edge-w-val))
                  (v (cdr edge-w-val)))
              (cond
                ((assoc e comb :test #'tree-equal)
                 (incf (cdr (assoc e comb :test #'tree-equal)) v))
                ((assoc (reverse e) comb :test #'tree-equal)
                 (decf (cdr (assoc (reverse e) comb :test #'tree-equal)) v))
                (t
                 (push edge-w-val comb)))))
          path2)
    comb))

(defun max-flow- (graph from to flow)
  ;; "augmenting path" is path through residual network in which each
  ;; edge has positive capacity
  (flet ((shortest-w-value (graph from to)
           (mapcar (lambda (edge) (cons edge (edge-value graph edge)))
                   (shortest-path graph from to)))
         (trim-path (path)
           (when path
             (let ((flow (apply #'min (mapcar #'cdr path))))
               (mapcar (lambda (el) (cons (car el) flow)) path)))))
    (let* ((residual (residual graph flow))
           (augment (trim-path (shortest-w-value residual from to))))
      (if augment
          ;; if ∃ an augmenting path, add it to the flow and repeat
          (max-flow- graph from to (add-paths flow augment))
          ;; otherwise we already have the max flow
          flow))))

(defun flow-value-into (flow node)
  "Return the value of the flow into NODE from FLOW."
  (reduce #'+ (remove-if-not (lambda (el) (equal (lastcar (car el)) node)) flow)
          :key #'cdr))

(defun combine-flows (flow1 val1 flow2 val2)
  (values (append flow1 flow2) (+ val1 val2)))

(defmethod max-flow ((graph graph) from to)
  "Return the maximum flow from FROM and TO in GRAPH.
GRAPHS must be a network with numeric values of all edges.
The Ford-Fulkerson algorithm is used."
  (let ((flow (max-flow- graph from to nil)))
    (values flow (flow-value-into flow to))))


;;; Min Cut
(defmethod min-s-t-cut ((graph graph) from to)
  "Return the minimum cut between FROM and TO in GRAPH.")

;; Stoer, M. and Wagner, Frank. 1997. A Simple Min-Cut Algorithm.
;; Journal of the ACM
;;
;; Theorem: Let s,t ∈ (nodes G), let G' be the result of merging s and
;;          t in G.  Then (min-cut G) is equal to the minimum of
;;          (min-s-t-cut G) and (min-cut G').
;;
(defmethod min-cut ((graph graph))
  (format t "start: ~S~%" (edges-w-values graph))
  (flet ((flow-to-cut (graph flow from)
           (let* ((residual (residual graph flow))
                  (half (dir-connected-component residual from)))
             (list half (set-difference (nodes graph) half)))))
    (if (= (length (nodes graph)) 2)
        (progn
          (format t "base: ~S~%" (edges-w-values graph))
          (values (mapcar #'list (nodes graph))
                  (reduce (lambda (acc edge) (+ (abs (edge-value graph edge)) acc))
                          (remove-if-not (curry #'subsetp (nodes graph))
                                         (edges graph))
                          :initial-value 0)))
        (let* ((from (random-elt (nodes graph)))
               (to (random-elt (remove from (nodes graph)))))
          (format t "between: ~S ~S~%" from to)
          (multiple-value-bind (flow f-size)
              (multiple-value-call #'combine-flows
                (max-flow graph from to)
                (max-flow graph to from))
            (format t "flow: ~S~%" f-size)
            (if (zerop f-size)
                (values (flow-to-cut graph flow from) f-size)
                (multiple-value-bind (cut c-size)
                    (min-cut (merge-nodes (copy graph) from to from))
                  (format t "flow=~S cut=~S~%" f-size c-size)
                  (if (< f-size c-size)
                      (values (flow-to-cut graph flow from) f-size)
                      (values cut c-size)))))))))
