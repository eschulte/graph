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


;;; Reader macros

;; partial application with {} using Alexandria's `curry' and `rcurry'
(set-syntax-from-char #\{ #\( )
(set-syntax-from-char #\} #\) )

(defun lcurly-brace-reader (stream inchar)
  (declare (ignore inchar))
  (let ((spec (read-delimited-list #\} stream t)))
    (if (eq (cadr spec) '_)
        `(rcurry (function ,(car spec)) ,@(cddr spec))
        `(curry (function ,(car spec)) ,@(cdr spec)))))

(set-macro-character #\{ #'lcurly-brace-reader)
(set-macro-character #\} (get-macro-character #\) ))

;; composition with [] using Alexandria's `compose'
(set-syntax-from-char #\[ #\( )
(set-syntax-from-char #\] #\) )

(defun lsquare-brace-reader (stream inchar)
  (declare (ignore inchar))
  (cons 'compose (read-delimited-list #\] stream t)))

(set-macro-character #\[ #'lsquare-brace-reader)
(set-macro-character #\] (get-macro-character #\) ))


;;; Graph objects and basic methods
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
  (prog1 (mapc {delete-edge graph} (gethash node (node-h graph)))
    (mapc {add-edge graph} new)))

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
    (mapc {add-node g} nodes)
    (mapc {add-edge g} edges)
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

(defmethod edge-neighbors ((graph graph) edge)
  "Return all edges which share a node with EDGE in GRAPH."
  (mapcan {node-edges graph} edge))

(defmethod neighbors ((graph graph) node)
  "Return all nodes which share an edge with NODE in GRAPH."
  (apply {concatenate 'list} (node-edges graph node)))

(defmethod neighbors ((digraph digraph) node)
  "Return all nodes following NODE in an edge in DIGRAPH."
  (mapcan [#'cdr {member node}] (node-edges digraph node)))

(defmethod precedents ((digraph digraph) node)
  "Return all nodes preceding NODE in an edge of DIGRAPH."
  (mapcan [#'cdr {member node} #'reverse]
          (copy-tree (node-edges digraph node))))

(defmethod connected-component ((graph graph) node)
  "Return the connected component of NODE in GRAPH."
  (let ((from (list node)) (seen))
    (loop :until (null from) :do
       (let ((next (remove-duplicates (mapcan {neighbors graph} from))))
         (setf from (remove node (set-difference next seen)))
         (setf seen (union next seen))))
    (reverse seen)))

(defmethod connectedp ((graph graph))
  "Return true if the graph is connected."
  (let ((nodes (nodes graph)))
    (subsetp (nodes graph) (connected-component graph (car nodes)))))

(defmethod connected-components ((graph graph))
  "Return a list of the connected components of GRAPH."
  (let ((nodes (nodes graph)) ccs)
    (loop :until (null nodes) :do
       (let ((cc (connected-component graph (car nodes))))
         (setf nodes (set-difference nodes cc))
         (setf ccs (cons cc ccs))))
    ccs))


;;; Cycles and strongly connected components
(defmethod strongly-connected-components ((graph graph))
  "Return the nodes of GRAPH partitioned into strongly connected components.
Uses Tarjan's algorithm."
  (let ((index (make-hash-table))
        (lowlink (make-hash-table))
        (counter 0) stack sccs)
    (labels ((tarjan (node)
               ;; mark this node
               (setf (gethash node index) counter)
               (setf (gethash node lowlink) counter)
               (incf counter)
               (push node stack)
               ;; consider successors
               (mapc (lambda (neighbor)
                       (cond
                         ((not (gethash neighbor index))
                          (tarjan neighbor)
                          (setf (gethash node lowlink)
                                (min (gethash node lowlink)
                                     (gethash neighbor lowlink))))
                         ((member neighbor stack)
                          (setf (gethash node lowlink)
                                (min (gethash node lowlink)
                                     (gethash neighbor index))))))
                     (neighbors graph node))
               ;; is NODE the root of a strongly connected component
               (when (= (gethash node index) (gethash node lowlink))
                 (push (loop :for v = (pop stack) :collect v :until (eq v node))
                       sccs))))
      (mapc (lambda (node) (unless (gethash node index) (tarjan node)))
            (nodes graph))
      sccs)))

(defmethod cycles-from ((graph graph) node)
  "Return all cycles in GRAPH containing NODE."
  (let* ((paths (mapcar #'list (node-edges graph node)))
         (seen (node-edges graph node))
         cycles)
    (loop :until (null paths) :do
       (format t "paths:~S~%" paths)
       (let ((path (pop paths)))
         (mapc (lambda (edge)
                 (if (member node edge :test #'tree-equal)
                     ;; pushing everything here...
                     (push (reverse (cons edge path)) cycles)
                     (unless (member edge seen)
                       (push edge seen)
                       (push (cons edge path) paths))))
               (remove-if (lambda (edge) (member edge path :test #'tree-equal))
                          (edge-neighbors graph (car path))))))
    cycles))

(defmethod cycles ((graph graph))
  "Return all directed `cycles' in GRAPH."
  (cycle- graph nil nil nil))

(defmethod cycle-connected-components
    ((graph graph) &optional (cycles (cycles graph)))
  "Return the groups of nodes of GRAPH which are connected by cycles."
  (mapcar
   {remove-duplicates _ :test #'tree-equal}
   (reduce (lambda (acc cycle)
             (let ((has {intersection cycle}))
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
                  (half (connected-component residual from)))
             (list half (set-difference (nodes graph) half)))))
    (if (= (length (nodes graph)) 2)
        (progn
          (format t "base: ~S~%" (edges-w-values graph))
          (values (mapcar #'list (nodes graph))
                  (reduce (lambda (acc edge) (+ (abs (edge-value graph edge)) acc))
                          (remove-if-not {subsetp (nodes graph)} (edges graph))
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
