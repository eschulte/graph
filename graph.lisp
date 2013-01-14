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

;;; Code:
(in-package :graph)


;;; Reader macros
(eval-when (:compile-toplevel :load-toplevel :execute)
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
  (set-macro-character #\] (get-macro-character #\) )))


;;; Special hashes keyed for edges
(defun edge-equalp (edge1 edge2)
  (set-equal edge1 edge2))

(defun sxhash-edge (edge)
  (sxhash (sort (copy-tree edge) (if (numberp edge) #'< #'string<))))

(sb-ext:define-hash-table-test edge-equalp sxhash-edge)

(defun dir-edge-equalp (edge1 edge2)
  (tree-equal edge1 edge2))

(sb-ext:define-hash-table-test dir-edge-equalp sxhash)

(defun make-edge-hash-table ()
  #+sbcl
  (make-hash-table :test 'edge-equalp)
  #-(or sbcl)
  (error "unsupport lisp distribution"))

(defun make-diedge-hash-table ()
  #+sbcl
  (make-hash-table :test 'dir-edge-equalp)
  #-(or sbcl)
  (error "unsupport lisp distribution"))


;;; Graph objects and basic methods
(defclass graph ()
  ((node-h :initarg :node-h :accessor node-h :initform (make-hash-table))
   (edge-h :initarg :edge-h :accessor edge-h :initform (make-edge-hash-table))
   (edge-eq :initarg :edge-eq :accessor edge-eq :initform 'edge-equalp))
  (:documentation "A graph consisting of `nodes' connected by `edges'.
Nodes must be numbers symbols or keywords.  Edges may be assigned
arbitrary values, although some functions assume numeric values (e.g.,
`merge-nodes', `merge-edges', `max-flow' and `min-cut')."))

(defclass digraph (graph)
  ((edge-h :initarg :edge-h :accessor edge-h :initform (make-diedge-hash-table))
   (edge-eq :initarg :edge-eq :accessor edge-eq :initform 'dir-edge-equalp))
  (:documentation "A `graph' with directed edges."))

(defun copy-hash (hash &optional test comb)
  "Return a copy of HASH.
Optional argument TEST specifies a new equality test to use for the
copy.  Second optional argument COMB specifies a function to use to
combine the values of elements of HASH which collide in the copy due
to a new equality test specified with TEST."
  (let ((copy (make-hash-table :test (or test (hash-table-test hash)))))
    (maphash (lambda (k v) (setf (gethash k copy)
                            (if (and (gethash k copy) comb)
                                (funcall comb (gethash k copy) v)
                                v)))
             hash)
    copy))

(defun hash-equal (hash1 hash2)
  "Test HASH1 and HASH2 for equality."
  (tree-equal (hash-table-plist hash1)
              (hash-table-plist hash2) :test 'equalp))

(defgeneric copy (graph)
  (:documentation "Return a copy of GRAPH."))

(defmethod copy ((graph graph))
  (make-instance (type-of graph)
    :node-h    (copy-hash (node-h graph))
    :edge-h    (copy-hash (edge-h graph))
    :edge-eq   (edge-eq graph)))

(defgeneric digraph-of (graph)
  (:documentation "Copy GRAPH into a `digraph' and return."))

(defmethod digraph-of ((graph graph))
  (make-instance 'digraph
    :node-h    (copy-hash (node-h graph))
    :edge-h    (copy-hash (edge-h graph))
    :edge-eq   (edge-eq graph)))

(defgeneric graph-of (digraph)
  (:documentation "Copy DIGRAPH into a `graph' and return."))

(defmethod graph-of ((digraph digraph))
  (make-instance 'graph
    :node-h    (copy-hash (node-h digraph))
    :edge-h    (copy-hash (edge-h digraph) 'equalp)
    :edge-eq   (edge-eq digraph)))

(defgeneric populate (graph &key nodes edges edges-w-values)
  (:documentation
   "Populate the nodes and edges of GRAPH based on keyword arguments."))

(defmethod populate ((graph graph) &key nodes edges edges-w-values)
  (mapc {add-node graph} nodes)
  (mapc {add-edge graph} edges)
  (mapc (lambda-bind ((edge . value)) (add-edge graph edge value)) edges-w-values)
  graph)

(defgeneric graph-equal (graph1 graph2)
  (:documentation "Compare GRAPH1 and GRAPH2 for equality."))

(defmethod graph-equal ((graph1 graph) (graph2 graph))
  (every (lambda-bind ((test key))
           (apply test (append (mapcar key (list graph1 graph2)))))
         '((eq         type-of)
           (equal      edge-eq)
           (hash-equal edge-h)
           (hash-equal node-h))))


;;; Simple graph methods
(defgeneric edges (graph)
  (:documentation "Return a list of the edges in GRAPH."))

(defmethod edges ((graph graph))
  (loop :for key :being :each :hash-key :of (edge-h graph) :collect key))

(defgeneric (setf edges) (new graph)
  (:documentation "Set the edges in GRAPH to NEW."))

(defmethod (setf edges) (new (graph graph))
  (mapc {delete-edge graph} (set-difference (edges graph) new
                                            :test (edge-eq graph)))
  (mapc {add-edge graph} (set-difference new (edges graph)
                                         :test (edge-eq graph)))
  (edges graph))

(defgeneric edges-w-values (graph)
  (:documentation "Return an alist of edges of GRAPH with their values."))

(defmethod edges-w-values ((graph graph) &aux alist)
  (maphash (lambda (edge value) (push (cons edge value) alist)) (edge-h graph))
  alist)

(defgeneric nodes (graph)
  (:documentation "Return a list of the nodes in GRAPH."))

(defmethod nodes ((graph graph))
  (loop :for key :being :each :hash-key :of (node-h graph) :collect key))

(defgeneric (setf nodes) (new graph)
  (:documentation "Set the nodes in GRAPH to NEW."))

(defmethod (setf nodes) (new (graph graph))
  (mapc {delete-node graph} (set-difference (nodes graph) new))
  (mapc {add-node graph} (set-difference new (nodes graph)))
  (nodes graph))

(defgeneric nodes-w-values (graph)
  (:documentation "Return an alist of nodes of GRAPH with their values."))

(defmethod nodes-w-values ((graph graph) &aux alist)
  (maphash (lambda (node value) (push (cons node value) alist)) (node-h graph))
  alist)

(defgeneric has-node-p (graph node)
  (:documentation "Return `true' if GRAPH has node NODE."))

(defmethod has-node-p ((graph graph) node)
  (multiple-value-bind (value included) (gethash node (node-h graph))
    (declare (ignorable value)) included))

(defgeneric has-edge-p (graph edge)
  (:documentation "Return `true' if GRAPH has edge EDGE."))

(defmethod has-edge-p ((graph graph) edge)
  (multiple-value-bind (value included) (gethash edge (edge-h graph))
    (declare (ignorable value)) included))

(defgeneric subgraph (graph nodes)
  (:documentation "Return the subgraph of GRAPH restricted to NODES."))

(defmethod subgraph ((graph graph) nodes)
  (let ((g (copy graph))) (setf (nodes g) nodes) g))

(defgeneric add-node (graph node)
  (:documentation "Add NODE to GRAPH."))

(defmethod add-node ((graph graph) node)
  (assert (or (numberp node) (symbolp node)) (node)
          "Nodes must be numbers, symbols or keywords, not ~S.~%Invalid node:~S"
   (type-of node) node)
  (unless (has-node-p graph node)
    (setf (gethash node (node-h graph)) nil)
    node))

(defgeneric add-edge (graph edge &optional value)
  (:documentation "Add EDGE to GRAPH with optional VALUE."))

(defmethod add-edge ((graph graph) edge &optional value)
  (mapc (lambda (node)
          (add-node graph node)
          (pushnew (case (type-of graph)
                     (graph (remove-duplicates edge))
                     (digraph edge))
                   (gethash node (node-h graph))))
        edge)
  (setf (gethash edge (edge-h graph)) value)
  edge)

(defgeneric node-edges (graph node)
  (:documentation "Return the value of NODE in GRAPH."))

(defmethod node-edges ((graph graph) node)
  (multiple-value-bind (edges included) (gethash node (node-h graph))
    (assert included (node graph) "~S doesn't include ~S" graph node)
    (copy-tree edges)))

(defgeneric degree (graph node)
  (:documentation "Return the degree of NODE in GRAPH."))

(defmethod degree ((graph graph) node)
  (length (node-edges graph node)))

(defgeneric (setf node-edges) (new graph node)
  (:documentation "Set the edges of NODE in GRAPH to NEW.
Delete and return the old edges of NODE in GRAPH."))

(defmethod (setf node-edges) (new (graph graph) node)
  (prog1 (mapc {delete-edge graph} (gethash node (node-h graph)))
    (mapc {add-edge graph} new)))

(defgeneric delete-node (graph node)
  (:documentation "Delete NODE from GRAPH.
Delete and return the old edges of NODE in GRAPH."))

(defmethod delete-node ((graph graph) node)
  (prog1 (mapcar (lambda (edge) (cons edge (delete-edge graph edge)))
                 (node-edges graph node))
    (remhash node (node-h graph))))

(defgeneric edge-value (graph edge)
  (:documentation "Return the value of EDGE in GRAPH."))

(defmethod edge-value ((graph graph) edge)
  (multiple-value-bind (value included) (gethash edge (edge-h graph))
    (assert included (edge graph) "~S doesn't include ~S" graph edge)
    value))

(defgeneric (setf edge-value) (new graph edge)
  (:documentation "Set the value of EDGE in GRAPH to NEW."))

(defmethod (setf edge-value) (new (graph graph) edge)
  (setf (gethash edge (edge-h graph)) new))

(defgeneric delete-edge (graph edge)
  (:documentation "Delete EDGE from GRAPH.
Return the old value of EDGE."))

(defmethod delete-edge ((graph graph) edge)
  (prog1 (edge-value graph edge)
    (mapc (lambda (node) (setf (gethash node (node-h graph))
                          (remove edge (gethash node (node-h graph))
                                  :test (edge-eq graph))))
          edge)
    (remhash edge (edge-h graph))))


;;; Complex graph methods
(defgeneric merge-nodes (graph node1 node2 &key new)
  (:documentation "Combine NODE1 and NODE2 in GRAPH into the node NEW.
All edges of NODE1 and NODE2 in GRAPH will be combined into a new node
holding VALUE.  Edges between only NODE1 and NODE2 will be removed."))

(defmethod merge-nodes ((graph graph) node1 node2 &key (new node1))
  ;; replace all removed edges with NEW instead of NODE1 or NODE2
  (mapcar
   (lambda-bind ((edge . value))
     (let ((e (mapcar (lambda (n) (if (member n (list node1 node2)) new n)) edge)))
       (if (has-edge-p graph e)
           (when (and (edge-value graph e) value)
             (setf (edge-value graph e) (+ (edge-value graph e) value)))
           (add-edge graph e value))))
   ;; drop edges between only node1 and node2
   (remove-if-not [{set-difference _ (list node1 node2)} #'car]
                  ;; delete both nodes keeping their edges and values
                  (prog1 (append (delete-node graph node1)
                                 (delete-node graph node2))
                    ;; add the new node
                    (add-node graph new))))
  graph)

(defgeneric merge-edges (graph edge1 edge2 &key value)
  (:documentation "Combine EDGE1 and EDGE2 in GRAPH into a new EDGE.
Optionally provide a value for the new edge, the values of EDGE1 and
EDGE2 will be combined."))

(defmethod merge-edges ((graph graph) edge1 edge2 &key value)
  (add-edge graph (remove-duplicates (append edge1 edge2))
            (or value
                (when (and (edge-value graph edge1) (edge-value graph edge2))
                  (+ (edge-value graph edge1) (edge-value graph edge2)))))
  (append (delete-edge graph edge1)
          (delete-edge graph edge2)))

(defgeneric edge-neighbors (graph edge)
  (:documentation "Return all edges which share a node with EDGE in GRAPH."))

(defmethod edge-neighbors ((graph graph) edge)
  (mapcan {node-edges graph} edge))

(defgeneric neighbors (graph node)
  (:documentation "Return all nodes which share an edge with NODE in GRAPH."))

(defmethod neighbors ((graph graph) node)
  (apply {concatenate 'list} (node-edges graph node)))

(defmethod neighbors ((digraph digraph) node)
  (mapcan [#'cdr {member node}] (node-edges digraph node)))

(defgeneric precedents (digraph node)
  (:documentation "Return all nodes preceding NODE in an edge of DIGRAPH."))

(defmethod precedents ((digraph digraph) node)
  (mapcan [#'cdr {member node} #'reverse] (node-edges digraph node)))

(defgeneric connected-component (graph node)
  (:documentation "Return the connected component of NODE in GRAPH."))

(defmethod connected-component ((graph graph) node)
  (let ((from (list node)) (seen))
    (loop :until (null from) :do
       (let ((next (remove-duplicates (mapcan {neighbors graph} from))))
         (setf from (remove node (set-difference next seen)))
         (setf seen (union next seen))))
    (reverse seen)))

(defgeneric connectedp (graph)
  (:documentation "Return true if the graph is connected."))

(defmethod connectedp ((graph graph))
  (let ((nodes (nodes graph)))
    (subsetp (nodes graph) (connected-component graph (car nodes)))))

(defgeneric connected-components (graph)
  (:documentation "Return a list of the connected components of GRAPH."))

(defmethod connected-components ((graph graph))
  (let ((nodes (sort (nodes graph) #'< :key {degree graph})) ccs)
    (loop :until (null nodes) :do
       (let ((cc (connected-component graph (car nodes))))
         (setf nodes (set-difference nodes cc))
         (push cc ccs)))
    ccs))


;;; Cycles and strongly connected components
(defgeneric strongly-connected-components (graph)
  (:documentation
   "Return the nodes of GRAPH partitioned into strongly connected components.
Uses Tarjan's algorithm."))

(defmethod strongly-connected-components ((graph graph))
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
            (nodes graph)))
    sccs))

(defgeneric basic-cycles (graph)
  (:documentation "Return all basic cycles in the GRAPH."))

(defmethod basic-cycles ((graph graph))
  (let (cycles seen)
    (labels ((follow (node path used-edges)
               (push node seen)
               (dolist (edge (node-edges graph node))
                 (unless (member edge used-edges :test (edge-eq graph))
                   (dolist (neighbor (remove node edge))
                     (cond ((member neighbor path)
                            (push (subseq path 0 (1+ (position neighbor path)))
                                  cycles))
                           ((not (member neighbor seen))
                            (follow neighbor
                                    (cons neighbor path)
                                    (cons edge used-edges)))))))))
      (dolist (node (nodes graph))
        (unless (member node seen)
          (follow node (list node) nil))))
    cycles))

(defgeneric cycles (graph)
  (:documentation "Return all cycles of GRAPH (both basic and compound)."))

(defmethod cycles ((graph graph))
  (flet ((combine (c1 c2)
           (let (done)
             (reduce (lambda (acc el)
                       (append
                        (if (and (not done) (member el c1))
                            (progn
                              (setf done t)
                              (append (member el c1)
                                      (reverse (member el (reverse c1)))))
                            (list el))
                        acc))
                     c2 :initial-value nil))))
    (let ((basic-cycles (basic-cycles graph)) cycles)
      (loop :for cycle = (pop basic-cycles) :do
         (push cycle cycles)
         (mapc (lambda (c) (push (combine c cycle) cycles))
               (remove-if-not {intersection cycle} basic-cycles))
         :until (null basic-cycles))
      cycles)))


;;; Shortest Path
(defgeneric shortest-path (graph a b)
  (:documentation "Return the shortest path in GRAPH from A to B.
GRAPH must be a directed graph.  Dijkstra's algorithm is used."))

(defmethod shortest-path ((graph graph) a b &aux seen)
  (block nil ;; (car next) is leading node, (cdr next) is edge path
    (let ((next (list (list a))))
      (loop :until (null next) :do
         (setf next
               (mapcan
                (lambda-bind ((from . rest))
                  (mapcan
                   (lambda (edge)
                     (if (member b (cdr (member from edge)))
                         (return (reverse (cons edge rest)))
                         (unless (member edge seen :test (edge-eq graph))
                           (push edge seen)
                           (mapcar
                            (lambda (n) (cons n (cons edge rest)))
                            (case (type-of graph)
                              (graph (remove from edge))
                              (digraph (cdr (member from edge))))))))
                   (node-edges graph from)))
                next))))))


;;; Max Flow
;; - Must be a "network" (digraph in which each edge has a positive weight)
;; - Ford-Fulkerson is used
(defgeneric residual (graph flow)
  (:documentation "Return the residual graph of GRAPH with FLOW.
Each edge in the residual has a value equal to the original capacity
minus the current flow, or equal to the negative of the current flow."))

(defmethod residual ((graph graph) flow)
  (flet ((flow-value (edge) (or (cdr (assoc edge flow :test (edge-eq graph))) 0)))
    (let ((residual (make-instance (type-of graph))))
      (mapc (lambda (edge)
              (let ((left (- (edge-value graph edge) (flow-value edge))))
                (when (not (zerop left))
                  (add-edge residual edge left)))
              (when (not (zerop (flow-value edge)))
                (add-edge residual (reverse edge) (flow-value edge))))
            (edges graph))
      residual)))

(defgeneric add-paths (graph path1 path2)
  (:documentation
   "Return the combination of paths PATH1 and PATH2 through GRAPH.
Each element of PATH has the form (cons edge value)."))

(defmethod add-paths ((graph graph) path1 path2)
  (let ((comb (copy-tree path1)))
    (mapc (lambda-bind ((edge . value))
            (if (assoc edge comb :test (edge-eq graph))
                (setf (cdr (assoc edge comb :test (edge-eq graph)))
                      (+ (cdr (assoc edge comb :test (edge-eq graph))) value))
                (push (cons edge value) comb)))
          path2)
    comb))

(defmethod add-paths ((digraph digraph) path1 path2)
  "Return the combination of paths PATH1 and PATH2 through DIGRAPH.
Each element of path has the form (cons edge value)."
  (let ((comb (copy-tree path1)))
    (mapc (lambda-bind ((edge . value))
            (cond
              ((assoc edge comb :test (edge-eq digraph))
               (setf (cdr (assoc edge comb :test (edge-eq digraph)))
                     (+ (cdr (assoc edge comb :test (edge-eq digraph))) value)))
              ((assoc (reverse edge) comb :test (edge-eq digraph))
               (setf (cdr (assoc (reverse edge) comb :test (edge-eq digraph)))
                     (- (cdr (assoc edge comb :test (edge-eq digraph))) value)))
              (t (push (cons edge value) comb))))
          path2)
    comb))

(defgeneric max-flow (graph from to)
  (:documentation "Return the maximum flow from FROM and TO in GRAPH.
GRAPHS must be a network with numeric values of all edges.
The Ford-Fulkerson algorithm is used."))

(defmethod max-flow ((digraph digraph) from to)
  (flet ((trim-path (path)
           (when path
             (let ((flow (apply #'min (mapcar #'cdr path))))
               (mapcar (lambda (el) (cons (car el) flow)) path))))
         (flow-value-into (flow node)
           (reduce #'+ (remove-if-not (lambda (el) (equal (lastcar (car el)) node))
                                      flow)
                   :key #'cdr)))
    (let ((from from) (to to) augment residual flow)
      (loop :do
         (setf residual (residual digraph flow))
         ;; "augmenting path" is path through residual network in which each
         ;; edge has positive capacity
         (setf augment (trim-path
                        (mapcar (lambda (edge)
                                  (cons edge (edge-value residual edge)))
                                (shortest-path residual from to))))
         :while augment :do
         ;; if ∃ an augmenting path, add it to the flow and repeat
         (setf flow (add-paths digraph flow augment)))
      (values flow (flow-value-into flow to)))))


;;; Min Cut
;;
;; Stoer, M. and Wagner, Frank. 1997. A Simple Min-Cut Algorithm.
;; Journal of the ACM
;;
;; Theorem: Let s,t ∈ (nodes G), let G' be the result of merging s and
;;          t in G.  Then (min-cut G) is equal to the minimum of the
;;          min cut of s and t in G and (min-cut G').
;;          
(defgeneric min-cut (graph)
  (:documentation
   "Return both the global min-cut of GRAPH and the weight of the cut."))

(defmethod min-cut ((graph graph))
  (let ((g (copy graph))
        (merged-nodes (mapcar (lambda (n) (list n n)) (nodes graph)))
        cuts-of-phase)
    (flet ((connection-weight (group node)
             ;; return the weight of edges between GROUP and NODE
             (reduce #'+ (mapcar {edge-value g}
                                 (remove-if-not {intersection group}
                                                (node-edges g node)))))
           (my-merge (a b)
             ;; merge in the graph
             (merge-nodes g a b)
             ;; update our merged nodes alist
             (setf (cdr (assoc a merged-nodes))
                   (append (cdr (assoc a merged-nodes))
                           (cdr (assoc b merged-nodes))))
             (setq merged-nodes
                   (remove-if (lambda (it) (eql (car it) b)) merged-nodes))))
      (loop :while (> (length (nodes g)) 1) :do
         (let* ((a (list (random-elt (nodes g))))
                (rest (remove (car a) (nodes g))))
           (loop :while rest :do
              ;; grow A by adding the node most tightly connected to A
              (let ((new (car (sort rest #'> :key {connection-weight a}))))
                (setf rest (remove new rest))
                (push new a)))
           ;; store the cut-of-phase
           (push (cons (connection-weight (cdr a) (car a))
                       (cdr (assoc (car a) merged-nodes)))
                 cuts-of-phase)
           ;; merge two last added nodes
           (my-merge (first a) (second a))))
      ;; return the minimum cut-of-phase
      (let ((weight-and-cut (car (sort cuts-of-phase #'< :key #'car))))
        (values (list (cdr weight-and-cut)
                      (set-difference (nodes graph) (cdr weight-and-cut)))
                (car weight-and-cut))))))


;;; Serialize graphs to/from plists
(defgeneric to-plist (graph)
  (:documentation "Serialize GRAPH as a plist."))

(defmethod to-plist ((graph graph))
  (let ((counts (make-hash-table)) (counter -1))
    (list :nodes (mapcar {list :name}
                         (mapc (lambda (n) (setf (gethash n counts) (incf counter)))
                               (nodes graph)))
          :edges (map 'list (lambda (edge value) (list :edge edge :value value))
                      (mapcar {mapcar {position _ (nodes graph)}} (edges graph))
                      (mapcar {edge-value graph} (edges graph))))))

(defgeneric from-plist (graph plist)
  (:documentation "Populate GRAPH with the contents of PLIST."))

(defmethod from-plist ((graph graph) plist)
  (let ((nodes (map 'vector {getf _ :name} (getf plist :nodes))))
    (populate graph
      :nodes (coerce nodes 'list)
      :edges-w-values (mapcar (lambda (el)
                                (cons (mapcar {aref nodes} (getf el :edge))
                                      (getf el :value)))
                              (getf plist :edges)))))
