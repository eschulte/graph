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


;;; Hashes keyed on equality for edges (sets)
(defun symbol< (symbol1 symbol2)
  (string< (symbol-name symbol1) (symbol-name symbol2)))

(defun edge-hash (edge)
  (sxhash (sort (copy-tree edge)
                (cond
                  ((symbolp (car edge)) #'symbol<)
                  ((stringp (car edge)) #'string<)
                  ((numberp (car edge)) #'<)
                  (t        (lambda (a b) (string< (format nil "~a" a)
                                              (format nil "~a" b))))))))

(defun edge-equal (edge1 edge2) (set-equal edge1 edge2))
#+sbcl
(sb-ext:define-hash-table-test edge-equal edge-hash)

(defun make-edge-hash ()
  #+sbcl
  (make-hash-table :test 'edge-equal)
  #+ecl
  (make-custom-hash-table :test 'edge-equal :hash-function 'edge-hash)
  #-(or sbcl ecl)
  (error "unsupported lisp distribution"))


;;; Graph objects and basic methods
(defclass graph ()
  ((node-t :initarg :node-t :accessor node-t :initform 'symbol)
   (edge-t :initarg :edge-t :accessor edge-t :initform 'number)
   (node-h :initarg :node-h :accessor node-h :initform (make-hash-table))
   (edge-h :initarg :edge-h :accessor edge-h
           :initform (make-hash-table :test 'edge-equal))
   (test   :initarg :test   :accessor test   :initform #'eql)
   (edge-comb :initarg :edge-comb :accessor edge-comb :initform nil)))

;; TODO: this should use normal hashes for the edge hash
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
    :edge-comb (edge-comb graph)))

(defmethod graph-of ((digraph digraph))
  (make-instance 'digraph
    :node-h    (node-h digraph)
    :edge-h    (edge-h digraph)
    :test      (test digraph)
    :edge-comb (edge-comb digraph)))

(defmethod edges ((graph graph))
  "Return a list of the edges in GRAPH."
  (loop :for key :being :each :hash-key :of (edge-h graph) :collect key))

(defmethod (setf edges) (new (graph graph))
  "Set the edges in GRAPH to NEW."
  (mapc {delete-edge graph} (set-difference (edges graph) new :test 'set-equal))
  (mapc {add-edge graph} (set-difference new (edges graph) :test 'set-equal))
  (edges graph))

(defmethod edges-w-values ((graph graph) &aux alist)
  "Return an alist of edges of GRAPH with their values."
  (maphash (lambda (edge value) (push (cons edge value) alist)) (edge-h graph))
  alist)

(defmethod nodes ((graph graph))
  "Return a list of the nodes in GRAPH."
  (loop :for key :being :each :hash-key :of (node-h graph) :collect key))

(defmethod (setf nodes) (new (graph graph))
  "Set the nodes in GRAPH to NEW."
  (mapc {delete-node graph} (set-difference (nodes graph) new))
  (mapc {add-node graph} (set-difference new (nodes graph)))
  (nodes graph))

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
    (assert included (node graph) "~S doesn't include ~S" graph node)
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
    (assert included (edge graph) "~S doesn't include ~S" graph edge)
    value))

(defmethod (setf edge-value) (new (graph graph) edge)
  "Set the value of EDGE in GRAPH to NEW."
  (setf (gethash edge (edge-h graph)) new))

(defmethod delete-edge ((graph graph) edge)
  "Delete EDGE from GRAPH.
Return the old value of EDGE."
  (prog1 (edge-value graph edge)
    (mapc (lambda (node) (setf (gethash node (node-h graph))
                          (remove edge (gethash node (node-h graph))
                                  :test 'set-equal)))
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
(defmethod merge-nodes ((graph graph) node1 node2
                        &key (edge-comb (edge-comb graph)) (new node1))
  "Combine NODE1 and NODE2 in GRAPH into the node NEW.
All edges of NODE1 and NODE2 in GRAPH will be combined into a new node
holding VALUE.  Edges between only NODE1 and NODE2 will be removed."
  ;; replace all removed edges with NEW instead of NODE1 or NODE2
  (mapcar
   (lambda-bind ((edge . value))
     (let ((e (mapcar (lambda (n) (if (member n (list node1 node2)) new n)) edge)))
       (if (has-edge-p graph e)
           (when edge-comb
             (setf (edge-value graph e)
                   (funcall edge-comb (edge-value graph e) value)))
           (add-edge graph e value))))
   ;; drop edges between only node1 and node2
   (remove-if-not [{set-difference _ (list node1 node2)} #'car]
                  ;; delete both nodes keeping their edges and values
                  (prog1 (append (delete-node graph node1)
                                 (delete-node graph node2))
                    ;; add the new node
                    (add-node graph new))))
  graph)

(defmethod merge-edges ((graph graph) edge1 edge2
                        &key value (edge-comb (edge-comb graph)))
  "Combine EDGE1 and EDGE2 in GRAPH into a new EDGE.
Optionally provide a value for the new edge, otherwise if `edge-comb'
is defined for GRAPH it will be used or no value will be assigned."
  (add-edge graph (remove-duplicates (append edge1 edge2))
            (or value
                (when edge-comb
                  (funcall edge-comb
                           (edge-value graph edge1)
                           (edge-value graph edge2)))))
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
            (nodes graph)))
    sccs))

(defmethod basic-cycles ((graph graph))
  "Return all basic in GRAPH."
  (let (cycles seen)
    (labels ((follow (node path used-edges)
               (push node seen)
               (dolist (edge (node-edges graph node))
                 (unless (member edge used-edges :test #'tree-equal)
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

(defmethod max-flow ((graph graph) from to)
  "Return the maximum flow from FROM and TO in GRAPH.
GRAPHS must be a network with numeric values of all edges.
The Ford-Fulkerson algorithm is used."
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
         (setf residual (residual graph flow))
         ;; "augmenting path" is path through residual network in which each
         ;; edge has positive capacity
         (setf augment (trim-path
                        (mapcar (lambda (edge)
                                  (cons edge (edge-value residual edge)))
                                (shortest-path residual from to))))
         :while augment :do
         ;; if ∃ an augmenting path, add it to the flow and repeat
         (setf flow (add-paths flow augment)))
      (values flow (flow-value-into flow to)))))


;;; Min Cut
;;
;; Stoer, M. and Wagner, Frank. 1997. A Simple Min-Cut Algorithm.
;; Journal of the ACM
;;
(defmethod min-s-t-cut ((graph graph))
  "Return two arbitrary nodes in G and the minimum cut between them.
Use \"maximum carnality search\" aka \"maximum adjacency search\"."
  (let ((g (copy graph)) cuts-of-phase)
    (flet ((connection-weight (group node)
             ;; return the weight of edges between GROUP and NODE
             (reduce #'+ (mapcar {edge-value g}
                                 (remove-if-not {intersection group}
                                                (node-edges g node))))))
      (loop :while (> (length (nodes g)) 1) :do
         (let* ((a (list (random-elt (nodes g))))
                (rest (remove (car a) (nodes g))))
           (loop :while rest :do
              ;; grow A by adding the node most tightly connected to A
              (let ((new (car (sort rest #'> :key {connection-weight a}))))
                (setf rest (remove new rest))
                (push new a)))
           (assert (set-equal (nodes g) a) ((nodes g) a)
                   "at this point all nodes of g~S should be in a~S"
                   (nodes g) a)
           ;; store the cut-of-phase
           (format t "g~S a~S w~S~%"
                   (edges-w-values g) a (connection-weight (cdr a) (car a)))
           (push (cons (connection-weight (cdr a) (car a)) (subseq a 0 2))
                 cuts-of-phase)
           ;; merge two last added nodes
           (mapc {delete-edge g} (intersection (node-edges g (first a))
                                               (node-edges g (second a))
                                               :test #'tree-equal))
           (merge-nodes g (first a) (second a) :edge-comb #'+)))
      ;; return the minimum cut-of-phase
      (let ((weight-and-cut (car (sort cuts-of-phase #'< :key #'car))))
        (values (cdr weight-and-cut) (car weight-and-cut))))))

;; Theorem: Let s,t ∈ (nodes G), let G' be the result of merging s and
;;          t in G.  Then (min-cut G) is equal to the minimum of the
;;          min cut of s and t in G and (min-cut G').
(defmethod min-cut ((graph graph))
  "Return the global min-cut of GRAPH with the weight of the cut."
  (format t "min-cut ~S~%" (edges-w-values graph))
  (if (<= (length (nodes graph)) 2)
      (progn
        (assert (= (length (nodes graph)) 2) (graph) "%S has <2 nodes" graph)
        (values (nodes graph)
                (reduce #'+ (mapcar {edge-value graph} (edges graph)))))
      (multiple-value-bind (cut1 weight1) (min-s-t-cut graph)
        (format t "cut1 ~S -> weight1 ~S ~%" cut1 weight1)
        (multiple-value-bind (cut2 weight2)
            (min-cut (merge-nodes (copy graph) (first cut1) (second cut1)))
          (if (< weight1 weight2)
              (values cut1 weight1)
              (values cut2 weight2))))))


;;; Visualization
(defun edge-to-dot (graph edge)
  (concatenate 'string
    (if (eq 'digraph (type-of graph))
        (apply #'format nil "  \"~a\" -> \"~a\"" edge)
        (apply #'format nil "  \"~a\" -- \"~a\"" edge))
    (if (edge-value graph edge)
        (format nil "[label=\"~a\"];~%" (edge-value graph edge))
        ";")))

(defmethod to-dot ((graph graph) &optional (stream t))
  "Print the dot code representing GRAPH."
  (format stream "graph to_dot {~%")
  (mapc {format stream "  \"~a\";~%"} (nodes graph))
  (mapc [{format stream "~a"} {edge-to-dot graph}] (edges graph))
  (format stream "}~%"))

(defmethod to-dot ((digraph digraph) &optional (stream t))
  "Print the dot code representing DIGRAPH."
  (format stream "digraph to_dot {~%")
  (mapc {format stream "  \"~a\";~%"} (nodes digraph))
  (mapc [{format stream "~a"} {edge-to-dot digraph}] (edges digraph))
  (format stream "}~%"))
