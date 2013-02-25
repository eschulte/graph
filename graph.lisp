;;; graph.lisp --- because its easier to write than to learn such a library

;; Copyright (C) Eric Schulte and Thomas Dye 2012-2013

;; Licensed under the Gnu Public License Version 3 or later

;;; Commentary

;; Graphs are composed of two hash tables, nodes and edges.  The node
;; hash is keyed by node and holds the edges containing that node,
;; while the edge hash is keyed by edge containing any optional edge
;; value.
;;
;;                              Nodes                  Edges
;;                             -------                -------
;;     +----Graph G-----+     key |  value             key | value
;;     |   3   2        |    -----+---------        -------+----------
;;     | a---b---c   g  |       a |  (a b)           (a b) |  3
;;     |    1|   |1     |       b |  (b d) (b c)     (b d) |  1
;;     |     d---e---f  |       c |  (b c) (c e)     (b c) |  2
;;     |       2   3    |       d |  (b d) (d e)     (c e) |  1
;;     +----------------+       e |  (d e) (c e)     (d e) |  2
;;                              f |  (e f)           (e f) |  3
;;                              g |
;;
;; Graphs are CLOS objects which are constructed with the usual `make
;; instance` and are populated with the `populate` function.
;;
;;     (defvar *graph* (populate (make-instance 'graph)
;;                       :nodes '(a b c d e f g)
;;                       :edges-w-values '(((a b) . 3)
;;                                         ((b d) . 1)
;;                                         ((b c) . 2)
;;                                         ((c e) . 1)
;;                                         ((d e) . 2)
;;                                         ((e f) . 3))))
;;
;; Standard accessors are provided.
;;
;;     * (nodes *graph*)
;;     (A B C D E F G)
;;
;;     * (edges *graph*)
;;     ((A B) (B D) (B C) (C E) (D E) (E F))
;;
;;     * (node-edges *graph* 'b)
;;     ((B C) (B D) (A B))
;;
;;     * (edge-value *graph* '(d e))
;;     2
;;
;; Nodes and edges may be removed using `delete-node` and
;; `delete-edge`, or using setf methods on any of the accessors above.
;;
;;     * (delete-edge *graph* '(e f))
;;     3
;;
;;     * (edges *graph*)
;;     ((A B) (B D) (B C) (C E) (D E))
;;
;;     * (setf (nodes *graph*) (remove 'a (nodes *graph*)))
;;     (B C D E F G)
;;
;;     * (edges *graph*)
;;     ((B D) (B C) (C E) (D E))
;;
;; Some more sophisticated graph algorithms are implemented.  A couple
;; are shown below, see the dictionary for a complete list.
;;
;;     * (shortest-path *graph* 'b 'e)
;;     ((B C) (C E))
;;
;;     * (connected-components *graph*)
;;     ((G) (B D C E) (F))
;;
;;     * (setf (nodes *graph*) '(B D C E))
;;     (B C D E)
;;
;;     * (min-cut *graph*)
;;     ((B C) (E D))
;;     2
;;
;; Additionally digraphs represent graphs with directed edges.
;; Starting with the original graph above we get the following.
;;
;;     * (strongly-connected-components *graph*)
;;     ((G) (D F E C B A))
;;
;;     * (strongly-connected-components (digraph-of *graph*))
;;     ((G) (A) (B) (D) (C) (E) (F))
;;
;;     * (delete-edge *graph* '(d e))
;;     2
;;
;;     * (push '(e d) (edges *graph*))
;;     ((A B) (B D) (B C) (C E) (E D) (E F))
;;
;;     * (push '(d c) (edges *graph*))
;;     ((A B) (B D) (B C) (C E) (E D) (E F) (D C))
;;
;;     * (strongly-connected-components (digraph-of *graph*))
;;     ((G) (A) (B) (D E C) (F))

;;; Code:
(in-package :graph)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))


;;; Special hashes keyed for edges
(defun edge-equalp (edge1 edge2)
  (set-equal edge1 edge2))

(defun sxhash-edge (edge)
  (sxhash (sort (copy-tree edge) (if (numberp (car edge)) #'< #'string<))))

#+sbcl
(sb-ext:define-hash-table-test edge-equalp sxhash-edge)
#+clisp
(ext:define-hash-table-test edge-equalp edge-equalp sxhash-edge)

(defun dir-edge-equalp (edge1 edge2)
  (tree-equal edge1 edge2))

#+sbcl
(sb-ext:define-hash-table-test dir-edge-equalp sxhash)
#+clisp
(ext:define-hash-table-test dir-edge-equalp dir-edge-equalp sxhash)

(defun make-edge-hash-table ()
  #+sbcl
  (make-hash-table :test 'edge-equalp)
  #+clisp
  (make-hash-table :test 'edge-equalp)
  #+ccl
  (make-hash-table :test 'edge-equalp :hash-function 'sxhash-edge)
  #-(or sbcl clisp ccl)
  (error "unsupport lisp distribution"))

(defun make-diedge-hash-table ()
  #+sbcl
  (make-hash-table :test 'dir-edge-equalp)
  #+clisp
  (make-hash-table :test 'dir-edge-equalp)
  #+ccl
  (make-hash-table :test 'dir-edge-equalp :hash-function 'sxhash)
  #-(or sbcl clisp ccl)
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
  (let ((copy
         #+sbcl (make-hash-table :test (or test (hash-table-test hash)))
         #+clisp (make-hash-table :test (or test (hash-table-test hash)))
         #+ccl (make-hash-table
                :test (or test (hash-table-test hash))
                :hash-function (case (or test (hash-table-test hash))
                                 (edge-equalp 'sxhash-edge)
                                 ((dir-edge-equalp equalp) 'sxhash)))
         #-(or sbcl clisp ccl) (error "unsupported lisp distribution")))
    (maphash (lambda (k v) (setf (gethash k copy)
                            (if (and (gethash k copy) comb)
                                (funcall comb (gethash k copy) v)
                                v)))
             hash)
    copy))

(defun node-hash-equal (hash1 hash2)
  "Test node hashes HASH1 and HASH2 for equality."
  (set-equal (hash-table-alist hash1)
             (hash-table-alist hash2)
             :test (lambda (a b)
                     (and (equalp (car a) (car b))
                          (set-equal (cdr a) (cdr b) :test 'tree-equal)))))

(defun edge-hash-equal (hash1 hash2)
  "Test edge hashes HASH1 and HASH2 for equality."
  (set-equal (hash-table-alist hash1)
             (hash-table-alist hash2)
             :test 'equalp))

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
  (setf (edges-w-values graph) edges-w-values)
  graph)

(defgeneric graph-equal (graph1 graph2)
  (:documentation "Compare GRAPH1 and GRAPH2 for equality."))

(defmethod graph-equal ((graph1 graph) (graph2 graph))
  (every (lambda-bind ((test key)) ;; TODO: digraph's need a stricter graph-equal
           (apply test (append (mapcar key (list graph1 graph2)))))
         '((eq         type-of)
           (equal      edge-eq)
           (edge-hash-equal edge-h)
           (node-hash-equal node-h))))


;;; Serialize graphs
(defgeneric to-plist (graph)
  (:documentation "Serialize GRAPH as a plist."))

(defmethod to-plist ((graph graph))
  (let ((counts (make-hash-table)) (counter -1))
    (list :nodes (mapcar {list :name}
                         (mapc (lambda (n) (setf (gethash n counts) (incf counter)))
                               (nodes graph)))
          :edges (map 'list (lambda (edge value) (list :edge edge :value value))
                      (mapcar {mapcar {gethash _ counts}} (edges graph))
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

(defgeneric to-adjacency-matrix (graph)
  (:documentation "Return the adjacency matrix of GRAPH."))

(defmethod to-adjacency-matrix ((graph graph))
  (let ((node-index-hash (make-hash-table))
        (counter -1))
    (mapc (lambda (node) (setf (gethash node node-index-hash) (incf counter)))
          (nodes graph))
    (let ((matrix (make-array (list (1+ counter) (1+ counter))
                              :initial-element 0)))
      (mapc (lambda-bind ((a b))
              (setf (aref matrix
                          (gethash a node-index-hash)
                          (gethash b node-index-hash))
                    1))
            (edges graph))
      matrix)))

(defgeneric to-value-matrix (graph)
  (:documentation "Return the value matrix of GRAPH."))

(defmethod to-value-matrix ((graph graph))
  (let ((node-index-hash (make-hash-table))
        (counter -1))
    (mapc (lambda (node) (setf (gethash node node-index-hash) (incf counter)))
          (nodes graph))
    (let ((matrix (make-array (list (1+ counter) (1+ counter))
                              :initial-element nil)))
      (mapc (lambda-bind (((a b) . value))
              (setf (aref matrix
                          (gethash a node-index-hash)
                          (gethash b node-index-hash))
                    (or value t)))
            (edges-w-values graph))
      matrix)))

(defgeneric from-value-matrix (graph matrix)
  (:documentation "Populate GRAPH from the value matrix MATRIX."))

(defmethod from-value-matrix ((graph graph) matrix)
  (bind (((as bs) (array-dimensions matrix)))
    (assert (= as bs) (matrix) "Value matrix ~S must be square." matrix)
    (loop :for a :below as :do
       (loop :for b :below bs :do
          (when (aref matrix a b)
            (add-edge graph (list a b)
                      (if (eq t (aref matrix a b)) nil (aref matrix a b)))))))
  graph)


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

(defgeneric (setf edges-w-values) (new graph)
    (:documentation "Set the edges of graph to edges and values in NEW."))

(defmethod (setf edges-w-values) (new (graph graph))
  (mapc (lambda-bind ((edge . value)) (add-edge graph edge value)) new))

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
                   (gethash node (node-h graph))
                   :test (edge-eq graph)))
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

(defgeneric indegree (digraph node)
  (:documentation "The number of edges directed to NODE in GRAPH."))

(defmethod indegree ((digraph digraph) node)
  (length (remove-if-not [{member node} #'cdr] (node-edges digraph node))))

(defgeneric outdegree (digraph node)
  (:documentation "The number of edges directed from NODE in DIGRAPH."))

(defmethod outdegree ((digraph digraph) node)
  (length (remove-if-not [{equal node} #'car] (node-edges digraph node))))

(defgeneric (setf node-edges) (new graph node) ;; TODO: seg-faults in clisp
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
  (let ((from (list node)) (seen (list node)))
    (loop :until (null from) :do
       (let ((next (remove-duplicates (mapcan {neighbors graph} from))))
         (setf from (set-difference next seen))
         (setf seen (union next seen))))
    (reverse seen)))

(defgeneric connectedp (graph)
  (:documentation "Return true if the graph is connected."))

(defmethod connectedp ((graph graph))
  (let ((nodes (nodes graph)))
    (subsetp (nodes graph) (connected-component graph (car nodes)))))

(defmethod connectedp ((digraph digraph))
  (every [{subsetp (nodes digraph)} {connected-component digraph}]
         (nodes digraph)))

(defgeneric connected-components (graph)
  (:documentation "Return a list of the connected components of GRAPH."))

(defmethod connected-components ((graph graph))
  (let ((nodes (sort (nodes graph) #'< :key {degree graph})) ccs)
    (loop :until (null nodes) :do
       (let ((cc (connected-component graph (car nodes))))
         (setf nodes (set-difference nodes cc))
         (push cc ccs)))
    ccs))

(defgeneric topological-sort (digraph)
  (:documentation
   "Returns a topologically ordered list of the nodes in DIGRAPH, such
   that, for each edge in DIGRAPH, the start of the edge appears in the
   list before the end of the edge."))

(defmethod topological-sort (digraph)
  (assert (null (basic-cycles digraph)) (digraph)
          "~S has a cycle so no topological sort is possible" digraph)
  (let ((index (make-hash-table))
        stack)
    (labels ((visit (node)
               (mapc (lambda (neighbor)
                       (unless (gethash neighbor index)
                         (visit neighbor)))
                     (neighbors digraph node))
               ;; mark this node
               (setf (gethash node index) 1)
               (push node stack)))
      (mapc (lambda (node) (unless (gethash node index) (visit node)))
            (nodes digraph)))
    stack))

(defgeneric levels (digraph &key alist)
  (:documentation "Assign a positive integer to each node in DIGRAPH,
called its level, where, for each directed edge (a b) the
corresponding integers satisfy a < b. Returns either a hash table
where the nodes are keys and the levels are values, or an association
list of nodes and their levels, along with the number of levels in
DIGRAPH."))

(defmethod levels (digraph &key alist)
  (let ((longest (make-hash-table))
        ret
        (max-levels 0))
    (dolist (x (topological-sort digraph))
      (let ((max-val 0)
            (incoming (precedents digraph x)))
        (if incoming
            (progn
              (dolist (y incoming)
                (when (> (gethash y longest) max-val)
                  (setf max-val (gethash y longest))))
              (setf (gethash x longest) (+ 1 max-val))
              (and (> (+ 1 max-val) max-levels)
                   (setf max-levels (+ 1 max-val))))
            (setf (gethash x longest) max-val))))
    (if alist
        (progn
          (maphash (lambda (k v)
                     (push (cons k v) ret))
                   longest)
          (values (nreverse ret) (+ 1 max-levels)))
        (values longest (+ 1 max-levels)))))


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
                   (dolist (neighbor (case (type-of graph)
                                       (graph   (remove node edge))
                                       (digraph (cdr (member node edge)))))
                     (cond ((member neighbor path)
                            (push (subseq path 0 (1+ (position neighbor path)))
                                  cycles))
                           (t (follow neighbor
                                      (cons neighbor path)
                                      (cons edge used-edges)))))))))
      (dolist (node (nodes graph))
        (unless (member node seen)
          (follow node (list node) nil))))
    (remove-duplicates cycles :test 'set-equal)))

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
      (loop :for cycle = (pop basic-cycles) :while cycle :do
         (push cycle cycles)
         (mapc (lambda (c) (push (combine c cycle) cycles))
               (remove-if-not {intersection cycle} basic-cycles)))
      cycles)))

(defgeneric minimum-spanning-tree (graph)
  (:documentation "Return a minimum spanning tree of GRAPH.
Prim's algorithm is used."))

(defmethod minimum-spanning-tree ((graph graph))
  (assert (connectedp graph) (graph) "~S is not connected" graph)
  (let ((copy (copy graph))
        (tree (populate (make-instance 'graph)
                :nodes (list (random-elt (nodes graph)))))
        (total-nodes (length (nodes graph))))
    (loop :until (= (length (nodes tree)) total-nodes) :do
       (let ((e (car (sort
                      (remove-if-not
                       {intersection (set-difference (nodes copy) (nodes tree))}
                       (mapcan {node-edges copy} (nodes tree)))
                      #'< :key {edge-value copy}))))
         (when e
           (add-edge tree e (edge-value graph e))
           (delete-edge copy e))))
    tree))

(defgeneric connected-groups-of-size (graph size)
  (:documentation "Return all connected node groups of SIZE in GRAPH."))

(defmethod connected-groups-of-size ((graph graph) size)
  ;; Note: this function doesn't work with hyper graphs
  (assert (> size 1) (size) "can't group less than two items")
  (let ((connected-groups (edges graph)))
    (loop :for i :from 2 :below size :do
       (setf connected-groups
             (mapcan (lambda (group)
                       (mapcar {union group}
                               (remove-if {subsetp _ group}
                                          (mapcan {node-edges graph}
                                                  group))))
                     connected-groups)))
    (remove-duplicates connected-groups :test 'set-equal)))

(defgeneric closedp (graph nodes)
  (:documentation "Return true if NODES are fully connected in GRAPH."))

(defmethod closedp ((graph graph) nodes)
  (block nil ;; Note: this function doesn't work with hyper graphs
    (map-combinations (lambda (pair) (unless (has-edge-p graph pair) (return nil)))
                      nodes :length 2)))

(defgeneric clustering-coefficient (graph)
  (:documentation "Fraction of connected triples which are closed."))

(defmethod clustering-coefficient ((graph graph))
  (let ((triples (connected-groups-of-size graph 3)))
    (/ (length (remove-if-not {closedp graph} triples)) (length triples))))


;;; Shortest Path
(defgeneric shortest-path (graph a b)
  (:documentation "Return the shortest path in GRAPH from A to B.
GRAPH must be a directed graph.  Dijkstra's algorithm is used."))

;; TODO: needs to work for un-directed edges
(defmethod shortest-path ((graph graph) a b &aux seen)
  (block nil ;; (car next) is leading node, (cdr next) is edge path
    (let ((next (list (list a))))
      (loop :until (null next) :do
         (setf next
               (mapcan
                (lambda-bind ((from . rest))
                  (mapcan
                   (lambda (edge)
                     (if (case (type-of graph)
                           (graph   (member b edge))
                           (digraph (member b (cdr (member from edge)))))
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
(defun weigh-cut (graph cut)
  (reduce #'+ (mapcar {edge-value graph}
                      (remove-if-not (lambda (edge)
                                       (and (intersection edge (first cut))
                                            (intersection edge (second cut))))
                                     (edges graph)))))

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
      (let* ((half (cdar (sort cuts-of-phase #'< :key #'car)))
             (cut  (list half (set-difference (nodes graph) half))))
        (values (sort cut #'< :key #'length) (weigh-cut graph cut))))))


;;; Random graphs generation
(defgeneric preferential-attachment-populate (graph nodes &key edge-vals)
  (:documentation ;; TODO: add optional argument for desired average degree
   "Add NODES to GRAPH using preferential attachment, return the new edges.
Optionally assign edge values from those listed in EDGE-VALS."))

(defmethod preferential-attachment-populate ((graph graph) nodes &key edge-vals)
  (let ((degree-sum 0) (connections (make-array (* 2 (length nodes)))))
    (flet ((save-edge (from to)
             (incf degree-sum 2)
             (setf (aref connections (- degree-sum 2)) from)
             (setf (aref connections (- degree-sum 1)) to)
             (add-edge graph (list from to) (when edge-vals (pop edge-vals)))))
      (assert (not (= 1 (length nodes))) (nodes)
              "Can't preferentially attach a single node.")
      (when (null (nodes graph))
        (save-edge (pop nodes) (pop nodes)))
      (mapc (lambda (n) (save-edge n (aref connections (random degree-sum)))) nodes)
      (edges-w-values graph))))

(defgeneric erdos-renyi-populate (graph m)
  (:documentation
   "Populate GRAPH with M edges in an Erdős–Rényi random graph model."))

(defmethod erdos-renyi-populate ((graph graph) m)
  (let* ((nodes (coerce (nodes graph) 'vector))
         (num (length nodes)))
    (loop :until (= m 0) :do
       ;; NOTE: this naive approach will slow down drastically for
       ;;       large nearly complete graphs
       (let ((a (aref nodes (random num)))
             (b (aref nodes (random num))))
         (unless (or (= a b) (has-edge-p graph (list a b)))
           (add-edge graph (list a b))
           (decf m)))))
  graph)

(defun erdos-renyi-graph (n m)
  "Return an Erdős–Rényi graph with N nodes and M edges."
  (assert (and (not (< m 0)) (< m (/ (* n (1- n)) 2))) (n m)
          "an ~S-node graph can not have ~S edges" n m)
  (erdos-renyi-populate (populate (make-instance 'graph)
                          :nodes (loop :for i :below n :collect i))
                        m))

(defun erdos-renyi-digraph (n m)
  "Return an Erdős–Rényi digraph with N nodes and M edges."
  (assert (and (not (< m 0)) (< m (* n (1- n)))) (n m)
          "an ~S-node digraph can not have ~S edges" n m)
  (erdos-renyi-populate (populate (make-instance 'digraph)
                          :nodes (loop :for i :below n :collect i))
                        m))


;;; Centrality
(defgeneric farness (graph node)
  (:documentation
   "Sum of the distance from NODE to every other node in connected GRAPH."))

(defmethod farness ((graph graph) node)
  (assert (connectedp graph) (graph)
          "~S must be connected to calculate farness." graph)
  (reduce #'+ (mapcar [#'length {shortest-path graph node}]
                      (remove node (nodes graph)))))

(defgeneric closeness (graph node)
  (:documentation "Inverse of the `farness' for NODE in GRAPH."))

(defmethod closeness ((graph graph) node)
  (/ 1 ) (farness graph node))

(defgeneric betweenness (graph node)
  (:documentation
   "Fraction of shortest paths through GRAPH which pass through NODE.
Fraction of node pairs (s,t) s.t. s and t ≠ NODE and the shortest path
between s and t in GRAPH passes through NODE."))

(defmethod betweenness ((graph graph) node)
  (flet ((all-pairs (lst)
           (case (type-of graph)
             (graph (mapcan (lambda (n) (mapcar {list n} (cdr (member n lst)))) lst))
             (digraph (mapcan (lambda (n) (mapcar {list n} (remove n lst))) lst)))))
    (let ((num 0) (denom 0))
      (mapc (lambda-bind ((a b))
              (when (member node (apply #'append (shortest-path graph a b)))
                (incf num))
              (incf denom))
            (all-pairs (remove node (nodes graph))))
      (/ num denom))))

(defgeneric katz-centrality (graph node &key attenuation)
  (:documentation "Combined measure of number and nearness of nodes to NODE."))

(defmethod katz-centrality ((graph graph) node &key (attenuation 0.8))
  (let ((cc (connected-component graph node)))
    (reduce #'+ (mapcar [{expt attenuation} #'length {shortest-path graph node}]
                        (remove node cc)))))


;;; Degeneracy
;;
;; From the Wikipedia article on "Degeneracy (graph theory)".
;;
(defgeneric degeneracy (graph)
  (:documentation "Return the degeneracy and k-cores of GRAPH.
Also return the node ordering with optimal coloring number as an
alist.  The `car' of each element of the alist identifies k-cores and
the `cdr' holds the nodes in the ordering."))

(defmethod degeneracy ((graph graph))
  (let ((copy (copy graph))
        (node-degree (make-hash-table))
        (max-degree 0) (num-nodes 0) (k 0) (i 0)
        by-degree output)
    ;; initialize
    (mapc (lambda (n)
            (let ((degree (degree copy n)))
              (incf num-nodes)
              (setf (gethash n node-degree) degree)
              (setf max-degree (max max-degree degree))))
          (nodes copy))
    (setf by-degree (make-array (1+ max-degree) :initial-element nil))
    (maphash (lambda (node degree) (push node (aref by-degree degree)))
             node-degree)
    ;; reduction
    (dotimes (n num-nodes (values k output))
      (setf i 0)
      (loop :until (aref by-degree i) :do (incf i))
      ;; create alist element for the new core
      (when (< k (setf k (max k i))) (push (list k) output))
      ;; drop a node and demote all neighbors
      (let ((node (pop (aref by-degree i))))
        (push node (cdr (assoc k output)))
        (mapc (lambda (node)
                (setf (aref by-degree (gethash node node-degree))
                      (remove node (aref by-degree (gethash node node-degree))))
                (decf (gethash node node-degree))
                (push node (aref by-degree (gethash node node-degree))))
              (prog1 (remove-duplicates (remove node (neighbors copy node)))
                (delete-node copy node)))))))

(defgeneric k-cores (graph)
  (:documentation "Return the k-cores of GRAPH."))

(defmethod k-cores ((graph graph))
  (multiple-value-bind (k cores) (degeneracy graph)
    (declare (ignorable k)) cores))
