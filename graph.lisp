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
  ((test   :initarg :test   :accessor test   :initform #'eql)
   (node-h :initarg :node-h :accessor node-h :initform (make-hash-table))
   (edge-h :initarg :edge-h :accessor edge-h :initform (make-hash-table :test 'equalp))))

(defmethod edges ((graph graph))
  "Return a list of the edges in GRAPH."
  (loop :for key :being :each :hash-key :of (edge-h graph) :collect key))

(defmethod nodes ((graph graph))
  "Return a list of the nodes in GRAPH."
  (loop :for key :being :each :hash-key :of (node-h graph) :collect key))

(defmethod ghash ((graph graph) node-or-edge)
  (case node-or-edge
    (:node (node-h graph))
    (:edge (edge-h graph))))

(defmethod has-it-p ((graph graph) node-or-edge it)
  (multiple-value-bind (val included) (gethash it (ghash graph node-or-edge))
    (declare (ignorable val)) included))

(defmethod has-node-p ((graph graph) node)
  "Return `true' if GRAPH has node NODE."
    (has-it-p graph :node node))

(defmethod has-edge-p ((graph graph) edge)
  "Return `true' if GRAPH has edge EDGE."
  (has-it-p graph :edge edge))

(defmethod add-node ((graph graph) node)
  "Add NODE to GRAPH."
  (unless (has-node-p graph node)
    (setf (gethash node (node-h graph)) nil)))

(defmethod add-edge ((graph graph) edge &optional value)
  "Add EDGE to GRAPH with optional VALUE."
  (mapc (lambda (node)
          (add-node graph node)
          (pushnew edge (gethash node (node-h graph))))
        edge)
  (setf (gethash edge (edge-h graph)) value))

(defmethod node-edges ((graph graph) node)
  "Return the value of NODE in GRAPH."
  (multiple-value-bind (edges included) (gethash node (node-h graph))
    (unless included (error 'missing-node "~S doesn't include ~S" graph node))
    edges))

(defmethod delete-node ((graph graph) node)
  "Delete NODE from GRAPH.
Delete and return the old edges of NODE in GRAPH."
  (prog1 (mapc (curry #'delete-edge graph) (node-edges graph node))
    (remhash node (node-h graph))))

(defmethod (setf node-edges) (new (graph graph) node)
  "Set the edges of NODE in GRAPH to NEW.
Delete and return the old edges of NODE in GRAPH."
  (prog1 (mapc (curry #'delete-edge graph) (gethash node (node-h graph)))
    (mapc (curry #'add-edge graph) new)))

(defmethod edge-value ((graph graph) edge)
  "Return the value of EDGE in GRAPH."
  (multiple-value-bind (value included) (gethash edge (edge-h graph))
    (unless included (error 'missing-edge "~S doesn't include ~S" graph edge))
    value))

(defmethod delete-edge ((graph graph) edge)
  "Delete EDGE from GRAPH.
Return the old value of EDGE."
  (prog1 (edge-value graph edge)
    (mapc (lambda (node) (setf (gethash node (node-h graph))
                          (delete edge (gethash node (node-h graph))
                                  :test #'equalp)))
          edge)
    (remhash edge (edge-h graph))))

(defmethod (setf edge-value) (new (graph graph) edge)
  "Set the value of EDGE in GRAPH to NEW."
  (setf (gethash edge (edge-h graph)) new))

(defun make-graph (&key (nodes nil) (edges nil) (test #'eql))
  "Make a graph."
  (let ((g (make-instance 'graph :test test)))
    (mapc (curry #'add-node g) nodes)
    (mapc (curry #'add-edge g) edges)
    g))


;;; Complex graph methods
;; (defmethod merge-nodes ((graph graph) node1 node2 val)
;;   "Combine NODE1 and NODE2 in GRAPH into new node VAL.
;; All edges of NODE1 and NODE2 in GRAPH will be combined into a new node
;; holding VALUE."
;;   (edges ))

(defmethod neighbors ((graph graph) node)
  "Return all nodes which share an edge with NODE in GRAPH."
  (apply #'append (node-edges graph node)))

(defmethod dir-neighbors ((graph graph) node)
  "Return all nodes after NODE in GRAPH along directed edges."
  (let ((edges (node-edges graph node)))
    (mapcan (compose #'cdr (curry #'member node)) (copy-tree edges))))

(defmethod dir-step ((graph graph) path)
  "Take all directed steps forward from PATH through GRAPH.
Returns a new path for each possible next step."
  (mapcar (lambda (next) (cons next path)) (dir-neighbors graph (car path))))

(defun connected-to- (graph from prev)
  (if (null from) (reverse prev)
      (let ((next (remove-duplicates (mapcan (curry #'neighbors graph) from))))
        (connected-to- graph (set-difference next prev) (union next prev)))))

(defmethod connected-to ((graph graph) node)
  "Return all nodes reachable from NODE."
  (connected-to- graph (list node) (list node)))

(defun dir-connected-to- (graph new prev)
  (if (null new) (reverse prev)
      (let ((next (mapcan (curry #'dir-neighbors graph) new)))
        (dir-connected-to-
         graph (set-difference next prev) (union next prev)))))

(defmethod dir-connected-to ((graph graph) node)
  "Return all node reachable along directed edges from NODE."
  (dir-connected-to- graph (list node) nil))

(defmethod connectedp ((graph graph))
  "Return true if the graph is connected."
  (let ((nodes (nodes graph)))
    (subsetp (nodes *graph*) (connected-to graph (car nodes)))))

(defmethod dir-connectedp ((graph graph))
  "Return true if directed nodes connect every pair of edges."
  (let ((nodes (nodes *graph*)))
    (every (compose (curry #'subsetp nodes)
                    (curry #'dir-connected-to graph))
           nodes)))

(defun connected-components- (graph nodes ccs)
  (if (null nodes) ccs
      (let ((cc (connected-to graph (car nodes))))
        (connected-compoents- graph (set-difference nodes cc) (cons cc ccs)))))

(defmethod connected-components ((graph graph))
  "Return a list of the connected components of GRAPH."
  (connected-components- graph (nodes graph) nil))

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
