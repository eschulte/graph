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

(defmethod edges ((graph graph) &aux return)
  (loop :for key :being :each :hash-key :of (edge-h graph) :collect key))

(defmethod nodes ((graph graph))
  (loop :for key :being :each :hash-key :of (node-h graph) :collect key))

(defmethod ghash ((graph graph) node-or-edge)
  (case node-or-edge
    (:node (node-h graph))
    (:edge (edge-h graph))))

(defmethod has-it-p ((graph graph) node-or-edge it)
  (multiple-value-bind (val included) (gethash it (ghash graph node-or-edge))
    (declare (ignorable val)) included))

(defmethod has-node-p ((graph graph) node)
  (has-it-p graph :node node))

(defmethod has-edge-p ((graph graph) edge)
  (has-it-p graph :edge edge))

(defmethod add-node ((graph graph) node)
  (unless (has-node-p graph node)
    (setf (gethash node (node-h graph)) nil)))

(defmethod add-edge ((graph graph) edge &optional value)
  (mapc (lambda (node)
          (add-node graph node)
          (pushnew edge (gethash node (node-h graph))))
        edge)
  (setf (gethash edge (edge-h graph)) value))

(defmethod node-edges ((graph graph) node)
  (multiple-value-bind (edges included) (gethash node (node-h graph))
    (unless included (error 'missing-node "~S doesn't include ~S" graph node))
    edges))

(defmethod (setf node-edges) (new (graph graph) node)
  (error 'todo "implement `node-edges'"))

(defmethod edge-value ((graph graph) edge)
  (multiple-value-bind (value included) (gethash edge (edge-h graph))
    (unless included (error 'missing-edge "~S doesn't include ~S" graph edge))
    value))

(defmethod (setf edge-value) (new (graph graph) edge)
  (setf (gethash edge (edge-h graph)) new))

(defun make-graph (&key (nodes nil) (edges nil) (test #'eql))
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
  (apply #'append (node-edges graph node)))

(defmethod dir-neighbors ((graph graph) node)
  (let ((edges (node-edges graph node)))
    (mapcan (compose #'cdr (curry #'member node)) (copy-tree edges))))

(defun dir-step (graph path)
  "Take all steps forward from a path through a graph.
Returns a new path for each possible next step."
  (mapcar (lambda (next) (cons next path)) (dir-neighbors graph (car path))))

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
