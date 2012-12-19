;;; graph.lisp --- because its easier to write than to learn such a library

;; Copyright (C) Eric Schulte 2012

;; Licensed under the Gnu Public License Version 3 or later

;;; Code:
(in-package :modularize)

(defclass graph ()
  ((nodes :initarg :nodes :accessor nodes :initform nil)
   (edges :initarg :edges :accessor edges :initform nil)))

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


;;; tests
#+testing
(progn
  (defvar *little-graph* (make-instance 'graph
                           :nodes '(:foo :bar :baz :qux)
                           :edges '(((:nodes . (0 1)))
                                    ((:nodes . (0 2)))
                                    ((:nodes . (1 2)))
                                    ((:nodes . (1 3))))))

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
  )
