;;; graph-matrix.lisp --- build and manipulate matrix graph representations

;; Copyright (C) Eric Schulte and Tom Dye 2013

;; Licensed under the Gnu Public License Version 3 or later

;;; Commentary

;; Functions for manipulating matrix graph representations.

;;; Code:
(in-package :graph-matrix)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

(defgeneric to-adjacency-foreign-array (graph)
  (:documentation "Return the adjacency matrix of GRAPH.
The result is a `gsll' array."))

(defmethod to-adjacency-foreign-array ((graph graph))
  (let ((grid:*default-grid-type* 'grid:foreign-array))
    (let ((node-index-hash (make-hash-table))
          (counter -1))
      (mapc (lambda (node) (setf (gethash node node-index-hash) (incf counter)))
            (nodes graph))
      (let ((matrix (grid:make-foreign-array
                     '(unsigned-byte 8)
                     :dimensions (list (1+ counter) (1+ counter)))))
        (mapc (lambda-bind ((a b))
                (setf (grid:aref matrix
                                 (gethash a node-index-hash)
                                 (gethash b node-index-hash))
                      1))
              (edges graph))
        matrix))))
