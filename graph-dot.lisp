;;; graph-dot.lisp --- serialize graphs to and from DOT format

;; Copyright (C) Eric Schulte 2012

;; Licensed under the Gnu Public License Version 3 or later

;;; Code:
(in-package :graph-dot)


;;; Visualization
(defun edge-to-dot (graph edge)
  (concatenate 'string
    (if (eq 'digraph (type-of graph))
        (apply #'format nil "  \"~a\" -> \"~a\"" edge)
        (apply #'format nil "  \"~a\" -- \"~a\"" edge))
    (if (edge-value graph edge)
        (format nil "[label=\"~a\"];~%" (edge-value graph edge))
        ";")))

(defgeneric to-dot (graph &optional stream)
  (:documentation "Print the dot code representing GRAPH."))

(defmethod to-dot ((graph graph) &optional (stream t))
  (format stream "graph to_dot {~%")
  (mapc {format stream "  \"~a\";~%"} (nodes graph))
  (mapc [{format stream "~a"} {edge-to-dot graph}] (edges graph))
  (format stream "}~%"))

(defmethod to-dot ((digraph digraph) &optional (stream t))
  (format stream "digraph to_dot {~%")
  (mapc {format stream "  \"~a\";~%"} (nodes digraph))
  (mapc [{format stream "~a"} {edge-to-dot digraph}] (edges digraph))
  (format stream "}~%"))

(defgeneric to-dot-file (graph path)
  (:documentation "Write a dot representation of GRAPH to PATH."))

(defmethod to-dot-file ((graph graph) path)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (to-dot graph out)))
