;;; graph-dot.lisp --- serialize graphs to and from DOT format

;; Copyright (C) Eric Schulte 2013

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

(defun from-dot (dot-string)
  "Parse the DOT format string DOT-STRING into a graph.
More robust behavior may be achieved through parsing the output of the
dot executable."
  (flet ((string->symbol (string) (intern (string-upcase string))))
    (let* ((graph-type-re "^ *((di)?graph)")
           (spec-re       "[\\s]*(\\[([^]]+)\\])?;")
           (node-name-re  "[\\s]*\"?([a-zA-Z0-9_]+)\"?")
           (node-spec-re  (concatenate 'string node-name-re spec-re))
           (edge-spec-re  (concatenate 'string
                            node-name-re "[\\s]+([->]+)" node-name-re spec-re))
           (label-name-re "label=(\"([^\"]+)\"|([^, ]+))[,\\]]")
           (number-re     "[0-9.\/e]+")
           (graph (multiple-value-bind (string matches)
                      (scan-to-strings graph-type-re dot-string)
                      (declare (ignorable string))
                    (make-instance (string->symbol (aref matches 0))))))
      ;; add nodes
      (do-register-groups (node spec) (node-spec-re dot-string)
        (declare (ignorable spec))
        (unless (member node '("node" "graph") :test 'string=)
          (add-node graph (string->symbol node))))
      ;; add edges
      (do-register-groups (left arrow right spec) (edge-spec-re dot-string)
        (declare (ignorable arrow))
        (multiple-value-bind (matchp regs) (scan-to-strings label-name-re spec)
          (add-edge graph
                    (mapcar #'string->symbol (list left right))
                    (when matchp
                      (if (scan number-re (aref regs 1))
                          (read-from-string (aref regs 1))
                          (aref regs 1))))))
      graph)))
