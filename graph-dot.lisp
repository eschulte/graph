;;; graph-dot.lisp --- serialize graphs to and from DOT format

;; Copyright (C) Eric Schulte 2013

;; Licensed under the Gnu Public License Version 3 or later

;;; Commentary

;; Functions for reading/writing graphs from/to the
;; [graphviz](http://www.graphviz.org/) DOT format.

;;; Code:
(in-package :graph-dot)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))


;;; Visualization
(defun edge-to-dot (edge type &optional edge-label)
  (flet ((wrp (val wrapper) (if val (format nil wrapper val) "")))
    (let ((edge-label (or edge-label (constantly nil))))
      (concatenate 'string
        (case type
          (graph   (apply #'format nil "  \"~a\" -- \"~a\"" edge))
          (digraph (apply #'format nil "  \"~a\" -> \"~a\"" edge)))
        (wrp (funcall edge-label edge) " [label=\"~a\"]")
        ";" (list #\Newline)))))

(defun node-to-dot (node &optional node-label node-color)
  (flet ((wrp (val wrapper) (if val (format nil wrapper val) "")))
    (let ((node-label (or node-label (constantly nil)))
          (node-color (or node-color (constantly nil))))
      (concatenate 'string
        (format nil "  \"~a\"" node)
        (wrp (funcall node-label node) " [label=\"~a\"]")
        (wrp (funcall node-color node) " [style=\"filled\"] [fillcolor=\"~a\"]")
        ";" (list #\Newline)))))

(defgeneric to-dot (graph &key stream node-label edge-label node-color)
  (:documentation "Print the dot code representing GRAPH. Keyword
arguments NODE-LABEL and EDGE-LABEL may provide functions which when
called on a node or edge return a label for that element of the dot
graph. NODE-COLOR expects a function that when called on a node
returns a color that will be used to fill the node."))

(defmethod to-dot ((graph graph)
                   &key (stream t) node-label (edge-label {edge-value graph})
                     node-color)
  (format stream "~a to_dot {~%" (intern (string-downcase (type-of graph))))
  (mapc [{format stream "~a"} {node-to-dot _ node-label node-color}]
        (nodes graph))
  (mapc [{format stream "~a"} {edge-to-dot _ (type-of graph) edge-label}]
        (edges graph))
  (format stream "}~%"))

(defgeneric to-dot-file (graph path &key node-label edge-label node-color)
  (:documentation "Write a dot representation of GRAPH to PATH."))

(defmethod to-dot-file
    ((graph graph) path &key node-label edge-label node-color)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (to-dot graph :stream out :node-label node-label
            :edge-label edge-label :node-color node-color)))

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
