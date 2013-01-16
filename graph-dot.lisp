;;; graph-dot.lisp --- serialize graphs to and from DOT format

;; Copyright (C) Eric Schulte 2013

;; Licensed under the Gnu Public License Version 3 or later

;;; Code:
(in-package :graph-dot)


;;; Visualization
(defun edge-to-dot (graph edge &optional edge-label)
  (concatenate 'string
    (if (eq 'digraph (type-of graph))
        (apply #'format nil "  \"~a\" -> \"~a\"" edge)
        (apply #'format nil "  \"~a\" -- \"~a\"" edge))
    (if (or edge-label (edge-value graph edge))
        (format nil " [label=\"~a\"];~%" (if edge-label
                                             (funcall edge-label edge)
                                             (edge-value graph edge)))
        ";")))

(defun node-to-dot (node &optional node-label)
  (if node-label
      (format nil "  \"~a\" [label=\"~a\"];~%" node (funcall node-label node))
      (format nil "  \"~a\";" node)))

(defgeneric to-dot (graph &key stream node-label edge-label)
  (:documentation "Print the dot code representing GRAPH.
Keyword arguments NODE-LABEL and EDGE-LABEL may provide functions
which when called on a node or edge return a label for that element of
the dot graph."))

(defmethod to-dot ((graph graph) &key (stream t) node-label edge-label)
  (format stream "~a to_dot {~%" (intern (string-downcase (type-of graph))))
  (mapc [{format stream "~a"} (lambda (n) (node-to-dot n node-label))]
        (nodes graph))
  (mapc [{format stream "~a"} (lambda (e) (edge-to-dot graph e edge-label))]
        (edges graph))
  (format stream "}~%"))

(defgeneric to-dot-file (graph path &key node-label edge-label)
  (:documentation "Write a dot representation of GRAPH to PATH."))

(defmethod to-dot-file ((graph graph) path &key node-label edge-label)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (to-dot graph :stream out :node-label node-label :edge-label edge-label)))

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
