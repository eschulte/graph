;;; graph-dot.lisp --- serialize graphs to and from DOT format

;; Copyright (C) Eric Schulte and Thomas Dye 2013

;; Licensed under the Gnu Public License Version 3 or later

;;; Commentary

;; Functions for reading/writing graphs from/to the
;; [graphviz](http://www.graphviz.org/) DOT format.
;;
;; Many graphviz properties and exposed as keyword arguments to the
;; `to-dot` function.
;;
;;     (defvar *graph* (populate (make-instance 'digraph)
;;                              :nodes '(a b c d e f)
;;                              :edges-w-values '(((a b) . 3)
;;                                                ((b c) . 2)
;;                                                ((c d) . 1)
;;                                                ((d b) . 2)
;;                                                ((b e) . 3))))
;;
;; (let ((ccs (mapcar #'cons (connected-components *graph*)
;;                    '(1 2 3 4))))
;;   (to-dot-file *graph* "dot-graph-1.dot"
;;                :node-attrs
;;                (list (cons :fillcolor
;;                            (lambda (n) (cdr (assoc-if {member n} ccs))))
;;                      (cons :style
;;                            (constantly "filled"))
;;                      (cons :colorscheme
;;                            (constantly "set34")))))
;;
;; <img src="dot-graph-1.png"/>
;;
;; Or less colorfully.
;;
;;     (setf *graph* (populate (make-instance 'digraph)
;;                     :edges '((A T2) (T1 B) (T2 B) (T2 C) (T1 D))))
;;     
;;     (let ((s1 (make-subgraph :attributes '(("color" . "lightgrey")
;;                                            ("label" . "One" ))
;;                              :node-list (first
;;                                          (connected-components
;;                                           *graph*
;;                                           :type :unilateral))))
;;           (s2 (make-subgraph :attributes '(("color" . "lightgrey")
;;                                            ("label" . "Two" ))
;;                              :node-list (second
;;                                          (connected-components
;;                                           *graph*
;;                                           :type :unilateral)))))
;;       (to-dot-file *graph* "dot-graph-2.dot"
;;                    :subgraphs (list s1 s2)))
;;
;; <img src="dot-graph-2.png"/>

;;; Code:
(defpackage #:graph/dot
  (:use :common-lisp
        :alexandria
        :metabang-bind
        :named-readtables
        :curry-compose-reader-macros
        :graph
        :cl-ppcre)
  (:export :to-dot :to-dot-file :from-dot :make-subgraph))
(in-package :graph/dot)
(in-readtable :curry-compose-reader-macros)


;;; Visualization
(defstruct rank
  "The information needed to specify a DOT rank statement. VALUE
  expects a string and NODE-LIST expects a list."
  value
  node-list)

(defun rank-print (r)
  "Returns a string containing a DOT rank statement. R is a RANK structure."
  (when (rank-p r))
  (with-output-to-string (out)
    (when (and (rank-value r) (rank-node-list r))
      (format out "{rank=~a;" (rank-value r))
      (mapc (lambda (n)
              (format out " ~s;" n))
            (rank-node-list r))
       (format out " }~%"))))

(defstruct subgraph
  "The information needed to specify a DOT subgraph. NODE-ATTRIBUTES,
EDGE-ATTRIBUTES, and ATTRIBUTES expect assoc lists, and NODE-LIST
expects a list."
  node-attributes
  edge-attributes
  attributes
  ranks
  node-list)

(defun subgraph-print (s)
  "Returns a string containing a DOT subgraph statement. S is a
SUBGRAPH structure."
  (when (subgraph-p s)
    (with-output-to-string (out)
      (format out "subgraph ~a {~%" (string (gensym "cluster_")))
      (when (subgraph-node-attributes s)
        (format out "  node [~a];~%"
                (mapc (lambda (pair)
                        (format out "~a=~a, " (car pair) (cdr pair)))
                      (subgraph-node-attributes s))))
      (when (subgraph-edge-attributes s)
        (format out "  edge [~a];~%"
                (mapc (lambda (pair)
                        (format out "~a=~a, " (car pair) (cdr pair)))
                      (subgraph-edge-attributes s))))
      (when (subgraph-attributes s)
        (mapc (lambda (pair)
                (format out "  ~a=\"~a\";~%" (car pair) (cdr pair)))
              (subgraph-attributes s)))
      (when (subgraph-ranks s)
        (mapcar #'rank-print (subgraph-ranks s)))
      (when (subgraph-node-list s)
        (mapc (lambda (n)
                (format out "  ~a;~%" n))
              (subgraph-node-list s)))
      (format out "  }~%"))))

(defun edge-to-dot (edge graph attrs &optional stream)
  (format stream " \"~a\" ~a \"~a\" ~{~a~^ ~};~%"
          (first edge)
          (etypecase graph
            (digraph "->")
            (graph "--"))
          (second edge)
          (mapcar (lambda-bind ((attr . fn))
                    (let ((val (funcall fn edge)))
                      (if val
                          (if (search "URL" (string attr))
                                    (format nil "[~a=~a]"
                                            (string-downcase
                                             (string attr)
                                             :end (- (length (string attr)) 3))
                                            val)
                                    (format nil "[~(~a~)=~a]" attr val)) "")))
                  attrs)))

(defun node-to-dot (node attrs &optional stream)
  (format stream "  \"~a\" ~{~a~^ ~};~%" node
          (mapcar (lambda-bind ((attr . fn))
                    (let ((val (funcall fn node)))
                      (if val (if (search "URL" (string attr))
                                  (format nil "[~a=~a]" attr val)
                                  (format nil "[~(~a~)=~a]" attr val)) "")))
                  attrs)))

(defgeneric to-dot (graph
                    &key stream attributes node-attrs edge-attrs
                      subgraphs ranks)
  (:documentation "Print the dot code representing GRAPH. The keyword
argument ATTRIBUTES takes an assoc list with DOT graph attribute (name
. value) pairs. NODE-ATTRS and EDGE-ATTRS also take assoc lists of DOT
graph attributes and functions taking nodes or edges respectively and
returning values. The DOT graph, node, and edge attributes are
described at http://www.graphviz.org/doc/info/attrs.html. SUBGRAPHS is
a list of SUBGRAPH structures.  RANKS is a list of RANK structures."))

(defmethod to-dot ((graph graph)
                   &key (stream t) attributes node-attrs edge-attrs
                     subgraphs ranks)
  ;; by default edges are labeled with their values
  (unless (assoc :label edge-attrs)
    (push (cons :label {edge-value graph}) edge-attrs))
  (format stream "~a to_dot {~%~{~a~}}~%"
          (etypecase graph
            (digraph "digraph")
            (graph "graph"))
          (append
           (mapcar (lambda-bind ((a . b))
                                (if (search "URL" (string a))
                                    (format nil "  ~a=~a;~%" a b)
                                    (format nil "  ~(~a~)=~a;~%" a b)))
                   attributes)
           (mapcar {node-to-dot _ node-attrs} (nodes graph))
           (mapcar {edge-to-dot _ graph edge-attrs} (edges graph))
           (mapcar #'subgraph-print subgraphs)
           (mapcar #'rank-print ranks))))

(defgeneric to-dot-file (graph path &key attributes node-attrs edge-attrs
                                      subgraphs ranks)
  (:documentation "Write a dot representation of GRAPH to PATH."))

(defmethod to-dot-file
    ((graph graph) path &key attributes node-attrs edge-attrs
                          subgraphs ranks)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (to-dot graph :stream out :attributes attributes :node-attrs node-attrs
            :edge-attrs edge-attrs :subgraphs subgraphs :ranks ranks)))

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
