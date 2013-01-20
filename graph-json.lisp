;;; graph.lisp --- because its easier to write than to learn such a library

;; Copyright (C) Eric Schulte 2013

;; Licensed under the Gnu Public License Version 3 or later

;;; Commentary

;; Helper function for serializing Graphs to JSON objects and reading
;; JSON objects back into graphs.  The JSON syntax for a graph is
;; compatible with the JavaScript d3 visualization library, allowing
;; for interactive viewing of graphs in the browser.

;; See [d3-link] for more.

;;; Code:
(in-package :graph-json)

(defun json-to-plist (input)
  "Parse string or stream INPUT into a plist."
  (let ((yason:*parse-object-key-fn*
         (lambda (el) (intern (string-upcase el) "KEYWORD")))
        (yason:*parse-object-as* :plist))
    (yason:parse input)))

;;; JSON import and export
(defmethod yason:encode ((symbol symbol) &optional (stream *standard-output*))
  (yason:encode (string-downcase (symbol-name symbol)) stream))

(defmethod to-json ((graph graph) &optional (stream *standard-output*))
  "Write a JSON encoding of GRAPH to STREAM."
  (let ((plist (to-plist graph)))
    (yason:encode
     (plist-hash-table
      (list :nodes (mapcar #'plist-hash-table (getf plist :nodes))
            :edges (mapcar #'plist-hash-table (getf plist :edges))))
     stream)))

(defun intern-string-nodes (plist)
  (list :nodes (mapcar (compose (curry #'list :name)
                                #'intern #'string-upcase
                                (rcurry #'getf :name))
                (getf plist :nodes))
        :edges (getf plist :edges)))

(defmethod from-json ((graph graph) input)
  "Parse string or stream INPUT into GRAPH."
  (from-plist graph (intern-string-nodes (json-to-plist input))))

;;; plist and D3 conversion
(defun plist-to-d3 (plist)
  "Convert plist graph encoding PLIST to D3 format.
Note that D3 only handles 2-node edges, so extra nodes in edges will
be silently dropped."
  (list :nodes (getf plist :nodes)
        :links (mapcar (lambda (edge)
                         (let ((edge  (getf edge :edge))
                               (value (getf edge :value)))
                           (list :source (first edge)
                                 :target (second edge)
                                 :value  value)))
                       (getf plist :edges))))

(defun d3-to-plist (plist)
  "Convert D3 format PLIST to graph encoding."
  (list :nodes (mapcar (compose (curry #'list :name)
                                #'intern #'string-upcase
                                (rcurry #'getf :name))
                       (getf plist :nodes))
        :edges (mapcar (lambda (edge)
                         (list :edge (list (getf edge :source)
                                           (getf edge :target))
                               :value (getf edge :value)))
                       (getf plist :links))))

;;; D3 format JSON import and export
(defmethod to-d3 ((graph graph) &optional (stream *standard-output*))
  "Return a JSON encoding of GRAPH formatted for D3."
  (let ((plist (plist-to-d3 (to-plist graph))))
    (yason:encode
     (plist-hash-table
      (list :nodes (mapcar #'plist-hash-table (getf plist :nodes))
            :links (mapcar #'plist-hash-table (getf plist :links))))
     stream)))

(defmethod from-d3 ((graph graph) input)
  "Parse a D3 format string or stream INPUT into GRAPH."
  (from-plist graph (d3-to-plist (json-to-plist input))))
