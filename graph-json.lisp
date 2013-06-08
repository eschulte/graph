;;; graph.lisp --- because its easier to write than to learn such a library

;; Copyright (C) Eric Schulte 2013

;; Licensed under the Gnu Public License Version 3 or later

;;; Commentary

;; Helper function for serializing Graphs to JSON objects and reading
;; JSON objects back into graphs.  The JSON syntax for a graph is
;; compatible with the JavaScript d3 visualization library, allowing
;; for interactive viewing of graphs in the browser.

;; See [d3](http://d3js.org/)
;; (specifically [d3-force](http://bl.ocks.org/4062045)) for more.

;;; Code:
(in-package :graph-json)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

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
  (list :nodes (mapcar [{list :name} #'intern #'string-upcase {getf _ :name}]
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
  (list :nodes (mapcar [{list :name} #'intern #'string-upcase {getf _ :name}]
                       (getf plist :nodes))
        :edges (mapcar (lambda (edge)
                         (list :edge (list (getf edge :source)
                                           (getf edge :target))
                               :value (getf edge :value)))
                       (getf plist :links))))

;;; D3 format JSON import and export
(defgeneric to-d3 (graph &optional stream)
  (:documentation "Return a JSON encoding of GRAPH formatted for D3.
Edges should have numeric values which d3 will translate into their width."))

(defmethod to-d3 ((graph graph) &optional (stream *standard-output*))
  (let* ((plist (plist-to-d3 (to-plist graph)))
         (hash (plist-hash-table
                (list :nodes (mapcar #'plist-hash-table (getf plist :nodes))
                      :links (mapcar #'plist-hash-table (getf plist :links))))))
    (if stream
        (yason:encode hash stream)
        (with-output-to-string (out)
          (yason:encode hash out)))))

(defgeneric from-d3 (graph input)
  (:documentation "Parse a D3 format string or stream INPUT into GRAPH."))

(defmethod from-d3 ((graph graph) input)
  (from-plist graph (d3-to-plist (json-to-plist input))))

(defgeneric to-html (graph &optional stream)
  (:documentation "Write GRAPH to an HTML file.
The resulting HTML file will display an interactive version of GRAPH.
Uses `to-d3' to first encode the graph as JSON which is embedded in
the HTML page."))

(defmethod to-html ((graph graph) &optional (stream *standard-output*))
  (format stream d3-html (to-d3 graph nil)))

(defvar d3-html
  "<!DOCTYPE html>
<!-- Taken from http://bl.ocks.org/4062045 -->
<html>
  <style>
    .node { stroke: #fff; stroke-width: 1.5px; }
    .link { stroke: #999; stroke-opacity: .6; }
  </style>
  <body>
    <script src=\"http://d3js.org/d3.v3.min.js\"></script>
    <script>

      var width = 960,
      height = 500;

      var color = d3.scale.category20();

      var force = d3.layout.force()
      .charge(-120)
      .linkDistance(30)
      .size([width, height]);

      var svg = d3.select(\"body\").append(\"svg\")
      .attr(\"width\", width)
      .attr(\"height\", height);

      var ingest = function(graph) {
      force
      .nodes(graph.nodes)
      .links(graph.links)
      .start();

      var link = svg.selectAll(\".link\")
      .data(graph.links)
      .enter().append(\"line\")
      .attr(\"class\", \"link\")
      .style(\"stroke-width\", function(d) { return Math.sqrt(d.value); });

      var node = svg.selectAll(\".node\")
      .data(graph.nodes)
      .enter().append(\"circle\")
      .attr(\"class\", \"node\")
      .attr(\"r\", 5)
      .style(\"fill\", function(d) { return color(d.group); })
      .call(force.drag);

      node.append(\"title\")
      .text(function(d) { return d.name; });

      force.on(\"tick\", function() {
      link.attr(\"x1\", function(d) { return d.source.x; })
      .attr(\"y1\", function(d) { return d.source.y; })
      .attr(\"x2\", function(d) { return d.target.x; })
      .attr(\"y2\", function(d) { return d.target.y; });

      node.attr(\"cx\", function(d) { return d.x; })
      .attr(\"cy\", function(d) { return d.y; });
      });
      };

      ingest(~a);

    </script>
  </body>
</html>
")
