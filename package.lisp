(defpackage #:graph
  (:use :common-lisp :alexandria :metabang-bind)
  (:export
   :graph
   :digraph
   :copy
   :digraph-of
   :graph-of
   :populate
   :graph-equal
   ;; Simple Graph Methods
   :edges
   :edges-w-values
   :nodes
   :nodes-w-values
   :has-node-p
   :has-edge-p
   :add-node
   :add-edge
   :node-edges
   :delete-node
   :edge-value
   :delete-edge
   ;; Complex Graph Methods
   :merge-nodes
   :merge-edges
   :edge-neighbors
   :neighbors
   :precedents
   :connected-component
   :connectedp
   :connected-components
   ;; Cycles and strongly connected components
   :strongly-connected-components
   :basic-cycles
   :cycles
   ;; Shortest Path
   :shortest-path
   ;; Max Flow
   :residual
   :add-paths
   :max-flow
   ;; Min Cut
   :min-cut
   ;; Visualization
   :to-dot
   :dot-to-file
   ;; Serialization
   :to-plist
   :from-plist
   ))
