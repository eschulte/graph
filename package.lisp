(defpackage #:graph
  (:use :common-lisp :alexandria :metabang-bind :curry-compose-reader-macros)
  (:export
   :graph
   :digraph
   :copy
   :digraph-of
   :graph-of
   :populate
   :graph-equal
   ;; Serialization
   :to-plist
   :from-plist
   :to-adjacency-matrix
   :to-value-matrix
   :from-value-matrix
   ;; Simple Graph Methods
   :edges
   :edges-w-values
   :nodes
   :nodes-w-values
   :has-node-p
   :has-edge-p
   :subgraph
   :add-node
   :add-edge
   :node-edges
   :degree
   :indegree
   :outdegree
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
   ;; Random Graph generation
   :preferential-attachment-populate
   :erdos-renyi-populate
   :erdos-renyi-graph
   :erdos-renyi-digraph
   ;; Centrality
   :farness
   :closeness
   :betweenness
   :katz-centrality
   ;; Degeneracy
   :degeneracy
   :k-cores))
