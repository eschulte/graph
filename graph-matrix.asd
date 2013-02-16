(defsystem :graph-matrix
  :description "build and manipulate matrix graph representations"
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on
  (alexandria metabang-bind curry-compose-reader-macros graph gsll)
  :components
  ((:static-file "COPYING")
   (:file "package-matrix")
   (:file "graph-matrix" :depends-on ("package-matrix"))))
