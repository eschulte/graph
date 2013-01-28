(defsystem :graph
  :description "simple library for building and manipulating graphs"
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (alexandria metabang-bind curry-compose-reader-macros)
  :components
  ((:static-file "COPYING")
   (:file "package")
   (:file "graph" :depends-on ("package"))))
