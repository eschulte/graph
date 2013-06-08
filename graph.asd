(defsystem :graph
  :description "simple library for building and manipulating graphs"
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (alexandria metabang-bind curry-compose-reader-macros)
  :components
  ((:static-file "COPYING")
   (:file "package")
   (:file "graph" :depends-on ("package"))))

(defsystem :graph-test
  :description "Test the graph library."
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on
  (alexandria metabang-bind graph stefil curry-compose-reader-macros)
  :components
  ((:static-file "COPYING")
   (:module "test"
            :components ((:file "package")
                         (:file "graph" :depends-on ("package"))))))
