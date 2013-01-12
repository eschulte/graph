(defsystem :graph-test
  :description "Test the graph library."
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (alexandria metabang-bind graph stefil)
  :components
  ((:static-file "COPYING")
   (:file "package-test")
   (:file "graph-test" :depends-on ("package-test"))))
