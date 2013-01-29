(defsystem :graph-dot
  :description "Serialize graphs to and from DOT format"
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on
  (alexandria metabang-bind curry-compose-reader-macros graph cl-ppcre)
  :components
  ((:static-file "COPYING")
   (:file "package-dot")
   (:file "graph-dot" :depends-on ("package-dot"))))
