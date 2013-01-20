(defsystem :graph-dot
  :description "Serialize graphs to and from DOT format"
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (alexandria metabang-bind graph cl-ppcre)
  :components
  ((:static-file "COPYING")
   (:file "package-dot")
   (:file "curry-compose-reader-macros")
   (:file "graph-dot"
          :depends-on ("package-dot" "curry-compose-reader-macros"))))
