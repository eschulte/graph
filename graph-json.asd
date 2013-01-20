(defsystem :graph-json
  :description "Serialize graphs to/from json with D3 format support."
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (alexandria metabang-bind graph yason)
  :components
  ((:static-file "COPYING")
   (:file "package-json")
   (:file "curry-compose-reader-macros")
   (:file "graph-json"
          :depends-on ("package-json" "curry-compose-reader-macros"))))
