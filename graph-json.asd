(defsystem :graph-json
  :description "Serialize graphs to/from json with D3 format support."
  :author ("Eric Schulte <schulte.eric@gmail.com>" "Thomas Dye")
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (alexandria metabang-bind curry-compose-reader-macros graph yason)
  :components
  ((:static-file "COPYING")
   (:file "package-json")
   (:file "graph-json" :depends-on ("package-json"))))
