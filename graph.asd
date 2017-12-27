(defsystem :graph
  :description "simple library for building and manipulating graphs"
  :version "0.0.0"
  :author ("Eric Schulte <schulte.eric@gmail.com>" "Thomas Dye")
  :licence "GPL V3"
  :depends-on (alexandria
               metabang-bind
               named-readtables
               curry-compose-reader-macros)
  :components
  ((:static-file "COPYING")
   (:file "graph")))

(defsystem :graph/test
  :description "Test the graph library."
  :author ("Eric Schulte <schulte.eric@gmail.com>" "Thomas Dye")
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (alexandria
               metabang-bind
               graph
               stefil
               named-readtables
               curry-compose-reader-macros)
  :components
  ((:static-file "COPYING")
   (:module "test" :components ((:file "graph")))))

(defsystem :graph/dot
  :description "Serialize graphs to and from DOT format"
  :author ("Eric Schulte <schulte.eric@gmail.com>" "Thomas Dye")
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (alexandria
               metabang-bind
               named-readtables
               curry-compose-reader-macros
               graph
               cl-ppcre)
  :components
  ((:static-file "COPYING")
   (:file "graph-dot")))

(defsystem :graph/json
  :description "Serialize graphs to/from json with D3 format support."
  :author ("Eric Schulte <schulte.eric@gmail.com>" "Thomas Dye")
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (alexandria
               metabang-bind
               named-readtables
               curry-compose-reader-macros
               graph
               yason)
  :components
  ((:static-file "COPYING")
   (:file "graph-json")))

(defsystem :graph/matrix
  :description "build and manipulate matrix graph representations"
  :author ("Eric Schulte <schulte.eric@gmail.com>" "Thomas Dye")
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (alexandria
               metabang-bind
               named-readtables
               curry-compose-reader-macros
               graph
               femlisp)
  :components
  ((:static-file "COPYING")
   (:file "graph-matrix")))

(defsystem :graph/matrix-test
  :description "test the matrix graph representations"
  :author ("Eric Schulte <schulte.eric@gmail.com>" "Thomas Dye")
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (alexandria
               metabang-bind
               stefil
               named-readtables
               curry-compose-reader-macros
               graph
               graph/matrix)
  :components
  ((:static-file "COPYING")
   (:module "test"
            :components ((:file "graph-matrix")))))
