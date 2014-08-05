(defsystem :graph-matrix
  :description "build and manipulate matrix graph representations"
  :author ("Eric Schulte <schulte.eric@gmail.com>" "Thomas Dye")
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on
  (alexandria metabang-bind curry-compose-reader-macros graph femlisp)
  :components
  ((:static-file "COPYING")
   (:file "package-matrix")
   (:file "graph-matrix" :depends-on ("package-matrix"))))

(defsystem :graph-matrix-test
  :description "test the matrix graph representations"
  :author ("Eric Schulte <schulte.eric@gmail.com>" "Thomas Dye")
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on
  (alexandria metabang-bind stefil curry-compose-reader-macros
   graph graph-matrix)
  :components
  ((:static-file "COPYING")
   (:module "test"
    :components ((:file "package-matrix")
                 (:file "graph-matrix" :depends-on ("package-matrix"))))))
