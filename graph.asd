(defsystem :graph
  :description "simple library for building and manipulating graphs"
  :version "0.0.0"
  :author ("Eric Schulte <schulte.eric@gmail.com>" "Thomas Dye")
  :licence "GPL V3"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (alexandria
               metabang-bind
               named-readtables
               curry-compose-reader-macros
               graph/graph))

(register-system-packages "femlisp-matlisp" '(:fl.matlisp))
