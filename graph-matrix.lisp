;;; graph-matrix.lisp --- build and manipulate matrix graph representations

;; Copyright (C) Eric Schulte and Tom Dye 2013

;; Licensed under the Gnu Public License Version 3 or later

;;; Commentary

;; Functions for manipulating matrix graph representations.

;;; Code:
(in-package :graph-matrix)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))
