;;; curry-compose-reader-macros --- concise curry and compose

;; Copyright (C) Eric Schulte 2013

;; Licensed under the Gnu Public License Version 3 or later

;;; Commentary

;; Reader macros for concise function partial application and
;; composition.
;;
;; These macros use the `curry', `rcurry' and `compose' functions from
;; the Alexandria library.  The contents of curly brackets are curried
;; and the contents of square brackets are composed.  The `_' symbol
;; inside curly brackets changes the order of arguments with `rcurry'.
;;
;; The following examples demonstrate the usage.
;;
;;     ;; partial application `curry'
;;     (mapcar {+ 1} '(1 2 3 4)) ; => (2 3 4 5)
;;
;;     ;; alternate order of arguments `rcurry'
;;     (mapcar {- _ 1} '(1 2 3 4)) ; => (0 1 2 3)
;;
;;     ;; function composition
;;     (mapcar [#'list {* 2}] '(1 2 3 4)) ; => ((2) (4) (6) (8))

;;; Code:

;; partial application with {} using Alexandria's `curry' and `rcurry'
(set-syntax-from-char #\{ #\( )
(set-syntax-from-char #\} #\) )

(defun lcurly-brace-reader (stream inchar)
  (declare (ignore inchar))
  (let ((spec (read-delimited-list #\} stream t)))
    (if (eq (cadr spec) '_)
        `(rcurry (function ,(car spec)) ,@(cddr spec))
        `(curry (function ,(car spec)) ,@(cdr spec)))))

(set-macro-character #\{ #'lcurly-brace-reader)
(set-macro-character #\} (get-macro-character #\) ))

;; composition with [] using Alexandria's `compose'
(set-syntax-from-char #\[ #\( )
(set-syntax-from-char #\] #\) )

(defun lsquare-brace-reader (stream inchar)
  (declare (ignore inchar))
  (cons 'compose (read-delimited-list #\] stream t)))

(set-macro-character #\[ #'lsquare-brace-reader)
(set-macro-character #\] (get-macro-character #\) ))
