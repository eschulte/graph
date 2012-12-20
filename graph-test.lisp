;;; graph.lisp --- because its easier to write than to learn such a library

;; Copyright (C) Eric Schulte 2012

;; Licensed under the Gnu Public License Version 3 or later

;;; Code:
(in-package :graph)
(require :stefil)
(use-package :stefil)
(defsuite graph)
(in-suite graph)

(defvar *graph* nil
  "Variable for use in graph tests.")

(defixture small-graph
    (:setup (setf *graph*
                  (make-graph
                   :nodes '(:foo :bar :baz :qux)
                   :edges '((nil :foo :bar)
                            (nil :foo :baz)
                            (nil :bar :baz)))))
  (:teardown (setf *graph* nil)))

(defixture graph
    (:setup (setf *graph*
                  (make-graph
                   :nodes '(a b c d e f)
                   :edges '((:nodes a b)
                            (:nodes b c)
                            (:nodes c d)
                            (:nodes d e)
                            (:nodes e c)
                            (:nodes e f)
                            (:nodes f b))))))


;;; Tests
(deftest simple-make-test ()
  (with-fixture simple-graph
    (is (= 4 (length (edges *graph*))))
    (is (= 3 (length (nodes *graph*))))))

(deftest dir-neighbors ()
  (with-fixture graph
    (is (tree-equal (dir-neighbors *graph* 'e)
                    '(C F)))))

(deftest cycles ()
  (with-fixture graph
    (is (tree-equal (cycles *graph*)
                    '((C D E F B) (D E C))))))

(deftest cycle-connected-components-1 ()
  (with-fixture graph
    (is (tree-equal (cycle-connected-components *graph*)
                    '((D E C C D E F B))))))

(deftest cycle-connected-components-2 ()
  (with-fixture graph
    (is (tree-equal (cycle-connected-components *g2*)
                    '((:BAR :BAZ :FOO) (:ZAP :ZAF :QUX) (:FIZ))))))

(deftest shortest-path ()
  (with-fixture small-graph
    (is (tree-equal (shortest-path *g* '(:foo) '(:baz :qux))
                    '(:FOO :BAZ)))))
