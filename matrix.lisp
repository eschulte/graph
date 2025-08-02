;;; graph-matrix.lisp --- build and manipulate matrix graph representations

;; Copyright (C) Eric Schulte and Tom Dye 2013

;; Licensed under the Gnu Public License Version 3 or later

;;; Commentary

;; Functions for manipulating matrix graph representations.

;;; Code:
(defpackage #:graph/matrix
  (:use :common-lisp
        :alexandria
        :metabang-bind
        :named-readtables
        :curry-compose-reader-macros
        :graph
        :fl.matlisp)
  ;; shadow functions defined in alexandria, fl.matlisp, and graph
  (:shadow :copy :factorial :standard-deviation :variance :median :mean :degree)
  (:export
   :matrix
   :fast-matrix
   :matrix-ref
   :matrix-n-rows
   :matrix-n-cols
   :matrix-same-size-p
   :matrix-symmetric-p
   :matrix-entries-different-p
   :matrix-copy
   :matrix-transpose
   :make-universal-matrix
   :make-identity-matrix
   :make-zeros-matrix
   :to-adjacency-matrix
   :to-reachability-matrix
   :reachablep
   :reachable-from
   :to-strong-component-matrix
   :strong-component-of
   :to-distance-matrix
   :distance-from-to
   :reflexivep
   :irreflexivep
   :symmetricp
   :asymmetricp
   :transitivep
   :intransitivep
   :completep
   :relational-structure
   :infinite
   :infinitep))
(in-package :graph/matrix)
(in-readtable :curry-compose-reader-macros)

(defclass matrix ()
  ((self :initarg :self :accessor self :initform nil)))

(defclass fast-matrix (matrix) ())

(defgeneric infinite (matrix)
  (:documentation "Return the most-positive value for the element type
  of MATRIX."))

(defmethod infinite ((matrix matrix))
  most-positive-fixnum)

(defmethod infinite ((matrix fast-matrix))
  most-positive-single-float)

(defgeneric infinitep (value matrix)
  (:documentation "Non-nil if VALUE is the most-positive value that
  can be held in MATRIX."))

(defmethod infinitep (value (matrix matrix))
  (= value (infinite matrix)))

(defgeneric matrix-ref (matrix row col)
  (:documentation "Return the value at ROW and COL in MATRIX."))

(defmethod matrix-ref ((matrix matrix) row col)
  (aref (self matrix) row col))

(defmethod matrix-ref ((fm fast-matrix) row col)
  (fl.matlisp::mref (self fm) row col))

(defgeneric (setf matrix-ref) (new matrix row col)
  (:documentation "Make matrix-ref setf-able."))

(defmethod (setf matrix-ref) (new (matrix matrix) row col)
  (setf (aref (self matrix) row col) new))

(defmethod (setf matrix-ref) (new (fm fast-matrix) row col)
  (setf (fl.matlisp::mref (self fm) row col) new))

(defgeneric matrix-n-rows (matrix)
  (:documentation "Return the number of rows in MATRIX."))

(defmethod matrix-n-rows ((matrix matrix))
  (if (self matrix)
      (array-dimension (self matrix) 0)
      0))

(defmethod matrix-n-rows ((matrix fast-matrix))
  (if (self matrix)
      (fl.matlisp::nrows (self matrix))
      0))

(defgeneric matrix-n-cols (matrix)
  (:documentation "Return the number of columns in MATRIX."))

(defmethod matrix-n-cols ((matrix matrix))
  (if (self matrix)
      (array-dimension (self matrix) 1)
      0))

(defmethod matrix-n-cols ((matrix fast-matrix))
  (if (self matrix)
      (fl.matlisp::ncols (self matrix))
      0))

(defun matrix-same-size-p (m1 m2)
  "Return t if matrix M1 has the same number of rows and columns as
matrix M2, nil otherwise."
  (and (= (matrix-n-rows m1) (matrix-n-rows m2))
       (= (matrix-n-cols m1) (matrix-n-cols m2))))

(defun matrix-entries-different-p (m1 m2)
  "Returns nil if the entries in matrix M1 and matrix M2 do not differ
from one another.  Returns 1 if the sizes of matrix M1 and matrix M2
differ.  Otherwise, returns a list of lists containing discrepant
entries. "
  (let ((result))
    (if (matrix-same-size-p m1 m2)
        (let ((m (matrix-n-rows m1))
              (n (matrix-n-cols m1)))
          (loop :for i :from 0 :below m :do
             (loop :for j :from 0 :below n :do
                (unless (= (matrix-ref m1 i j)
                           (matrix-ref m2 i j))
                  (push (list i j) result))))
          (when result (reverse result)))
        (setf result 1))
    result))

(defun matrix-symmetric-p (matrix)
  "Return t if matrix MATRIX is symmetric, nil otherwise."
  (not (matrix-entries-different-p matrix (matrix-transpose matrix))))

(defgeneric matrix-copy (matrix)
  (:documentation "Return a copy of MATRIX."))

(defmethod matrix-copy ((matrix matrix))
  (let* ((m (matrix-n-rows matrix))
         (n (matrix-n-cols matrix))
         (result (make-zeros-matrix (make-instance 'matrix) m n)))
    (when (self matrix)
      (loop :for i :from 0 :below m :do
         (loop :for j :from 0 :below n :do
            (setf (matrix-ref result i j) (matrix-ref matrix i j)))))
    result))

(defmethod matrix-copy ((fm fast-matrix))
  (let ((result (make-instance 'fast-matrix)))
    (when (self fm)
      (setf (self result) (fl.matlisp::copy (self fm))))
    result))

(defgeneric matrix-sum (m1 m2 &key boolean)
  (:documentation "Return the result of adding matrix M1 and matrix
  M2. M1 and M2 must be the same size. If BOOLEAN is non-nil, then use
  boolean arithmetic, where 1+1=1."))

(defmethod matrix-sum ((m1 matrix) (m2 matrix) &key boolean)
  (and (matrix-same-size-p m1 m2)
       (let* ((m (matrix-n-rows m1))
              (n (matrix-n-cols m1))
              (result (make-zeros-matrix (make-instance 'matrix) m n))
              (zero 0)
              (one 1))
         (declare (type fixnum zero))
         (declare (type fixnum one))
         (loop :for i :from 0 :below m :do
            (loop :for j :from 0 :below n :do
               (setf (matrix-ref result i j)
                     (if boolean
                         (if (> (+ (matrix-ref m1 i j)
                                   (matrix-ref m2 i j)) 0) one zero)
                         (+ (matrix-ref m1 i j)
                            (matrix-ref m2 i j))))))
         result)))

(defmethod matrix-sum ((m1 fast-matrix) (m2 fast-matrix) &key boolean)
  (when (matrix-same-size-p m1 m2)
    (let ((result (make-instance 'fast-matrix)))
      (setf (self result) (fl.matlisp::m+ (self m1) (self m2)))
      (when boolean
        (let ((m (matrix-n-rows result))
              (n (matrix-n-cols result))
              (one 1.0s0))
          (declare (type single-float one))
          (loop :for i :from 0 :below m :do
             (loop :for j :from 0 :below n :do
                (if (> (matrix-ref result i j) 0)
                    (setf (matrix-ref result i j) one))))))
      result)))

(defgeneric matrix-difference (m1 m2)
  (:documentation "Return the result of subtracting M2 from M1. M1 and
  M2 must be the same size."))

(defmethod matrix-difference ((m1 matrix) (m2 matrix))
  (and (matrix-same-size-p m1 m2)
       (let ((result (matrix-copy m1))
             (m (matrix-n-rows m1))
             (n (matrix-n-cols m1)))
         (loop :for i :from 0 :below m :do
            (loop :for j :from 0 :below n :do
               (setf (matrix-ref result i j)
                     (- (matrix-ref result i j)
                        (matrix-ref m2 i j)))))
         result)))

(defgeneric matrix-elementwise-product (m1 m2 &key boolean)
  (:documentation "Return the result of multiplying the elements of
  matrix M1 and matrix M2. M1 and M2 must be the same size."))

(defmethod matrix-elementwise-product ((m1 matrix) (m2 matrix) &key boolean)
  (and (matrix-same-size-p m1 m2)
       (let ((result (matrix-copy m1))
             (m (matrix-n-rows m1))
             (n (matrix-n-cols m1)))
         (loop :for i :from 0 :below m :do
            (loop :for j :from 0 :below n :do
               (setf (matrix-ref result i j)
                     (if boolean
                         (if (* (matrix-ref result i j)
                                (matrix-ref m2 i j)) 1 0)
                         (* (matrix-ref result i j)
                            (matrix-ref m2 i j))))))
         result)))

(defgeneric matrix-product (m1 m2)
  (:documentation "Return the result of multiplying matrix M1 and
  matrix M2. The number of columns of M1 must equal the number of rows
  of M2."))

(defmethod matrix-product ((m1 matrix) (m2 matrix))
  (and (= (matrix-n-cols m1) (matrix-n-rows m2))
       (loop
          :with m = (matrix-n-rows m1)
          :with n = (matrix-n-cols m1)
          :with l = (matrix-n-cols m2)
          :with c = (make-zeros-matrix (make-instance 'matrix) m l)
          :for i :below m :do
          (loop :for k :below l :do
             (setf (matrix-ref c i k)
                   (loop :for j :below n
                      :sum (* (matrix-ref m1 i j)
                              (matrix-ref m2 j k)))))
          :finally (return c)))  )

(defmethod matrix-product ((m1 fast-matrix) (m2 fast-matrix))
  (and (= (matrix-n-cols m1) (matrix-n-rows m2))
       (let ((result (make-instance 'fast-matrix)))
         (setf (self result) (fl.matlisp::m* (self m1) (self m2)))
         result)))

(defgeneric matrix-transpose (matrix)
  (:documentation "Return a new matrix that interchanges the rows and
  columns of MATRIX."))

(defmethod matrix-transpose ((matrix matrix))
  (let ((m (matrix-n-rows matrix))
        (n (matrix-n-cols matrix))
        (result (make-instance 'matrix)))
    (setf result (make-zeros-matrix result n m))
    (loop :for i :from 0 :below m :do
       (loop :for j :from 0 :below n :do
          (setf (matrix-ref result j i)
                (matrix-ref matrix i j))))
    result))

(defmethod matrix-transpose ((fm fast-matrix))
  (let ((result (make-instance 'fast-matrix)))
    (setf (self result) (fl.matlisp::transpose (self fm)))
    result))

(defgeneric make-zeros-matrix (matrix rows cols)
  (:documentation "Return matrix MATRIX with ROWS rows and COLS
  columns of zeros."))

(defmethod make-zeros-matrix ((matrix matrix) rows cols)
  (setf (self matrix) (make-array (list rows cols)
                                  :element-type 'fixnum
                                  :initial-element 0))
  matrix)

(defmethod make-zeros-matrix ((fm fast-matrix) rows cols)
  (setf (self fm) (fl.matlisp::zeros rows cols 'single-float))
  fm)

(defgeneric make-universal-matrix (matrix rows cols)
  (:documentation "Return a universal matrix with ROWS rows and COLS columns."))

(defmethod make-universal-matrix ((matrix matrix) rows cols)
  (setf (self matrix) (make-array (list rows cols)
                                  :element-type 'fixnum
                                  :initial-element 1))
  matrix)

(defmethod make-universal-matrix ((fm fast-matrix) rows cols)
  (setf (self fm) (fl.matlisp::ones rows cols 'single-float))
  fm)

(defgeneric make-infinity-matrix (matrix rows cols)
  (:documentation "Return a matrix of ROWS rows and COLS cols with
  each entry set to infinity"))

(defmethod make-infinity-matrix ((matrix matrix) rows cols)
  (progn
    (setf (self matrix) (make-array (list rows cols)
                                    :element-type 'fixnum
                                    :initial-element (infinite matrix)))
    matrix))

(defmethod make-infinity-matrix ((fm fast-matrix) rows cols)
  (progn
    (setf (self fm) (fl.matlisp::zeros rows cols 'single-float))
    (fl.matlisp::fill! (self fm) (infinite fm))
    ;; (loop :for i :from 0 :below rows :do
    ;;    (loop :for j :from 0 :below cols :do
    ;;       (setf (matrix-ref fm i j) infinity)))
    fm))

(defgeneric make-identity-matrix (matrix order)
  (:documentation "Return an identity matrix of order ORDER."))

(defmethod make-identity-matrix ((matrix matrix) order)
  (setf matrix (make-zeros-matrix matrix order order))
  (loop :for i :from 0 :below order :do
     (setf (matrix-ref matrix i i) 1))
  matrix)

(defmethod make-identity-matrix ((fm fast-matrix) order)
  (setf (self fm) (fl.matlisp::eye order order 'single-float))
  fm)

;; Adapted from
;; https://rosettacode.org/wiki/Matrix-exponentiation_operator#Common_Lisp

(defgeneric matrix-power (matrix exp)
  (:documentation "Raise MATRIX to the power EXP and return the result."))

(defmethod matrix-power ((matrix matrix) exp)
  (let ((m-rows (matrix-n-rows matrix)))
    (cond
      ((/= m-rows (matrix-n-cols matrix)) (error "Non-square matrix"))
      ((zerop exp) (make-identity-matrix matrix m-rows))
      ((= 1 exp) (matrix-copy matrix))
      ((zerop (mod exp 2)) (let ((me2 (matrix-power matrix (/ exp 2))))
                             (matrix-product me2 me2)))
      (t (let ((me2 (matrix-power matrix (/ (1- exp) 2))))
           (matrix-product matrix (matrix-product me2 me2)))))))

(defgeneric to-adjacency-matrix (graph matrix)
  (:documentation "Return the adjacency matrix of GRAPH."))

(defmethod to-adjacency-matrix ((graph graph) (matrix matrix))
  (let ((node-index-hash (make-hash-table))
        (counter -1))
    (mapc (lambda (node) (setf (gethash node node-index-hash) (incf counter)))
          (nodes graph))
    (setf matrix (make-zeros-matrix matrix (+ counter 1) (+ counter 1)))
    (mapc (lambda-bind ((a b))
                  (setf (matrix-ref matrix
                                    (gethash a node-index-hash)
                                    (gethash b node-index-hash))
                        1)
                  (setf (matrix-ref matrix
                                    (gethash b node-index-hash)
                                    (gethash a node-index-hash))
                        1))
          (edges graph))
    matrix))

(defmethod to-adjacency-matrix ((graph digraph) (matrix matrix))
  (let ((node-index-hash (make-hash-table))
        (counter -1))
    (mapc (lambda (node) (setf (gethash node node-index-hash) (incf counter)))
          (nodes graph))
    (setf matrix (make-zeros-matrix matrix (+ counter 1) (+ counter 1)))
    (mapc (lambda-bind ((a b))
                  (setf (matrix-ref matrix
                                    (gethash a node-index-hash)
                                    (gethash b node-index-hash))
                        1))
          (edges graph))
    matrix))

(defmethod to-adjacency-matrix ((graph graph) (matrix fast-matrix))
  (let ((node-index-hash (make-hash-table))
        (counter -1)
        (one 1.0s0))
    (declare (type single-float one))
    (mapc (lambda (node) (setf (gethash node node-index-hash) (incf counter)))
          (nodes graph))
    (setf matrix (make-zeros-matrix matrix (+ counter 1) (+ counter 1)))
    (mapc (lambda-bind ((a b))
                  (setf (matrix-ref matrix
                                    (gethash a node-index-hash)
                                    (gethash b node-index-hash))
                        one)
                  (setf (matrix-ref matrix
                                    (gethash b node-index-hash)
                                    (gethash a node-index-hash))
                        one))
          (edges graph))
    matrix))

(defmethod to-adjacency-matrix ((graph digraph) (matrix fast-matrix))
  (let ((node-index-hash (make-hash-table))
        (counter -1)
        (one 1.0s0))
    (declare (type single-float one))
    (mapc (lambda (node) (setf (gethash node node-index-hash) (incf counter)))
          (nodes graph))
    (setf matrix (make-zeros-matrix matrix (+ counter 1) (+ counter 1)))
    (mapc (lambda-bind ((a b))
                  (setf (matrix-ref matrix
                                    (gethash a node-index-hash)
                                    (gethash b node-index-hash))
                        one))
          (edges graph))
    matrix))

(defgeneric to-reachability-matrix (graph matrix &key limit)
  (:documentation "Return the reachability matrix of the graph GRAPH.
  With the optional argument LIMIT set to an integer in the range 2 to
  two less than the number of nodes in GRAPH, produces a limited
  reachability matrix with paths of length LIMIT or less."))

(defmethod to-reachability-matrix ((graph graph) (matrix matrix) &key limit)
  (let ((n (length (nodes graph))))
    (assert (or (not limit)
                (and (integerp limit) (> limit 1) (< limit (- n 1))))
            (limit)
            "~S must be an integer between 2 and ~S"
            limit (- n 2))
    (let* ((result (make-identity-matrix (make-instance 'matrix) n))
           (max-power (or limit (- n 1)))
           (adjacency (to-adjacency-matrix graph (make-instance 'matrix)))
           (adjacency-powers (matrix-copy adjacency)))
      (setf result (matrix-sum adjacency result :boolean t))
      (loop :for i :from 2 :to max-power :do
         (setf adjacency-powers (matrix-product adjacency-powers adjacency))
         (setf result (matrix-sum adjacency-powers result :boolean t)))
      result)))

(defmethod to-reachability-matrix ((graph graph) (matrix fast-matrix) &key limit)
  (let ((n (length (nodes graph))))
    (assert (or (not limit)
                (and (integerp limit) (> limit 1) (< limit (- n 1))))
            (limit)
            "~S must be an integer between 2 and ~S"
            limit (- n 2))
    (let* ((result (make-identity-matrix (make-instance 'fast-matrix) n))
           (max-power (or limit (- n 1)))
           (adjacency (to-adjacency-matrix graph (make-instance 'fast-matrix)))
           (adjacency-powers (matrix-copy adjacency)))
      (setf result (matrix-sum adjacency result :boolean t))
      (loop :for i :from 2 :to max-power :do
         (setf adjacency-powers (matrix-product adjacency-powers adjacency))
         (setf result (matrix-sum adjacency-powers result :boolean t)))
      result)))

(defgeneric reachablep (graph rd from to)
  (:documentation "Given a graph GRAPH and a reachability matrix RD,
  returns t if node TO is reachable from node FROM, nil otherwise."))

(defmethod reachablep ((graph graph) (rd matrix) from to)
  (let ((node-index-hash (make-hash-table))
        (counter -1))
    (mapc (lambda (node) (setf (gethash node node-index-hash) (incf counter)))
          (nodes graph))
    (= 1 (matrix-ref rd (gethash from node-index-hash)
                     (gethash to node-index-hash)))))

(defgeneric reachable-from (graph rd from)
  (:documentation "Given a reachability matrix RD, return a list of
  the nodes in graph GRAPH reachable from node FROM."))

(defmethod reachable-from ((graph graph) (rd matrix) from)
  (let ((node-index-hash (make-hash-table))
        (counter -1)
        (result))
    (mapc (lambda (node) (setf (gethash node node-index-hash) (incf counter)))
          (nodes graph))
    (maphash #'(lambda (k v)
                 (unless
                     (= 0 (matrix-ref rd (gethash from node-index-hash) v))
                   (push k result)))
             node-index-hash)
    (reverse result)))

(defgeneric to-strong-component-matrix (rd)
  (:documentation "Given a reachability matrix of a digraph, RD,
  return a matrix in which the strong component of GRAPH containing
  node_i is given by the entries of 1 in the ith row (or column)."))

(defmethod to-strong-component-matrix ((rd matrix))
  (matrix-elementwise-product rd (matrix-transpose rd)))

(defgeneric strong-component-of (node graph strong-components)
  (:documentation "Return a list of nodes from graph GRAPH in the
  strong component that contains node NODE, as given by the strong
  component matrix STRONG-COMPONENTS."))

(defmethod strong-component-of (node (graph graph) (strong-components matrix))
  (let ((node-index-hash (make-hash-table))
        (counter -1)
        (result))
    (mapc (lambda (node) (setf (gethash node node-index-hash) (incf counter)))
          (nodes graph))
    (maphash #'(lambda (k v)
                 (unless (= 0 (matrix-ref
                                   strong-components
                                   (gethash node node-index-hash) v))
                   (push k result)))
             node-index-hash)
    (reverse result)))

(defgeneric to-distance-matrix (graph nd)
  (:documentation "Return the distance matrix ND of graph GRAPH."))

(defmethod to-distance-matrix ((graph graph) (nd matrix))
  (let* ((a (to-adjacency-matrix graph (make-instance 'matrix)))
         (a-power (to-adjacency-matrix graph (make-instance 'matrix)))
         (m (matrix-n-rows a))
         (finished)
         (zero 0)
         (one 1))
    (declare (type fixnum one))
    (declare (type fixnum zero))
    (setf nd (make-infinity-matrix nd m m))
    (loop :for i :from 0 :below m :do
       (setf (matrix-ref nd i i) zero))
    (loop :for i :from 0 :below m :do
       (loop :for j :from 0 :below m :do
          (when (= (matrix-ref a i j) one)
            (setf (matrix-ref nd i j) one))))
    (loop :for i :from 2 :to m :unless finished :do
       (setf a-power (matrix-product a a-power))
       (setf finished t)
       (loop :for j :from 0 :below m :do
          (loop :for k :from 0 :below m :do
             (when (and (infinitep (matrix-ref nd j k) nd)
                        (> (matrix-ref a-power j k) zero))
               (setf (matrix-ref nd j k) (coerce i 'fixnum))
               (setf finished nil)))))
    nd))

(defmethod to-distance-matrix ((graph graph) (nd fast-matrix))
  (let* ((a (to-adjacency-matrix graph (make-instance 'fast-matrix)))
         (a-power (to-adjacency-matrix graph (make-instance 'fast-matrix)))
         (m (matrix-n-rows a))
         (finished)
         (zero 0.0s0)
         (one 1.0s0))
    (declare (type single-float one))
    (declare (type single-float zero))
    (setf nd (make-infinity-matrix nd m m))
    (loop :for i :from 0 :below m :do
       (setf (matrix-ref nd i i) zero))
    (loop :for i :from 0 :below m :do
       (loop :for j :from 0 :below m :do
          (when (= (matrix-ref a i j) one)
            (setf (matrix-ref nd i j) one))))
    (loop :for i :from 2 :to m :unless finished :do
       (setf a-power (matrix-product a a-power))
       (setf finished t)
       (loop :for j :from 0 :below m :do
          (loop :for k :from 0 :below m :do
             (when (and (infinitep (matrix-ref nd j k) nd)
                        (> (matrix-ref a-power j k) zero))
               (setf (matrix-ref nd j k) (coerce i 'single-float))
               (setf finished nil)))))
    nd))

(defgeneric distance-from-to (graph nd from to)
  (:documentation "Returns the number of edges in graph GRAPH from
  node FROM to node TO, given the distance matrix ND."))

(defmethod distance-from-to ((graph graph) (nd matrix) from to)
  (let ((node-index-hash (make-hash-table))
        (counter -1))
    (mapc (lambda (node) (setf (gethash node node-index-hash) (incf counter)))
          (nodes graph))
    (matrix-ref nd
                (gethash from node-index-hash)
                (gethash to node-index-hash))))
