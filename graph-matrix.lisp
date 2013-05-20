;;; graph-matrix.lisp --- build and manipulate matrix graph representations

;; Copyright (C) Eric Schulte and Tom Dye 2013

;; Licensed under the Gnu Public License Version 3 or later

;;; Commentary

;; Functions for manipulating matrix graph representations.

;;; Code:
(in-package :graph-matrix)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

(defvar *infinity* 4294967295)

(defgeneric make-identity-matrix (order &key fast)
  (:documentation "Return an identity matrix of order ORDER.
If FAST, then use a femlisp matrix."))

(defmethod make-identity-matrix (order &key fast)
  (let ((matrix))
    (setf matrix (if fast 
                     (fl.function::eye order order '(unsigned-byte 32))
                     (make-array (list order order)
                                 :element-type '(unsigned-byte 32)
                                 :initial-element 0)))
    (or fast (loop :for i :from 0 :below order :do
           (setf (aref matrix i i) 1)))
    matrix))

(defgeneric make-universal-matrix (rows cols &key fast)
  (:documentation "Return a universal matrix with ROWS rows and COLS columns.
If FAST, then use a femlisp matrix."))

(defmethod make-universal-matrix (rows cols &key fast)
  (let ((matrix))
    (setf matrix
          (if fast
              (fl.function::ones rows cols '(unsigned-byte 32))
              (make-array (list rows cols)
                          :element-type '(unsigned-byte 32)
                          :initial-element 1)))
    matrix))

(defgeneric to-adjacency-matrix-new (graph &key fast)
  (:documentation "Return the adjacency matrix of GRAPH.
If FAST, then use a femlisp matrix.  If FAST, then use a femlisp matrix."))

(defmethod to-adjacency-matrix-new ((graph graph) &key fast)
  (let ((node-index-hash (make-hash-table))
        (counter -1))
    (mapc (lambda (node) (setf (gethash node node-index-hash) (incf counter)))
          (nodes graph))
    (let ((matrix (make-array (list (1+ counter) (1+ counter))
                               :element-type '(unsigned-byte 32)
                               :initial-element 0)))
      (and fast (setf matrix (make-instance
                              (fl.function::standard-matrix
                               '(unsigned-byte 32))
                              :content matrix)))
      (mapc (lambda-bind
             ((a b))
             (if fast
                 (progn
                   (setf (fl.function::mref matrix
                                            (gethash a node-index-hash)
                                            (gethash b node-index-hash))
                         1)
                   (setf (fl.function::mref matrix
                                            (gethash b node-index-hash)
                                            (gethash a node-index-hash))
                         1))
                 (progn
                   (setf (aref matrix
                               (gethash a node-index-hash)
                               (gethash b node-index-hash))
                         1)
                   (setf (aref matrix
                               (gethash b node-index-hash)
                               (gethash a node-index-hash))
                         1))))
            (edges graph))
      matrix)))

(defmethod to-adjacency-matrix-new ((graph digraph) &key fast)
  (let ((node-index-hash (make-hash-table))
        (counter -1))
    (mapc (lambda (node) (setf (gethash node node-index-hash) (incf counter)))
          (nodes graph))
    (let ((matrix (make-array (list (1+ counter) (1+ counter))
                               :element-type '(unsigned-byte 32)
                               :initial-element 0)))
      (and fast (setf matrix (make-instance
                              (fl.function::standard-matrix
                               '(unsigned-byte 32))
                              :content matrix)))
      (mapc (lambda-bind ((a b))
                         (if fast
                             (setf
                              (fl.function::mref matrix
                                                 (gethash a node-index-hash)
                                                 (gethash b node-index-hash))
                              1)
                             (setf (aref matrix
                                         (gethash a node-index-hash)
                                         (gethash b node-index-hash))
                                   1)))
            (edges graph))
      matrix)))

(defgeneric to-reachability-matrix (graph &key limit fast)
  (:documentation "Return the reachability matrix of the graph GRAPH.
  With the optional argument LIMIT set to an integer in the range 2 to
  two less than the number of nodes in GRAPH, produces a limited
  reachability matrix with paths of length LIMIT or less. If FAST,
  then use a femlisp matrix."))

;;; might check that LIMIT has been set sensibly, throw an error if not?

(defmethod to-reachability-matrix ((graph graph) &key limit fast)
  (let* ((len (length (nodes graph)))
         (result (make-identity-matrix len :fast fast))
         (max-power (or limit (- len 1)))
         (adjacency (to-adjacency-matrix-new graph :fast fast))
         (adjacency-powers (to-adjacency-matrix-new graph :fast fast)))
    (if fast (fl.function::m+! adjacency result)
        (loop :for i :from 0 :below (* len len) :do
           (setf (row-major-aref result i)
                 (+ (row-major-aref result i)
                    (row-major-aref adjacency i)))))
    (loop :for i :from 2 :to max-power :do
       (setf adjacency-powers
             (if fast (fl.function::m* adjacency-powers adjacency)
                 (mmult adjacency-powers adjacency)))
       (if fast (fl.function::m+! adjacency-powers result)
           (loop :for i :from 0 :below (* len len) :do
              (setf (row-major-aref result i)
                    (+ (row-major-aref result i)
                       (row-major-aref adjacency-powers i))))))
    (if fast (fl.function::for-each-row-key
              (lambda (x) (fl.function::for-each-key-in-row
                      (lambda (y) (and (< 1 (fl.function::mref result x y))
                                  (setf (fl.function::mref result x y) 1)))
                      result x)) result)
        (loop :for i :from 0 :below (* len len) :do
           (when (< 1 (row-major-aref result i))
             (setf (row-major-aref result i) 1))))
    result))

(defgeneric reachablep (graph reachability-matrix from to &key fast)
  (:documentation "Returns 1 if TO is reachable from FROM, 0 otherwise.
If FAST, then use a femlisp matrix."))

(defmethod reachablep ((graph graph) reachability-matrix from to &key fast)
  (let ((node-index-hash (make-hash-table))
        (counter -1))
    (mapc (lambda (node) (setf (gethash node node-index-hash) (incf counter)))
          (nodes graph))
    (if fast
        (fl.function::mref reachability-matrix
                           (gethash from node-index-hash)
                           (gethash to node-index-hash))
        (aref reachability-matrix
              (gethash from node-index-hash)
              (gethash to node-index-hash)))))

(defgeneric strong-components (rd &key fast)
  (:documentation "Given a reachability matrix of a digraph, RD,
  return a matrix in which the strong component of GRAPH containing
  node_i is given by the entries of 1 in the ith row (or column)."))

(defmethod strong-components (rd &key fast)
  (let ((rdprime (if fast
                     (fl.function::transpose rd)
                     (mtp rd)))
        (len (if fast
                 (fl.function::nrows rd)
                 (* (array-dimension rd 0)
                    (array-dimension rd 1))))
        (result (if fast
                    (fl.function::copy rd)
                    (copy-array rd :element-type '(unsigned-byte 32)))))
    (if fast
        (loop :for i :from 0 :below len :do
           (loop :for j :from 0 :below len :do
              (setf (fl.function::mref result i j)
                    (* (fl.function::mref rd i j)
                       (fl.function::mref rdprime i j)))))
        (loop :for i :from 0 :below len :do
           (setf (row-major-aref result i)
                 (* (row-major-aref rd i)
                    (row-major-aref rdprime i)))))
    result))

(defgeneric to-distance-matrix (graph &key fast)
  (:documentation "Return the distance matrix of GRAPH."))

(defmethod to-distance-matrix ((graph graph) &key fast)
  (let ((adj-mat (to-adjacency-matrix-new graph :fast fast))
        (result (make-array (list (length (nodes graph))
                                  (length (nodes graph)))
                      :element-type '(unsigned-byte 32)
                      :initial-element *infinity*))
        (adj-n (to-adjacency-matrix-new graph :fast fast))
        (len (length (nodes graph))))
    (if fast
        (setf result
              (make-instance (fl.function::standard-matrix '(unsigned-byte 32))
                             :content result)))
    (if fast
        (progn
          (loop :for i :from 0 :below len :do
             (setf (fl.function::mref result i i) 0))
          (loop :for i :from 0 :below len :do
             (loop :for j :from 0 :below len :do
                (when (= (fl.function::mref adj-mat i j) 1)
                  (setf (fl.function::mref result i j) 1))))
          (loop for i :from 2 :below len :do
               (setf adj-n (fl.function::m* adj-mat adj-n))
               (loop :for j :from 0 :below len :do
                  (loop :for k :from 0 :below len :do
                     (and (= (fl.function::mref result j k) *infinity*)
                          (> (fl.function::mref adj-n j k) 0)
                          (setf (fl.function::mref result j k) i))))))
        (progn
          (loop :for i :from 0 :below len :do
             (setf (aref result i i) 0))
          (loop :for i :from 0 :below (* len len) :do
             (when (= (row-major-aref adj-mat i) 1)
               (setf (row-major-aref result i) 1)))
          (loop :for i :from 2 :below len :do
             (setf adj-n (mmult adj-mat adj-n))
             (loop :for j :from 0 :below (* len len) :do
                (and (= (row-major-aref result j) *infinity*)
                     (> (row-major-aref adj-n j) 0)
                     (setf (row-major-aref result j) i))))))
    result))

(defun mmult (a b)
  (loop
     :with m = (array-dimension a 0)
     :with n = (array-dimension a 1)
     :with l = (array-dimension b 1)
     :with c = (make-array (list m l)
                           :initial-element 0
                           :element-type '(unsigned-byte 32))
     :for i :below m :do
     (loop :for k :below l :do
        (setf (aref c i k)
              (loop :for j :below n
                 :sum (* (aref a i j)
                         (aref b j k)))))
     :finally (return c)))

;; Transpose a mxn matrix A to a nxm matrix B=A'.
(defun mtp (A)
  (let* ((m (array-dimension A 0))
         (n (array-dimension A 1))
         (B (make-array `(,n ,m)
                        :initial-element 0
                        :element-type '(unsigned-byte 32))))
    (loop :for i :from 0 :below m :do
          (loop :for j :from 0 :below n :do
                (setf (aref B j i)
                      (aref A i j))))
    B))
