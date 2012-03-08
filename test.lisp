(in-package #:xdb2)

;;;;Test

(defparameter *tree* nil)

(defclass test-doc-non-storable ()
  ((id :initarg :id)
   (eid :initarg :eid)
   (aa :initarg :aa)
   (bb :initarg :bb)
   (cc :initarg :cc)
   (dd :initarg :dd)
   (ee :initarg :ee)
   (ff :initarg :ff)
   (hh :initarg :hh)
   (data :initarg :data
         :initform (make-hash-table)
         :accessor data)
   (key :initarg :key
        :initform nil
        :accessor key)
   (type :initarg :type
         :initform nil))
  )

(defclass test-doc-storable ()
  ((id :initarg :id)
   (eid :initarg :eid)
   (aa :initarg :aa)
   (bb :initarg :bb)
   (cc :initarg :cc)
   (dd :initarg :dd)
   (ee :initarg :ee)
   (ff :initarg :ff)
   (hh :initarg :hh)
   (data :initarg :data
         :initform (make-hash-table)
         :accessor data)
   (key :initarg :key
        :initform nil
        :accessor key)
   (type :initarg :type
         :initform nil))
  (:metaclass storable-class))


(defun make-doc-test (type key data)
  (let ((doc-obj (make-instance 'test-doc-storable :key key :type type)))
    (dolist (pair data)
      (setf (gethash (first pair) (data doc-obj)) (second pair)))
    doc-obj))

(defun test-store-doc (collection times)
  (dotimes (i times)
    (store-doc collection
               (make-doc-test
                "Test Doc"
                i
                (list
                 (list "id" i)
                 (list "eid" i)
                 (list "aa" (format nil "~R" (random 51234)))
                 (list "bb" (format nil "~R" (random 1234)))
                 (list "cc" (format nil "~R" (random 1234)))
                 (list "dd" (format nil "~R" (random 1234)))
                 (list "ee" (format nil "~R" (random 1234)))
                 (list "ff" (format nil "~R" (random 1234)))
                 (list "gg" (format nil "~R" (random 1234)))
                 (list "hh" (format nil "~R" (random 1234))))))))

(defun test (n)
  (let* ((db (make-instance 'xdb :location "/tmp/db-test/"))
         (col (add-collection db "test" :load-from-file-p nil)))
    (time (test-store-doc col n))
    ;; (time (snapshot db))
    ;; (time (sum col "eid"))
    ;; (time (find-doc col "eid" 50))
    ;; (time (sort-collection col))
    ))

(defun test-store-docx (collection times)
  (dotimes (i times)

      (store-doc collection  
               
                 (make-doc-test 
                  "Test Doc"
                  i
                  (list
                   (list "id" i)
                   (list "eid" i)
                   (list "aa" (random 51234))
                   (list "bb" (format nil "~R" (random 1234)))
                   (list "cc" (format nil "~R" (random 1234)))
                   (list "dd" (format nil "~R" (random 1234)))
                   (list "ee" (format nil "~R" (random 1234)))
                   (list "ff" (format nil "~R" (random 1234)))
                   (list "gg" (format nil "~R" (random 1234)))
                   (list "hh" (get-universal-time))))
                 )
      
      (if (equal (mod i 100000) 0)
          (sb-ext:gc :full t))))




(defun test-store-doc-storable-object (collection times)
  (dotimes (i times)
      (store-doc collection  
                 (make-instance 'test-doc-storable :key i :type "Test Doc"
                                :id i
                                :eid i
                                :aa (random 51234)
                                :bb (format nil "~R" (random 1234))
                                :cc (format nil "~R" (random 1234))
                                :dd (format nil "~R" (random 1234))
                                :ee (format nil "~R" (random 1234))
                                :ff (format nil "~R" (random 1234))
                                :hh (get-universal-time))
                 
                 )
      
      (if (equal (mod i 100000) 0)
          (sb-ext:gc :full t))))

(defun test-store-doc-non-storable-object (collection times)
  (dotimes (i times)
      (store-doc collection  
                 (make-instance 'test-doc-non-storable :key i :type "Test Doc"
                                :id i
                                :eid i
                                :aa (random 51234)
                                :bb (format nil "~R" (random 1234))
                                :cc (format nil "~R" (random 1234))
                                :dd (format nil "~R" (random 1234))
                                :ee (format nil "~R" (random 1234))
                                :ff (format nil "~R" (random 1234))
                                :hh (get-universal-time))
                 
                 )
      
      (if (equal (mod i 100000) 0)
          (sb-ext:gc :full t))))


(defun test-store-doc-hash (collection times)
  (dotimes (i times)
      (let ((hash (make-hash-table :test 'equal)))
        (setf (gethash 'key hash) i)
        (setf (gethash "id" hash) i)
        (setf (gethash "eid" hash) i)
        (setf (gethash "bb" hash) (format nil "~R" (random 1234)))
        (setf (gethash "cc" hash) (format nil "~R" (random 1234)))
        (setf (gethash "dd" hash) (format nil "~R" (random 1234)))
        (setf (gethash "ee" hash) (format nil "~R" (random 1234)))
        (setf (gethash "ff" hash) (format nil "~R" (random 1234)))
        (setf (gethash "stamp" hash) (get-universal-time))
        (store-doc collection hash))
      
      (if (equal (mod i 100000) 0)
          (sb-ext:gc :full t))))


(defun test-store-doc-list (collection times)
  (dotimes (i times)
      (store-doc collection (list
                             (list 'key i)
                             (list "id" i)
                             (list "eid" i)
                             (list "aa" (random 51234))
                             (list "bb" (format nil "~R" (random 1234)))
                             (list "cc" (format nil "~R" (random 1234)))
                             (list "dd" (format nil "~R" (random 1234)))
                             (list "ee" (format nil "~R" (random 1234)))
                             (list "ff" (format nil "~R" (random 1234)))
                             (list "gg" (format nil "~R" (random 1234)))
                             (list "stamp" (get-universal-time))))
      
      (if (equal (mod i 100000) 0)
          (sb-ext:gc :full t))))


#|

(defparameter db (make-instance 'xdb :location "/tmp/db-test/"))

(defparameter col-hash (add-collection db "test-hash" :load-from-file-p nil))
(format t "Hash Test~%")
(format t "Store~%")
(time (test-store-doc-hash col-hash 10000))
(format t "Sum~%")
(time (sum col-hash :element "id"))
(format t "Find~%")
(time (find-doc col-hash :test (lambda (doc) (equal (get-val doc "id") 500))))
(format t "Sort~%")
(time (sort-collection col-hash))

(defparameter col-list (add-collection db "test-list" :load-from-file-p nil))
(format t "List Test~%")
(format t "Store~%")
(time (test-store-doc-list col-list 10000))
(format t "Sum~%")
(time (sum col-list :element "id"))
(format t "Find~%")
(time (find-doc col-list :test (lambda (doc) (equal (get-val doc "id") 500))))
(format t "Sort~%")
(time (sort-collection col-list))

(defparameter col-object (add-collection db "test-object" :load-from-file-p nil))
(format t "Object non storable Test~%")
(format t "Store~%")
(time (test-store-doc-non-storable-object col-object 10000))
(format t "Sum~%")
(time (sum col-object :element 'id))
(format t "Find~%")
(time (find-doc col-object :test (lambda (doc) (equal (get-val doc 'id) 500))))
(format t "Sort~%")
(time (sort-collection col-object))

(defparameter col-object-storable (add-collection db "test-object-storable" :load-from-file-p nil))
(setf *fsync-data* nil)
(format t "Object storable Test~%")
(format t "Store~%")
(time (test-store-doc-storable-object col-object-storable 10000))
(format t "Sum~%")
(time (sum col-object-storable :element 'id))
(format t "Find~%")
(time (find-doc col-object-storable :test (lambda (doc) (equal (get-val doc 'id) 500))))
(format t "Sort~%")
(time (sort-collection col-object-storable))

(time (let ((x (sort-collection-temporary col)))
        (declare (ignore x))
        "done"))

|#





(defparameter *test-db* (list 1 2 3 4))

(defmacro with-db (spec &body body)
  "Locally establish a database connection, and bind *database* to it."
  `(let ((*test-db* ,spec))
    (unwind-protect (progn ,@body)
      )))

(defclass xxx ()
  ())

(defgeneric test-withxx (db &key test-db))

(defmethod  test-withxx ((collection xxx) &key (test-db *test-db*))
  (princ test-db))

(with-db (list 9 9 9) 
        (test-withxx (make-instance 'xxx)) )
