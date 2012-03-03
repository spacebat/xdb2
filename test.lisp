(in-package #:xdb2)

;;;;Test

(defparameter *tree* nil)

(defun make-doc-test (type key data)
  (let ((doc-obj (make-instance 'test-docx :key key :type type)))
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

(defclass test-docx ()
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


(defun test-store-docxx (collection times)
  (dotimes (i times)

      (store-doc collection  
                 (make-instance 'test-docx :key i :type "Test Doc"
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


#|

(defparameter db (make-instance 'xdb :location "/tmp/db-test/"))
(defparameter col (add-collection db "test" :load-from-file-p nil))
(time (test-store-docxx col 10000))
(time (sum col  :element 'eid))
(time (find-doc col :test (lambda (doc) (equal (get-val doc 'eid) 50))))
(time (sort-collection col))
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
