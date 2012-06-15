(in-package :xdb2)

(defclass foo ()
  ((version :initarg :version
            :initform 1
            :accessor version)))

(defclass standard-test ()
  ((foo :initarg :foo
        :initform (make-instance 'foo)
        :accessor foo) 
   (slot :initarg :slot
         :initform '("An" 1 #(0 -3) 1d0 #(1/4 foo) . #\s)
         :accessor slot)))

(defclass storable-test ()
  ((slot :initarg :slot
         :initform '("An" 1 #(0 -3) 1d0 #(1/4 'foo) . #\s)
         :accessor slot)
   (foo :initarg :foo
        :initform nil
        :accessor foo))
  (:metaclass storable-class))

(defclass boo ()
  ((slot :initarg :slot
         :initform '("An" 1 #(0 -3) 1d0 #(1/4 'foo) . #\s)
         :accessor slot)
   (foo :initarg :foo
        :initform nil
        :accessor foo))
  (:metaclass storable-class))

(defparameter *test-db* (make-instance 'xdb :location "/tmp/db-test2/"))
(defparameter *test-col* (add-collection *test-db* "test" :load-from-file-p t))

(defun storable-test ()
  (let ((a (make-instance 'storable-test)))
    (store-doc *test-col* (make-instance 'boo :foo a))))
