(in-package :xdb2)

(defparameter *test-db* (make-instance 'xdb :location "/tmp/db-test2/"))
(defparameter *test-col* (add-collection *test-db* "test" :load-from-file-p nil))

(defclass foo ()
  ((version :initarg :vcersion
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
         :accessor slot))
  (:metaclass storable-class))
