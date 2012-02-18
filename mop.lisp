;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:xdb2)

(defclass storable-class (standard-class)
  ((class-id :initform nil
             :accessor class-id)
   (slots-to-store :initform nil
                   :accessor slots-to-store)
   (slot-locations-and-initforms
    :initform nil
    :accessor slot-locations-and-initforms)
   (slot-locations-and-initforms-read
    :initform nil
    :accessor slot-locations-and-initforms-read)
   (all-slot-locations-and-initforms
    :initform nil
    :accessor all-slot-locations-and-initforms)
   (initforms :initform #()
	      :accessor class-initforms)
   (objects :initform nil
            :accessor objects-of-class)
   (storage :initform nil
            :initarg :storage
            :accessor class-storage)))

(defun initialize-storable-class (next-method class &rest args
                                  &key direct-superclasses &allow-other-keys)
  (apply next-method class
         (if direct-superclasses
             args
             (list* :direct-superclasses (list (find-class 'identifiable))
                    args))))

(defmethod initialize-instance :around ((class storable-class)
                                        &rest args)
  (apply #'initialize-storable-class #'call-next-method class args))

(defmethod reinitialize-instance :around ((class storable-class)
                                          &rest args)
  (apply #'initialize-storable-class #'call-next-method class args))

;;;

(defmethod validate-superclass
    ((class standard-class)
     (superclass storable-class))
  t)

(defmethod validate-superclass
    ((class storable-class)
     (superclass standard-class))
  t)

(defclass storable-slot-mixin ()
  ((storep :initarg :storep
           :initform t
           :accessor store-slot-p)))

(defclass storable-direct-slot-definition (storable-slot-mixin
                                           standard-direct-slot-definition)
  ())

(defclass storable-effective-slot-definition
    (storable-slot-mixin standard-effective-slot-definition)
  ())

(defmethod direct-slot-definition-class ((class storable-class)
                                         &rest initargs)
  (declare (ignore initargs))
  (find-class 'storable-direct-slot-definition))

(defmethod effective-slot-definition-class ((class storable-class)
                                            &key &allow-other-keys)
  (find-class 'storable-effective-slot-definition))

(defmethod compute-effective-slot-definition
    ((class storable-class) slot-name direct-definitions)
  (declare (ignore slot-name))
  (let ((effective-definition (call-next-method))
        (direct-definition (car direct-definitions)))
    (setf (store-slot-p effective-definition)
          (store-slot-p direct-definition))
    effective-definition))

(defun make-slots-cache (slot-definitions)
  (map 'vector
       (lambda (slot-definition)
	 (cons (slot-definition-location slot-definition)
	       (slot-definition-initform slot-definition)))
       slot-definitions))

(defun initialize-class-slots (class slots)
  (let* ((slots-to-store (coerce (remove-if-not #'store-slot-p slots)
                                 'simple-vector)))
    (setf (slots-to-store class)
          slots-to-store)
    (setf (slot-locations-and-initforms class)
          (make-slots-cache slots-to-store))
    (setf (all-slot-locations-and-initforms class)
          (make-slots-cache slots))
    (setf (class-initforms class)
          (map 'vector #'slot-definition-initform slots))))

(defmethod compute-slots :around ((class storable-class))
  (let ((slots (call-next-method)))
    (initialize-class-slots class slots)
    slots))

;;;

(defclass identifiable (standard-object)
  ((id :accessor id
       :initform 0
       :storep nil))
  (:metaclass storable-class))
