(in-package #:xdb2)

(defclass document ()
  ((collection :initarg :collection
               :accessor collection
               :storep nil)
   (key :initarg :key
        :accessor key)
   (type :initarg :type
         :initform nil
         :accessor doc-type))
  (:metaclass storable-class))

(defmethod duplicate-doc-p ((doc document) test-doc)
  (or (eq doc test-doc)
      (equal (key doc) (key test-doc))))

(defgeneric add (doc &key collection duplicate-doc-p-func)
  (:documentation "Add a document to the docs container."))

(defmethod add ((doc document) &key collection duplicate-doc-p-func)
  (when doc
    (if (slot-boundp doc 'collection)
        (add-doc (or (collection doc) collection) (or duplicate-doc-p-func  #'duplicate-doc-p))
      (error "Must specify collection to add document to."))))

(defmethod get-val ((doc document) element &key data-type)
  (declare (ignore data-type))
  (if (slot-boundp doc element)
      (slot-value doc element)))

(defmethod (setf get-val) (new-value (doc document) element &key data-type)
  (declare (ignore data-type))
  (if doc
      (setf (slot-value doc element) new-value)))



