(in-package #:xdb2)

(defclass document ()
  ((collection :initarg :collection
               :accessor collection)
   (key :initarg :key
        :accessor key)
   (doc-type :initarg :doc-type
         :initform nil
         :accessor doc-type)))

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
      (slot-val doc element)))

(defmethod (setf get-val) (new-value (doc document) element &key data-type)
  (declare (ignore data-type))
  (if doc
      (setf (slot-value doc element) new-value)))

(defclass document-join (join-docs)
  ())

(defclass document-join-result (join-result)
  ())

(defmethod get-val ((composite-doc document-join-result) element &key data-type)
  (declare (ignore data-type))
  (map 'list
       (lambda (doc)
         (cons (doc-type doc) (get-val doc element)))
       (docs composite-doc)))


(defmethod get-doc ((collection document-join) value &key (element 'key) (test #'equal))
  (map-docs
         nil
         (lambda (doc)
           (when (apply test (get-val doc element) value)
             (return-from get-doc doc)))
         collection))


(defmethod find-doc ((collection document-join) &key test)
  (if test
      (map-docs
       nil
       (lambda (doc)
         (when (apply test doc)
           (return-from find-doc doc)))
       collection)))

