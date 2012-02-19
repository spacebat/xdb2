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

(defmethod get-doc ((collection document-join) &rest more-collections
                     &key (test #'equal) (element 'key) value)
  (when collection
    (map-docs 
              (lambda (doc)
                (when (apply test (get-val doc element) value)
                  (return-from get-doc doc)))
              (append (list collection) more-collections))))

(defmethod find-doc ((collection document-join) &rest more-collections
                     &key test )
  (apply #'map-docs 
         (lambda (doc)
           (when (apply test doc)
             (return-from find-doc doc)))
         collection
         (cdr more-collections)))

(defmethod find-docs ((collection document-join) &rest more-collections 
                      &key test element value (return-type 'vector))
  (apply #'map-docs 
         (lambda (doc)
           (when (if test
                     (apply test doc element value)
                     (equal (get-val doc element) value))
             ))
         collection
         (cdr more-collections)
         :return-type  return-type))