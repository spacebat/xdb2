(in-package #:xdb)

(defclass document ()
  ((collection :initarg :collection
               :accessor collection)
   (data :initarg :data 
         :initform (make-hash-table :test 'equal)
         :accessor data)
   (key :initarg :key
        :accessor key)
   (type :initarg :type
         :initform nil
         :accessor doc-type)))

(defgeneric get-docs (xdb collection-name)
  (:documentation "Returns the docs that belong to a collection."))

(defmethod get-docs ((db xdb) collection-name)
  (when (gethash collection-name (collections db))
    (docs (gethash collection-name (collections db)))))

(defgeneric get-doc (collection key)
  (:documentation "Returns the docs that belong to a collection."))

(defmethod get-doc (collection key)
  (when collection
    (map-docs collection 
              (lambda (doc) 
                (when (equal (key doc) key)
                  (return-from get-doc doc))))))

(defgeneric get-val (doc element &key data-type)
(:documentation "Returns the value in a doc that belongs to a collection."))

(defmethod get-val (doc element &key data-type)
  (declare (ignore data-type))
  (when doc 
    (break "get-val does no handle this type of document. Implement override."))
  )

(defmethod get-val ((doc document) element &key data-type)
  (when doc
    (typecase (or data-type (data doc))    
      (hash-table 
       (gethash element  (data doc) ))
      (standard-object 
       (slot-value (data doc) element))
      (t 
       (if data-type
           (cond ((equal 'alist data-type)
                  (assoc element (data doc)))
                 ((equal 'plist data-type)
                  (get (data doc) element )))                 
           (if (listp (data doc))
               (assoc element (data doc))))))))

(defmethod (setf get-val) (new-value (doc document) element &key data-type)
  
  (typecase (or data-type (data doc))    
    (hash-table (setf (gethash element (data doc)) new-value))
    (standard-object (setf (slot-value (data doc) element) new-value))
    (t 
     ;;TODO: implement the set
     (break "Not implemented")
    )))


(defgeneric set-val (doc element value &key data-type))

(defmethod set-val ((doc document) element value &key data-type)
  (typecase (or data-type (data doc))    
    (hash-table (setf (gethash element (data doc)) value))
    (standard-object (setf (slot-value (data doc) element) value))
    (t 
     (if data-type
         (cond ((equal 'alist data-type)
                (assoc element (data doc)))
               ((equal 'plist data-type)
                (get (data doc) element )))                 
         (if (listp (data doc))
             (assoc element (data doc)))))
    ))

(defgeneric sum (collection function)
  (:documentation "Applies the function to all the docs in the collection and returns the sum of
the return values."))

(defmethod sum ((collection collection) element)
  (let ((sum 0))
    (map-docs collection 
              (lambda (doc) 
                (setf sum (+ sum (get-val doc element)))))
    sum))

(defun find-doc (collection element value)
  (map-docs collection 
              (lambda (doc) 
                (when (equal (get-val doc element) value)
                  (return-from find-doc doc)))))

(defun find-docs (collection element value &key (test #'equal))
  (let ((docs nil))
    (map-docs collection 
              (lambda (doc) 
                (when (funcall test (get-val doc element) value)
                  (setf docs (append docs (list doc))))))
    docs))

(defun sort-key (doc)
  (key doc))

(defgeneric sort-collection (collection &key return-sort sort-value-func sort-test-func)
  )

(defmethod sort-collection ((collection collection) &key return-sort sort-value-func sort-test-func)
  (setf (docs collection) 
        (sort (docs collection) 
              (if sort-test-func
                  sort-test-func
                  #'>) 
              :key (if sort-value-func
                       sort-value-func
                       #'sort-key)))
  (if return-sort
      (docs collection)
      t))
