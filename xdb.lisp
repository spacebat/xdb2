(in-package #:xdb2)

(defclass xdb ()
  ((location :initarg :location
             :accessor location
             :initform (error "Location is required"))
   (collections :initarg :collections
                :accessor collections
                :initform (make-hash-table :test 'equal))))

(defclass collection ()
  ((name :initarg :name
         :accessor name)
   (path :initarg :path
         :accessor path)
   (docs :initarg :docs
         :accessor docs)))

(defclass dbs ()
  ((databases :initarg :databases
        :accessor databases
        :initform (make-hash-table :test 'equal))
   (base-path :initarg :base-path
              :initform "/tmp/xdb2/dbs/"
              :accessor base-path)))

(defgeneric get-db (dbs name)
    (:documentation "Returns the xdb by name."))

(defmethod get-db ((dbs dbs) name)
  (gethash name (databases dbs)))

(defgeneric add-db (dbs name &key base-path load-from-file-p)
  (:documentation "Adds a xdb to the dbs hashtable. A base-path can be 
supplied here that is independatn of the dbs base-path so that a 
database collection can be build that spans multiple disks etc."))

(defmethod add-db ((db dbs) name &key base-path load-from-file-p)
  (unless (gethash name (databases db))
    (let ((path (or base-path (base-path db))))    
      (if (listp name)
          (dolist (part name)
            (setf path (format nil "~A/~A/" path (string-downcase part))))
          (setf path (format nil path name)))

      (ensure-directories-exist path)
      (setf (gethash name
                     (databases db))
            (make-instance 'xdb 
                           :location path))
      (load-db (gethash name (databases db)) :load-from-file-p load-from-file-p))))

(defparameter *dbs* nil)

(defun dbs ()
  *dbs*)

(defgeneric initialize-doc-container (collection)
  (:documentation
   "Create the docs container and set the collection's docs to the container.
If you specialize this then you have to specialize add-doc, store-doc,
sort-collection, sort-collection-temporary and union-collection. "))

(defmethod initialize-doc-container ((collection collection))
  (setf (docs collection) (make-array 0 :adjustable t :fill-pointer 0)))


(defgeneric map-docs (function collection &rest more-collections &key &allow-other-keys )
  (:documentation "Applies the function accross all the documents in the collection"))

(defmethod map-docs (function (collection collection) &rest more-collections 
                     &key result-type)
  (let ((more (if more-collections
                  (map 'vector (lambda (col) (docs col))
                       (cdr more-collections)))))
    (map result-type function (docs collection) more)))


(defgeneric duplicate-doc-p (doc test-doc))

(defgeneric find-duplicate-doc (collection doc &key func)
  (:documentation "Load collection from a file."))

(defmethod find-duplicate-doc ((collection collection) doc &key func)
  (let ((test (or func #'duplicate-doc-p)))
    (map-docs 
     (lambda (docx)
       (when (funcall test doc docx)
         (return-from find-duplicate-doc docx)))
     collection)))

(defgeneric add-doc (collection doc &key duplicate-doc-p-func)
  (:documentation "Add a document to the docs container."))

(defmethod add-doc ((collection collection) doc &key duplicate-doc-p-func)
  (when doc
    (if duplicate-doc-p-func
        (let ((dup (find-duplicate-doc collection doc :func duplicate-doc-p-func)))
          (if (not dup)
              (vector-push-extend doc (docs collection))
              (setf dup doc) ;;doing this because 
              ))
        (vector-push-extend doc (docs collection)))))


(defgeneric store-doc (collection doc &key duplicate-doc-p-func)
  (:documentation "Serialize the doc to file and add it to the collection."))

(defmethod store-doc ((collection collection) doc
                      &key (duplicate-doc-p-func 'duplicate-doc-p))
  (let ((dup (and duplicate-doc-p-func
                  (find-duplicate-doc collection doc
                                      :func duplicate-doc-p-func))))
    (if dup
        (setf dup doc)
        (vector-push-extend doc (docs collection)))
    (serialize-doc collection doc))
  collection)

(defgeneric serialize-doc (collection doc &key)
  (:documentation "Serialize the doc to file."))

(defmethod serialize-doc ((collection collection) doc &key)
  (let ((path (make-pathname :type "log" :defaults (path collection))))
    (ensure-directories-exist path)
    (save-doc collection doc path))
  doc)

(defgeneric serialize-docs (collection &key duplicate-doc-p-func)
  (:documentation "Store all the docs in the collection on file and add it to the collection."))

(defmethod serialize-docs (collection &key duplicate-doc-p-func)
  (map-docs 
   (lambda (doc)
     (store-doc collection doc
                :duplicate-doc-p-func duplicate-doc-p-func))
   collection))

(defgeneric load-from-file (collection file)
  (:documentation "Load collection from a file."))

(defmethod load-from-file ((collection collection) file)
  (ensure-directories-exist file)
  (load-data collection file
             (lambda (object)
               (add-doc collection object
                        :duplicate-doc-p-func #'duplicate-doc-p))))

(defgeneric get-collection (xdb name)
    (:documentation "Returns the collection by name."))

(defmethod get-collection ((db xdb) name)
  (gethash name (collections db)))


(defgeneric add-collection (xdb name &key load-from-file-p)
  (:documentation "Adds a collection to the db."))

(defun make-new-collection (name db)
  (let ((collection
          (make-instance 'collection
                         :name name
                         :path (merge-pathnames name (location db)))))
    (initialize-doc-container collection)
    collection))

(defmethod add-collection ((db xdb) name &key load-from-file-p)
  (let ((collection (or (gethash name (collections db))
                        (setf (gethash name (collections db))
                              (make-new-collection name db)))))
    (ensure-directories-exist (path collection))
    (when load-from-file-p
      (load-from-file collection
                      (make-pathname :defaults (path collection)
                                     :type "snapshot")))
    collection))

(defgeneric snapshot (collection)
  (:documentation "Write out a snapshot."))

(defun file-date ()
  "Returns current date as a string."
  (multiple-value-bind (sec min hr day mon yr)
                       (get-decoded-time)
    (format nil "~A~A~A_~A~A~A" yr mon day hr min sec)))

(defun append-date (name)
  (format nil "~a-~a" name (file-date)))

(defmethod snapshot ((collection collection))
  (let* ((backup (merge-pathnames "backup/" (path collection)))
         (log (make-pathname :type "log" :defaults (path collection)))
         (snap (make-pathname :type "snap" :defaults (path collection)))
         (backup-name (append-date (name collection)))
         (log-backup (make-pathname :name backup-name
                                    :type "log"
                                    :defaults backup))
         (snap-backup (make-pathname :name backup-name
                                     :type "snap"
                                     :defaults backup)))
    (ensure-directories-exist backup)
    (when (probe-file snap)
      (rename-file snap snap-backup))
    (when (probe-file log)
      (rename-file log log-backup))
    (save-data collection snap)))

(defmethod snapshot ((db xdb))
  (maphash (lambda (key value)
             (declare (ignore key))
             (snapshot value))
           (collections db)))

(defgeneric load-db (xdb &key load-from-file-p)
  (:documentation "Loads all the collections in a location."))

(defmethod load-db ((db xdb) &key load-from-file-p)
  (let ((unique-collections (make-hash-table :test 'equal)))
    (dolist (path (directory (format nil "~A/*.*" (location db))))
      (setf (gethash (pathname-name path) unique-collections) (pathname-name path)))
    (maphash  #'(lambda (key value)
                  (declare (ignore key))
               (add-collection db value :load-from-file-p load-from-file-p))
             unique-collections)))

(defgeneric get-docs (xdb collection-name &rest more-collections 
                          &key return-type &allow-other-keys)
  (:documentation "Returns the docs that belong to a collection."))

(defmethod get-docs ((db xdb) collection-name &rest more-collections 
                     &key (return-type 'vector))

  (apply #'map-docs 
         (lambda (col-name)
           (docs (gethash col-name (collections db))))
         collection-name
         (cdr more-collections)
         :return-type  return-type))

(defgeneric get-doc (collection &rest more-collections &key test &allow-other-keys)
  (:documentation "Returns the docs that belong to a collection."))

(defmethod get-doc (collection &rest more-collections 
                    &key (element 'key) value (test #'equal) )
  (apply #'map-docs 
         (lambda (doc)
           (when (apply test (get-val doc element) value)
             (return-from get-doc doc)))
         collection
         (cdr more-collections)))

(defgeneric find-doc (collection &rest more-collections &key test &allow-other-keys)
  (:documentation "Returns the first doc that matches the test."))

(defmethod find-doc ((collection collection) &rest more-collections &key test )
  (apply #'map-docs 
         (lambda (doc)
       (when (apply test doc)
         (return-from find-doc doc)))
         collection
         (cdr more-collections)))

(defgeneric find-docs (collection &rest more-collections &key test &allow-other-keys)
  (:documentation "Returns a list of all the docs that matches the test."))

(defmethod find-docs ((collection collection) &rest more-collections 
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

(defclass union-docs ()
  ((docs :initarg :docs
         :accessor :docs)))

(defgeneric union-collection (collection &rest more-collections 
                                         &key return-type &allow-other-keys))

(defmethod union-collection ((collection collection) &rest more-collections 
                             &key (return-type 'vector))
  (make-instance 'union-docs :docs (concatenate return-type collection more-collections)))


(defmethod find-docs ((collection union-docs) &key test element value (result-type 'vector))
  (map-docs collection
              (lambda (doc)
                (when (if test
                          (apply test doc element value)
                          (equal (get-val doc element) value))))
              :result-type result-type))


(defclass join-docs ()
  ((docs :initarg :docs
          :accessor :docs)))

(defclass join-result ()
  ((docs :initarg :docs
          :accessor :docs)))



(defun sort-key (doc)
  (get-val doc 'key))

(defgeneric sort-collection (collection &key return-sort sort-value-func sort-test-func)
  (:documentation "This sorts the collection 'permanantly'."))

;;TODO: How to update log if collection is sorted? Make a snapshot?

(defmethod sort-collection ((collection collection) 
                            &key return-sort sort-value-func sort-test-func)
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

(defgeneric sort-collection-temporary (collection &key sort-value-func sort-test-func)
  (:documentation "This does not sort the actual collection but returns an array 
of sorted docs."))

(defmethod sort-collection-temporary ((collection collection) 
                            &key sort-value-func sort-test-func)
  (let ((sorted-array (copy-array (docs collection))))
   (setf sorted-array 
         (sort sorted-array 
               (if sort-test-func
                   sort-test-func
                   #'>) 
               :key (if sort-value-func
                        sort-value-func
                        #'sort-key)))
   sorted-array))
;;Add method for validation when updating a collection.

