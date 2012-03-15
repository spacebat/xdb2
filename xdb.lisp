(in-package #:xdb2)

(defclass xdb ()
  ((location :initarg :location
             :accessor location
             :initform (error "Location is required"))
   (collections :initarg :collections
                :accessor collections
                :initform (make-hash-table :test 'equal))))

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

(defun parse-db-path (path)
  (make-pathname :directory
                 (list* :relative
                        (etypecase path
                          (cons path
                           path)
                          (string path
                           (list path))))))

(defmethod add-db ((dbs dbs) name &key base-path load-from-file-p)
  (unless (gethash name (databases dbs))
    (let* ((base-path (or base-path (base-path dbs)))
           (db-path (merge-pathnames (parse-db-path name) base-path))
           (db (make-instance 'xdb :location db-path)))
      (ensure-directories-exist db-path)
      (setf (gethash name (databases dbs)) db)
      (if load-from-file-p
          (load-db db :load-from-file-p load-from-file-p)))))

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


(defgeneric map-docs (result-type function collection &rest more-collections)
  (:documentation
   "Applies the function accross all the documents in the collection"))

(defmethod map-docs (result-type function (collection collection)
                     &rest more-collections)
  (let ((result
          (map result-type function (docs collection))))
    (loop for collection in more-collections
          for results = (map result-type function (docs collection))
          if result-type
          do (setf result (concatenate result-type result results)))
    result))


(defgeneric duplicate-doc-p (doc test-doc)
  (:method ((a t) (b t))))

(defgeneric find-duplicate-doc (collection doc &key function)
  (:documentation "Load collection from a file."))

(defmethod find-duplicate-doc ((collection collection) doc &key function)
  (let ((test (or function #'duplicate-doc-p)))
    (map-docs
     nil
     (lambda (docx)
       (when (funcall test doc docx)
         (return-from find-duplicate-doc docx)))
     collection)))

(defgeneric add-doc (collection doc &key duplicate-doc-p-func)
  (:documentation "Add a document to the docs container."))

(defmethod add-doc ((collection collection) doc &key duplicate-doc-p-func)
  (when doc
    (if duplicate-doc-p-func
        (let ((dup (find-duplicate-doc collection doc :function duplicate-doc-p-func)))
          (if (not dup)
              (vector-push-extend doc (docs collection))
              (setf dup doc) ;;doing this because
              ))
        (vector-push-extend doc (docs collection)))))


(defgeneric store-doc (collection doc &key duplicate-doc-p-func)
  (:documentation "Serialize the doc to file and add it to the collection."))

(defgeneric serialize-doc (collection doc &key)
  (:documentation "Serialize the doc to file."))

(defmethod store-doc :before ((collection collection) (doc identifiable) &rest args)
  (declare (ignore args))
  (unless (id doc)
    (setf (id doc) (last-id collection))
    (incf (last-id collection))))

(defmethod store-doc ((collection collection) doc
                      &key (duplicate-doc-p-func #'duplicate-doc-p))
  (let ((dup (and duplicate-doc-p-func
                  (find-duplicate-doc collection doc
                                      :function duplicate-doc-p-func))))
    ;; a document might be considered duplicate based on the data 
    ;;contained and not its eql status as lisp object so we have to replace
    ;;it in the array with the new object effectively updating the data.
    (if dup
        (setf dup doc)
        (vector-push-extend doc (docs collection)))
    (serialize-doc collection doc))
  collection)

(defmethod serialize-doc ((collection collection) doc &key)
  (let ((path (make-pathname :type "log" :defaults (path collection))))
    (ensure-directories-exist path)
    (save-doc collection doc path))
  doc)

(defgeneric serialize-docs (collection &key duplicate-doc-p-func)
  (:documentation "Store all the docs in the collection on file and add it to the collection."))

(defmethod serialize-docs (collection &key duplicate-doc-p-func)
  (map-docs
   nil
   (lambda (doc)
     (store-doc collection doc
                :duplicate-doc-p-func duplicate-doc-p-func))
   collection))

(defgeneric load-from-file (collection file)
  (:documentation "Load collection from a file."))

(defmethod load-from-file ((collection collection) file)
  (when (probe-file file)
    (load-data collection file
               (lambda (object)
                 (add-doc collection object
                          :duplicate-doc-p-func #'duplicate-doc-p)))))

(defgeneric get-collection (xdb name)
    (:documentation "Returns the collection by name."))

(defmethod get-collection ((db xdb) name)
  (gethash name (collections db)))


(defgeneric add-collection (xdb name &key load-from-file-p)
  (:documentation "Adds a collection to the db."))

(defun make-new-collection (name db &key collection-class)
  (let ((collection
         (make-instance collection-class
                         :name name
                         :path (merge-pathnames name (location db)))))
    (initialize-doc-container collection)
    collection))

(defmethod add-collection ((db xdb) name
                           &key (collection-class 'collection) load-from-file-p)
  (let ((collection (or (gethash name (collections db))
                        (setf (gethash name (collections db))
                              (make-new-collection name db
                                                   :collection-class collection-class)))))
    (ensure-directories-exist (path collection))
    (when load-from-file-p
      (load-from-file collection
                      (make-pathname :defaults (path collection)
                                     :type "snap"))
      (load-from-file collection
                      (make-pathname :defaults (path collection)
                                     :type "log")))
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
      (when (pathname-name path)
        (setf (gethash (pathname-name path) unique-collections)
              (pathname-name path))))
    (maphash  #'(lambda (key value)
                  (declare (ignore key))
                  (add-collection db value :load-from-file-p load-from-file-p))
              unique-collections)))

(defgeneric get-docs (xdb collection-name &key return-type &allow-other-keys)
  (:documentation "Returns the docs that belong to a collection."))

(defmethod get-docs ((db xdb) collection-name &key return-type)
  (let ((col (gethash collection-name (collections db))))
    (if return-type
        (coerce return-type
                (docs col))
        (docs col))))

(defgeneric get-doc (collection value  &key element test)
  (:documentation "Returns the docs that belong to a collection."))

(defmethod get-doc (collection value  &key (element 'key) (test #'equal))
  (map-docs
         nil
         (lambda (doc)
           (when (funcall test (get-val doc element) value)
             (return-from get-doc doc)))
         collection))

(defgeneric get-doc-complex (test element value collection  &rest more-collections)
  (:documentation "Returns the docs that belong to a collection."))

(defmethod get-doc-complex (test element value collection &rest more-collections)
  (apply #'map-docs
         nil
         (lambda (doc)
           (when (apply test (list (get-val doc element) value))
             (return-from get-doc-complex doc)))
         collection
         more-collections))

(defgeneric get-doc-simple (element value collection  &rest more-collections)
  (:documentation "Returns the docs that belong to a collection."))


(defgeneric find-doc (collection &key test)
  (:documentation "Returns the docs that belong to a collection."))

(defmethod find-doc (collection &key test)
  (if test
      (map-docs
       nil
       (lambda (doc)
         (when (funcall test doc)
           (return-from find-doc doc)))
       collection)))

(defgeneric find-doc-complex (test collection &rest more-collections)
  (:documentation "Returns the first doc that matches the test."))

(defmethod find-doc-complex (test collection &rest more-collections)
  (apply #'map-docs
         (lambda (doc)
           (when (funcall test doc)
             (return-from find-doc-complex doc)))
         collection
         (cdr more-collections)))

(defgeneric find-docs (return-type test collection &rest more-collections)
  (:documentation "Returns a list of all the docs that matches the test."))

(defmethod find-docs (return-type test (collection collection) &rest more-collections )
  (apply #'map-docs
         return-type
         (lambda (doc)
           (when (funcall test doc)
             doc))
         collection
         more-collections))

(defclass union-docs ()
  ((docs :initarg :docs
         :accessor :docs)))

(defgeneric union-collection (return-type collection &rest more-collections))

(defmethod union-collection (return-type (collection collection) &rest more-collections)
  (make-instance
   'union-docs
   :docs (apply #'map-docs (list return-type collection more-collections))))

(defmethod find-docs (return-type test (collection union-docs)  &rest more-collections)
  (apply #'map-docs
         return-type
         (lambda (doc)
           (when (apply test doc)
             doc))
         collection
         more-collections))

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



(defclass xdb-sequence ()
  ((key :initarg :key
         :accessor key)
   (value :initarg :value
          :accessor value)))


(defgeneric enable-sequences (xdb))

(defmethod enable-sequences ((xdb xdb))
  (add-collection xdb "sequences" 
                :collection-class 'collection
                :load-from-file-p t))

(defgeneric next-sequence (xdb key))

(defmethod next-sequence ((xdb xdb) key)
  (let ((doc (get-doc (get-collection xdb "sequences") key)))
    (unless doc
      (setf doc (make-instance 'xdb-sequence :key key :value 0)))
    (incf (get-val doc 'value))
    (store-doc (get-collection xdb "sequences")
                doc)
    (get-val doc 'value)))