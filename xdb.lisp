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

(defgeneric initialize-doc-container (collection)
  (:documentation
   "Create the docs container and set the collection's docs to the container."))

(defmethod initialize-doc-container ((collection collection))
  (setf (docs collection) (make-array 0 :adjustable t :fill-pointer 0)))

(defgeneric add-doc (collection doc &key check-duplicate-function)
  (:documentation "Add a document to the docs container."))

(defmethod add-doc ((collection collection) doc &key check-duplicate-function)
  (when doc
    (setf (collection doc) (name collection))
    (if check-duplicate-function
        (let ((dup (find-duplicate-doc collection doc :func check-duplicate-function)))
          (if (not dup)
              (vector-push-extend doc (docs collection))
              (setf (data dup) (data doc))))
        (vector-push-extend doc (docs collection)))))

(defgeneric map-docs (collection function)
  (:documentation "Applies the function accross all the documents in the collection"))

(defmethod map-docs ((collection collection) function)
  (map nil function (docs collection)))

(defun duplicate-doc-p (doc dox)
  (or (eq doc dox)
      (equal (key doc) (key dox))))

(defgeneric find-duplicate-doc (collection doc &key func)
  (:documentation "Load collection from a file."))

(defmethod find-duplicate-doc ((collection collection) doc &key func)
  (let ((test (or func #'duplicate-doc-p)))
    (map-docs collection
              (lambda (docx)
                (when (funcall test doc docx)
                  (return-from find-duplicate-doc docx))))))

(defgeneric store-doc (collection doc &key check-duplicate-function)
  (:documentation "Serialize the doc to file and add it to the collection."))

(defmethod store-doc ((collection collection) doc
                      &key (check-duplicate-function 'duplicate-doc-p))
  (let ((dup (and check-duplicate-function
                  (find-duplicate-doc collection doc
                                      :func check-duplicate-function))))
    (if dup
        (setf (data dup) (data doc))
        (vector-push-extend doc (docs collection)))
    (serialize-doc collection doc))
  collection)

(defgeneric serialize-doc (collection doc &key)
  (:documentation "Serialize the doc to file."))

(defmethod serialize-doc ((collection collection) doc &key)
  (let ((path (make-pathname :type "log" :defaults (path collection))))
    (ensure-directories-exist path)
    (save-doc collection doc path))
  collection)

(defgeneric serialize-docs (collection &key stream check-duplicate-function)
  (:documentation "Store all the docs in the collection on file and add it to the collection."))

(defmethod serialize-docs (collection &key stream check-duplicate-function)
  (map-docs collection
            (lambda (doc)
              (store-doc collection doc
                         :stream stream
                         :check-duplicate-function check-duplicate-function))))

(defgeneric load-from-file (collection file)
  (:documentation "Load collection from a file."))

(defmethod load-from-file ((collection collection) file)
  (ensure-directories-exist file)
  (load-data collection file
             (lambda (object)
               (add-doc collection object
                        :check-duplicate-function #'duplicate-doc-p))))

(defgeneric get-collection (xdb name)
    (:documentation "Returns the collection by name."))

(defmethod get-collection ((db xdb) name)
  (gethash name (collections db)))


(defgeneric add-collection (xdb name &key load-from-file)
  (:documentation "Adds a collection to the db."))

(defun make-new-collection (name db)
  (let ((collection
          (make-instance 'collection
                         :name name
                         :path (merge-pathnames name (location db)))))
    (initialize-doc-container collection)
    collection))

(defmethod add-collection ((db xdb) name &key load-from-file)
  (let ((collection (or (gethash name (collections db))
                        (setf (gethash name (collections db))
                              (make-new-collection name db)))))
    (when load-from-file
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

(defgeneric load-db (xdb)
  (:documentation "Loads all the collections in a location."))

(defmethod load-db ((db xdb))
  (let ((unique-collections (make-hash-table :test 'equal)))
    (dolist (path (directory (format nil "~A/*.*" (location db))))
      (setf (gethash (pathname-name path) unique-collections) (pathname-name path)))
    (maphash  #'(lambda (key value)
                  (declare (ignore key))
               (add-collection db value :load-from-file t))
             unique-collections)))



;;Add method for validation when updating a collection.
