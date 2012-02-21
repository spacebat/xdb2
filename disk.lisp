;;; -*- Mode: Lisp -*-

(in-package #:xdb2)

(defclass collection ()
  ((name :initarg :name
         :accessor name)
   (path :initarg :path
         :accessor path)
   (docs :initarg :docs
         :accessor docs)
   (packages :initform (make-s-packages)
             :accessor packages)
   (classes :initform (make-class-cache)
            :accessor classes)
   (last-id :initform 0
            :accessor last-id)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *codes*
    #(ascii-string
      identifiable
      cons
      string
      null
      storable-class
      standard-object
      fixnum
      bignum
      ratio
      double-float
      single-float
      complex
      symbol
      intern-package-and-symbol
      intern-symbol
      character
      simple-vector
      array
      hash-table
      pathname)))

(defvar *statistics* ())
(defun collect-stats (code)
  (let* ((type (aref *codes* code))
         (cons (assoc type *statistics*)))
    (if cons
        (incf (cdr cons))
        (push (cons type 1) *statistics*))
    type))

(defvar *collection* nil)

(defvar *classes*)
(defvar *packages*)
(declaim (vector *classes* *packages*))

(defvar *indexes*)
(declaim (hash-table *indexes*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun type-code (type)
    (position type *codes*)))

(defparameter *readers* (make-array (length *codes*)))
(declaim (type (simple-array function (*)) *readers*))

(defmacro defreader (type (stream) &body body)
  (let ((name (intern (format nil "~a-~a" type '#:reader))))
    `(progn
       (defun ,name (,stream)
         ,@body)
       (setf (aref *readers* ,(type-code type))
             #',name))))

(declaim (inline call-reader))
(defun call-reader (code stream)
  ;; (collect-stats code)
  (funcall (aref *readers* code) stream))

;;;

(defconstant +sequence-length+ 2)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +fixnum-length+ 4))
(defconstant +char-length+ 2)
(defconstant +id-length+ 4)
(defconstant +class-id-length+ 2)
(defconstant +hash-table-length+ 3)

(defconstant +unbound-slot+ 254)
(defconstant +end+ 255)

(defconstant +ascii-char-limit+ (code-char 128))

(deftype ascii-string ()
  '(or
    #+sb-unicode simple-base-string ; on #-sb-unicode the limit is 255
    (satisfies ascii-string-p)))

(defun ascii-string-p (string)
  (declare (simple-string string))
  (loop for char across string
        always (char< char +ascii-char-limit+)))

(deftype storage-fixnum ()
  `(signed-byte ,(* +fixnum-length+ 8)))

(defun make-class-cache ()
  (make-array 10 :adjustable t :fill-pointer 0))

(defmacro with-collection (collection &body body)
  (let ((collection-sym (gensym)))
    `(let* ((,collection-sym ,collection)
            (*collection* ,collection-sym)
            (*packages* (packages ,collection-sym))
            (*classes* (classes ,collection-sym)))
       ,@body)))

;;;

(defun slot-effective-definition (class slot-name)
  (find slot-name (class-slots class) :key #'slot-definition-name))

(defgeneric write-object (object stream))

(defun dump-data (stream)
  (map-docs
   nil
   (lambda (document)
     (write-standard-object document stream))
   *collection*))

(declaim (inline read-next-object))
(defun read-next-object (stream)
  (call-reader (read-n-bytes 1 stream) stream))

;;; NIL

(defmethod write-object ((object null) stream)
  (write-n-bytes #.(type-code 'null) 1 stream))

(defreader null (stream)
  (declare (ignore stream))
  nil)

;;; Symbol

(defun make-s-packages ()
  (make-array 10 :adjustable t :fill-pointer 0))

(defun make-s-package (package)
  (let ((symbols (make-array 100 :adjustable t :fill-pointer 0)))
    (values (vector-push-extend (cons package symbols) *packages*)
            symbols
            t)))

(defun find-s-package (package)
  (loop for i below (length *packages*)
        for (stored-package . symbols) = (aref *packages* i)
        when (eq package stored-package)
        return (values i symbols)
        finally (return (make-s-package package))))

(defun s-intern (symbol)
  (multiple-value-bind (package-id symbols new-package)
      (find-s-package (symbol-package symbol))
    (let* ((existing (and (not new-package)
                          (position symbol symbols)))
           (symbol-id (or existing
                          (vector-push-extend symbol symbols))))
      (values package-id symbol-id new-package (not existing)))))

(defun s-intern-existing (symbol symbols)
  (vector-push-extend symbol symbols))

(defmethod write-object ((symbol symbol) stream)
  (multiple-value-bind (package-id symbol-id
                        new-package new-symbol) (s-intern symbol)
    (cond ((and new-package new-symbol)
           (write-n-bytes #.(type-code 'intern-package-and-symbol) 1 stream)
           (write-object (package-name (symbol-package symbol)) stream)
           (write-object (symbol-name symbol) stream))
          (new-symbol
           (write-n-bytes #.(type-code 'intern-symbol) 1 stream)
           (write-n-bytes package-id +sequence-length+ stream)
           (write-object (symbol-name symbol) stream))
          (t
           (write-n-bytes #.(type-code 'symbol) 1 stream)
           (write-n-bytes package-id +sequence-length+ stream)
           (write-n-bytes symbol-id +sequence-length+ stream)))))

(defreader symbol (stream)
  (let* ((package-id (read-n-bytes +sequence-length+ stream))
         (symbol-id (read-n-bytes +sequence-length+ stream))
         (package (or (aref *packages* package-id)
                      (error "Package with id ~a not found" package-id)))
         (symbol (aref (cdr package) symbol-id)))
    (or symbol
        (error "Symbol with id ~a in package ~a not found"
               symbol-id (car package)))))

(defreader intern-package-and-symbol (stream)
  (let* ((package-name (read-next-object stream))
         (symbol-name (read-next-object stream))
         (package (or (find-package package-name)
                      (error "Package ~a not found" package-name)))
         (symbol (intern symbol-name package))
         (s-package (nth-value 1 (make-s-package package))))
    (s-intern-existing symbol s-package)
    symbol))

(defreader intern-symbol (stream)
  (let* ((package-id (read-n-bytes +sequence-length+ stream))
         (symbol-name (read-next-object stream))
         (package (or (aref *packages* package-id)
                      (error "Package with id ~a for symbol ~a not found"
                             package-id symbol-name)))
         (symbol (intern symbol-name (car package))))
    (s-intern-existing symbol (cdr package))
    symbol))

;;; Integer

(declaim (inline sign))
(defun sign (n)
  (if (minusp n)
      1
      0))

(defun write-fixnum (n stream)
  (declare (storage-fixnum n))
  (write-n-bytes #.(type-code 'fixnum) 1 stream)
  (write-n-signed-bytes n +fixnum-length+ stream))

(defun write-bignum (n stream)
  (declare ((and integer (not storage-fixnum)) n))
  (write-n-bytes #.(type-code 'bignum) 1 stream)
  (write-n-bytes (sign n) 1 stream)
  (let* ((fixnum-bits (* +fixnum-length+ 8))
         (n (abs n))
         (size (ceiling (integer-length n) fixnum-bits)))
    (write-n-bytes size 1 stream)
    (loop for position by fixnum-bits below (* size fixnum-bits)
          do
          (write-n-bytes (ldb (byte fixnum-bits position) n)
                         +fixnum-length+ stream))))

(defmethod write-object ((object integer) stream)
  (typecase object
    (storage-fixnum
     (write-fixnum object stream))
    (t (write-bignum object stream))))

(declaim (inline read-sign))
(defun read-sign (stream)
  (if (plusp (read-n-bytes 1 stream))
      -1
      1))

(defreader bignum (stream)
  (let ((fixnum-bits (* +fixnum-length+ 8))
        (sign (read-sign stream))
        (size (read-n-bytes 1 stream))
        (integer 0))
    (loop for position by fixnum-bits below (* size fixnum-bits)
          do
          (setf (ldb (byte fixnum-bits position) integer)
                (read-n-bytes +fixnum-length+ stream)))
    (* sign integer)))

(defreader fixnum (stream)
  (read-n-signed-bytes +fixnum-length+ stream))

;;; Ratio

(defmethod write-object ((object ratio) stream)
  (write-n-bytes #.(type-code 'ratio) 1 stream)
  (write-object (numerator object) stream)
  (write-object (denominator object) stream))

(defreader ratio (stream)
  (/ (read-next-object stream)
     (read-next-object stream)))

;;; Float

(defun write-8-bytes (n stream)
  (write-n-bytes (ldb (byte 32 0) n) 4 stream)
  (write-n-bytes (ldb (byte 64 32) n) 4 stream))

(defun read-8-bytes (stream)
  (logior (read-n-bytes 4 stream)
          (ash (read-n-bytes 4 stream) 32)))

(defmethod write-object ((float float) stream)
  (etypecase float
    (single-float
     (write-n-bytes #.(type-code 'single-float) 1 stream)
     (write-n-bytes (ieee-floats:encode-float32 float) 4 stream))
    (double-float
     (write-n-bytes #.(type-code 'double-float) 1 stream)
     (write-8-bytes (ieee-floats:encode-float64 float) stream))))

(defreader single-float (stream)
  (ieee-floats:decode-float32 (read-n-bytes 4 stream)))

(defreader double-float (stream)
  (ieee-floats:decode-float64 (read-8-bytes stream)))

;;; Complex

(defmethod write-object ((complex complex) stream)
  (write-n-bytes #.(type-code 'complex) 1 stream)
  (write-object (realpart complex) stream)
  (write-object (imagpart complex) stream))

(defreader complex (stream)
  (complex (read-next-object stream)
           (read-next-object stream)))

;;; Characters

(defmethod write-object ((character character) stream)
  (write-n-bytes #.(type-code 'character) 1 stream)
  (write-n-bytes (char-code character) +char-length+ stream))

(defreader character (stream)
  (code-char (read-n-bytes +char-length+ stream)))

;;; Strings

(defun write-ascii-string (string stream)
  (declare (simple-string string))
  (loop for char across string
        do (write-n-bytes (char-code char) 1 stream)))

(defun write-multibyte-string (string stream)
  (declare (simple-string string))
  (loop for char across string
        do (write-n-bytes (char-code char) +char-length+ stream)))

(defmethod write-object ((string string) stream)
  (etypecase string
    ((not simple-string)
     (call-next-method))
    #+sb-unicode
    (simple-base-string
     (write-n-bytes #.(type-code 'ascii-string) 1 stream)
     (write-n-bytes (length string) +sequence-length+ stream)
     (write-ascii-string string stream))
    (ascii-string
     (write-n-bytes #.(type-code 'ascii-string) 1 stream)
     (write-n-bytes (length string) +sequence-length+ stream)
     (write-ascii-string string stream))
    (string
     (write-n-bytes #.(type-code 'string) 1 stream)
     (write-n-bytes (length string) +sequence-length+ stream)
     (write-multibyte-string string stream))))

(declaim (inline read-ascii-string))
(defun read-ascii-string (length stream)
  (let ((string (make-string length :element-type 'base-char)))
    ;#-sbcl
    (loop for i below length
          do (setf (schar string i)
                   (code-char (read-n-bytes 1 stream))))
    #+(and nil sbcl (or x86 x86-64))
    (read-ascii-string-optimized length string stream)
    string))

(defreader ascii-string (stream)
  (read-ascii-string (read-n-bytes +sequence-length+ stream) stream))

(defreader string (stream)
  (let* ((length (read-n-bytes +sequence-length+ stream))
         (string (make-string length :element-type 'character)))
    (loop for i below length
          do (setf (schar string i)
                   (code-char (read-n-bytes +char-length+ stream))))
    string))

;;; Pathname

(defmethod write-object ((pathname pathname) stream)
  (write-n-bytes #.(type-code 'pathname) 1 stream)
  (write-object (pathname-name pathname) stream)
  (write-object (pathname-directory pathname) stream)
  (write-object (pathname-device pathname) stream)
  (write-object (pathname-type pathname) stream)
  (write-object (pathname-version pathname) stream))

(defreader pathname (stream)
  (make-pathname
   :name (read-next-object stream)
   :directory (read-next-object stream)
   :device (read-next-object stream)
   :type (read-next-object stream)
   :version (read-next-object stream)))

;;; Cons

(defmethod write-object ((list cons) stream)
  (cond ((alexandria:circular-list-p list)
         (error "Can't store circular lists"))
        (t
         (write-n-bytes #.(type-code 'cons) 1 stream)
         (loop for cdr = list then (cdr cdr)
               do
               (cond ((consp cdr)
                      (write-object (car cdr) stream))
                     (t
                      (write-n-bytes +end+ 1 stream)
                      (write-object cdr stream)
                      (return)))))))

(defreader cons (stream)
  (let ((first-cons (list (read-next-object stream))))
    (loop for previous-cons = first-cons then new-cons
          for car = (let ((id (read-n-bytes 1 stream)))
                      (cond ((eq id +end+)
                             (setf (cdr previous-cons) (read-next-object stream))
                             (return))
                            ((call-reader id stream))))
          for new-cons = (list car)
          do (setf (cdr previous-cons) new-cons))
    first-cons))

;;; Simple-vector

(defmethod write-object ((vector vector) stream)
  (typecase vector
    (simple-vector
     (write-simple-vector vector stream))
    (t
     (call-next-method))))

(defun write-simple-vector (vector stream)
  (declare (simple-vector vector))
  (write-n-bytes #.(type-code 'simple-vector) 1 stream)
  (write-n-bytes (length vector) +sequence-length+ stream)
  (loop for elt across vector
        do (write-object elt stream)))

(defreader simple-vector (stream)
  (let ((vector (make-array (read-n-bytes +sequence-length+ stream))))
    (loop for i below (length vector)
          do (setf (svref vector i) (read-next-object stream)))
    vector))

;;; Array

(defun boolify (x)
  (if x
      1
      0))

(defmethod write-object ((array array) stream)
  (write-n-bytes #.(type-code 'array) 1 stream)
  (write-object (array-dimensions array) stream)
  (cond ((array-has-fill-pointer-p array)
         (write-n-bytes 1 1 stream)
         (write-n-bytes (fill-pointer array) +sequence-length+ stream))
        (t
         (write-n-bytes 0 2 stream)))
  (write-object (array-element-type array) stream)
  (write-n-bytes (boolify (adjustable-array-p array)) 1 stream)
  (loop for i below (array-total-size array)
        do (write-object (row-major-aref array i) stream)))

(defun read-array-fill-pointer (stream)
  (if (plusp (read-n-bytes 1 stream))
      (read-n-bytes +sequence-length+ stream)
      (not (read-n-bytes 1 stream))))

(defreader array (stream)
  (let ((array (make-array (read-next-object stream)
                           :fill-pointer (read-array-fill-pointer stream)
                           :element-type (read-next-object stream)
                           :adjustable (plusp (read-n-bytes 1 stream)))))
    (loop for i below (array-total-size array)
          do (setf (row-major-aref array i) (read-next-object stream)))
    array))

;;; Hash-table

(defvar *hash-table-tests* #(eql equal equalp eq))
(declaim (simple-vector *hash-table-tests*))

(defun check-hash-table-test (hash-table)
  (let* ((test (hash-table-test hash-table))
         (test-id (position test *hash-table-tests*)))
   (unless test-id
     (error "Only standard hashtable tests are supported, ~a has ~a"
            hash-table test))
    test-id))

(defmethod write-object ((hash-table hash-table) stream)
  (write-n-bytes #.(type-code 'hash-table) 1 stream)
  (write-n-bytes (check-hash-table-test hash-table) 1 stream)
  (write-n-bytes (hash-table-size hash-table) +hash-table-length+ stream)
  (loop for key being the hash-keys of hash-table
        using (hash-value value)
        do
        (write-object key stream)
        (write-object value stream))
  (write-n-bytes +end+ 1 stream))

(defreader hash-table (stream)
  (let* ((test (svref *hash-table-tests* (read-n-bytes 1 stream)))
         (size (read-n-bytes +hash-table-length+ stream))
         (table (make-hash-table :test test :size size)))
    (loop for id = (read-n-bytes 1 stream)
          until (eq id +end+)
          do (setf (gethash (call-reader id stream) table)
                   (read-next-object stream)))
    table))

;;; storable-class

(defun cache-class (class id)
  (when (< (length *classes*) id)
    (adjust-array *classes* (1+ id)))
  (when (> (1+ id) (fill-pointer *classes*))
    (setf (fill-pointer *classes*) (1+ id)))
  (setf (aref *classes* id) class))

(defmethod write-object ((class storable-class) stream)
  (write-n-bytes #.(type-code 'storable-class) 1 stream)
  (write-object (class-name class) stream)
  (write-n-bytes (class-id class) +class-id-length+ stream)
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (let ((slots (slots-to-store class)))
    (write-n-bytes (length slots) +sequence-length+ stream)
    (loop for slot across slots
          do (write-object (slot-definition-name slot)
                           stream))))

(defreader storable-class (stream)
  (let ((class (find-class (read-next-object stream))))
    (cache-class class
                 (read-n-bytes +class-id-length+ stream))
    (unless (class-finalized-p class)
      (finalize-inheritance class))
    (let* ((length (read-n-bytes +sequence-length+ stream))
           (vector (make-array length)))
      (loop for i below length
            for slot-d =
            (slot-effective-definition class (read-next-object stream))
            do (setf (aref vector i)
                     (cons (slot-definition-location slot-d)
                           (slot-definition-initform slot-d))))
      (setf (slot-locations-and-initforms-read class) vector))
    class))

;;; identifiable

(defmethod write-object ((object identifiable) stream)
  (let* ((class (class-of object))
         (class-id (get-class-id class stream)))
    (write-n-bytes #.(type-code 'identifiable) 1 stream)
    (write-n-bytes class-id +class-id-length+ stream)
    (write-n-bytes (id object) +id-length+ stream)))

(defun get-class (id)
  (aref *classes* id))

(declaim (inline get-instance))
(defun get-instance (class-id id)
  (or (gethash id *indexes*)
      (setf (gethash id *indexes*)
            (fast-allocate-instance (get-class class-id)))))

(defreader identifiable (stream)
  (get-instance (read-n-bytes +class-id-length+ stream)
                (read-n-bytes +id-length+ stream)))

;;; standard-object

(defun store-class (class stream)
  (let ((id (vector-push-extend class *classes*)))
    (setf (class-id class) id)
    (write-object class stream)
    id))

(defun get-class-id (class stream)
  (or (position class *classes* :test #'eq)
      (store-class class stream)))

;;; Can't use write-object method, because it would conflict with
;;; writing a pointer to a standard object
(defun write-standard-object (object stream)
  (let* ((class (class-of object))
         (slots (slot-locations-and-initforms class))
         (class-id (get-class-id class stream)))
    (declare (simple-vector slots))
    (write-n-bytes #.(type-code 'standard-object) 1 stream)
    (write-n-bytes class-id +class-id-length+ stream)
    (write-n-bytes (id object) +id-length+ stream)
    (loop for id below (length slots)
          for (location . initform) = (aref slots id)
          for value = (standard-instance-access object location)
          unless (eql value initform)
          do
          (write-n-bytes id 1 stream)
          (if (eq value 'sb-pcl::..slot-unbound..)
              (write-n-bytes +unbound-slot+ 1 stream)
              (write-object value stream)))
    (write-n-bytes +end+ 1 stream)))

(defreader standard-object (stream)
  (let* ((class-id (read-n-bytes +class-id-length+ stream))
         (id (read-n-bytes +id-length+ stream))
         (instance (get-instance class-id id))
         (class (class-of instance))
         (slots (slot-locations-and-initforms-read class)))
    (declare (simple-vector slots))
    (setf (id instance) id)
    (if (> id (last-id *collection*))
     (setf (last-id *collection*) id))
    (loop for slot-id = (read-n-bytes 1 stream)
          until (= slot-id +end+)
          do (setf (standard-instance-access instance
                                             (car (aref slots slot-id)))
                   (let ((code (read-n-bytes 1 stream)))
                     (if (= code +unbound-slot+)
                         'sb-pcl::..slot-unbound..
                         (call-reader code stream)))))
    instance))

;;;
#+sbcl (declaim (inline %fast-allocate-instance))

#+sbcl
(defun %fast-allocate-instance (wrapper initforms)
  (declare (simple-vector initforms))
  (let ((instance (sb-pcl::%make-standard-instance
                   (copy-seq initforms) (sb-pcl::get-instance-hash-code))))
    (setf (sb-pcl::std-instance-wrapper instance)
          wrapper)
    instance))

#+sbcl
(defun fast-allocate-instance (class)
  (declare (optimize speed))
  (let ((initforms (class-initforms class))
        (wrapper (sb-pcl::class-wrapper class)))
    (%fast-allocate-instance wrapper initforms)))

(defun read-file (function file)
  (with-io-file (stream file)
    (loop until (stream-end-of-file-p stream)
          do (let ((object (read-next-object stream)))
               (when (typep object 'identifiable)
                 (funcall function object))))))

(defun load-data (collection file function)
  (let ((*indexes* (make-hash-table :size 1000)))
    (with-collection collection
      (read-file function file))))

(defun save-data (collection &optional file)
  (with-collection collection
    (with-io-file (stream file
                   :direction :output)
      (dump-data stream))))

(defun save-doc (collection document &optional file)
  (with-collection collection
    (with-io-file (stream file
                   :direction :output
                   :append t)
      (write-standard-object document stream))))
