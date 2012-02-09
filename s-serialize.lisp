;;;; This code was mostly stripped from cl-prevalence and contorted to 
;;;; my own special miss uses here.
;;;;
;;;; cl-prevalence is Copyright (C) 2003, 2004 Sven Van Caekenberghe, Beta Nine BVBA.
;;;; cl-prevalence is goverened by the terms of the Lisp Lesser General Public License (LLGPL).
;;;; For more about the LLGPL go to http://opensource.franz.com/preamble.html.
;;;; 
;;;; (NOTE to self :: So what else is needed to comply to LLGPL?)
;;;;

(defpackage :s-serialize
  (:use :cl)
  (:export
   #:serialize-sexp
   #:deserialize-sexp
  
  )
  (:documentation "s-expression based serialization for Common Lisp and CLOS."))

(in-package :s-serialize)


(defgeneric serialize-sexp-internal (object stream)
  (:documentation "Write a serialized version of object to stream using s-expressions"))

(defconstant +cl-package+ (find-package :cl))

(defconstant +keyword-package+ (find-package :keyword))


(defun print-string-xml (string stream &key (start 0) end)
  "Write the characters of string to stream using basic XML conventions"
  (loop for offset upfrom start below (or end (length string))
        for char = (char string offset)
	do (case char
	     (#\& (write-string "&amp;" stream))
	     (#\< (write-string "&lt;" stream))
	     (#\> (write-string "&gt;" stream))
	     (#\" (write-string "&quot;" stream))
             ((#\newline #\return #\tab) (write-char char stream))
	     (t (if (and (<= 32 (char-code char))
			 (<= (char-code char) 126))
		    (write-char char stream)
		  (progn
		    (write-string "&#x" stream)
		    (write (char-code char) :stream stream :base 16)
		    (write-char #\; stream)))))))

(defun print-symbol (symbol stream)
  (let ((package (symbol-package symbol))
	(name (prin1-to-string symbol)))
    (cond ((eq package +cl-package+) (write-string "CL:" stream))
	  ((eq package +keyword-package+) (write-char #\: stream))
	  (package (print-string-xml (package-name package) stream)
                   (write-string "::" stream))
          (t (write-string "#:" stream)))
    (if (char= (char name (1- (length name))) #\|)
        (write-string name stream :start (position #\| name))
      (write-string name stream :start (1+ (or (position #\: name :from-end t) -1))))))


(defgeneric serializable-slots (object))

(defmethod serializable-slots ((object structure-object))
  #+openmcl
  (let* ((sd (gethash (class-name (class-of object)) ccl::%defstructs%))
	 (slots (if sd (ccl::sd-slots sd))))
    (mapcar #'car (if (symbolp (caar slots)) slots (cdr slots))))
  #+cmu
  (mapcar #'pcl:slot-definition-name (pcl:class-slots (class-of object)))
  #+sbcl
  (mapcar #'sb-pcl:slot-definition-name (sb-pcl:class-slots (class-of object)))
  #+lispworks
  (structure:structure-class-slot-names (class-of object))
  #+allegro
  (mapcar #'mop:slot-definition-name (mop:class-slots (class-of object)))
  #+sbcl
  (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots (class-of object)))
  #+clisp
  (mapcar #'clos:slot-definition-name (ext:structure-slots (type-of object)))
  #-(or openmcl cmu lispworks allegro sbcl clisp)
  (error "not yet implemented"))

(defmethod serializable-slots ((object standard-object))
  #+openmcl
  (mapcar #'ccl:slot-definition-name
	  (#-openmcl-native-threads ccl:class-instance-slots
	   #+openmcl-native-threads ccl:class-slots
	   (class-of object)))
  #+cmu
  (mapcar #'pcl:slot-definition-name (pcl:class-slots (class-of object)))
  #+sbcl
  (mapcar #'sb-pcl:slot-definition-name (sb-pcl:class-slots (class-of object)))
  #+lispworks
  (mapcar #'hcl:slot-definition-name (hcl:class-slots (class-of object)))
  #+allegro
  (mapcar #'mop:slot-definition-name (mop:class-slots (class-of object)))
  #+sbcl
  (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots (class-of object)))
  #+clisp
  (mapcar #'clos:slot-definition-name (clos:class-slots (class-of object)))
  #-(or openmcl cmu lispworks allegro sbcl clisp)
  (error "not yet implemented"))

(defun get-serializable-slots (object)
  (serializable-slots object))

(defun sequence-type-and-length (sequence)
  (if (listp sequence)
      (handler-case
          (let ((length (list-length sequence)))
            (if length
                (values :proper-list length)
                (values :circular-list nil)))
        (type-error ()
          (values :dotted-list nil)))
      (values :proper-sequence (length sequence))))

;;;; SERIALIZATION

;;; basic serializers
(defmethod serialize-sexp-internal ((object null) stream)
  (write-string "NIL" stream))

(defmethod serialize-sexp-internal ((object number) stream )
  (prin1 object stream))

(defmethod serialize-sexp-internal ((object (eql 't)) stream)
  (write-string "T" stream))

(defmethod serialize-sexp-internal ((object string) stream)
  (declare (ignore))
  (prin1 object stream))

(defmethod serialize-sexp-internal ((object character) stream)
  (declare (ignore))
  (prin1 object stream))

(defmethod serialize-sexp-internal ((object symbol) stream)
  (declare (ignore))
  (print-symbol object stream))

;;; generic sequences
(defmethod serialize-sexp-internal ((object sequence) stream)
  (flet ((proper-sequence (length)
           (let ((id ;(set-id object)
                  ))
             (write-string "(:SEQUENCE " stream)
            ; (prin1 id stream)
             (write-string " :CLASS " stream)
             (print-symbol (etypecase object (list 'list) (vector 'vector)) stream)
             (write-string " :SIZE " stream)
             (prin1 length stream)
             (unless (zerop length)
               (write-string " :ELEMENTS (" stream)
               (map nil
                    #'(lambda (element) 
                        (write-string " " stream)
                        (serialize-sexp-internal element stream))
                    object))
             (write-string " ) )" stream)))
         (improper-list ()           
           (let ((id ;(set-id object)
                  ))
             (write-string "(:CONS " stream)
             ;(prin1 id stream)
             (write-char #\Space stream)        
             (serialize-sexp-internal (car object) stream)
             (write-char #\Space stream)                
             (serialize-sexp-internal (cdr object) stream)
             (write-string " ) " stream))))
    (multiple-value-bind (seq-type length) (sequence-type-and-length object)
            (ecase seq-type
              ((:proper-sequence :proper-list) (proper-sequence length))
              ((:dotted-list :circular-list) (improper-list))))))

;;; hash tables
(defmethod serialize-sexp-internal ((object hash-table) stream)
  (let ((count (hash-table-count object)))
  ;        (setf id (set-id object))
          (write-string "(:HASH-TABLE " stream)
         ; (prin1 id stream)
          (write-string " :TEST " stream)
          (print-symbol (hash-table-test object) stream)
          (write-string " :SIZE " stream)
          (prin1 (hash-table-size object) stream)
          (write-string " :REHASH-SIZE " stream)
          (prin1 (hash-table-rehash-size object) stream)
          (write-string " :REHASH-THRESHOLD " stream)
          (prin1 (hash-table-rehash-threshold object) stream)
          (unless (zerop count)
            (write-string " :ENTRIES (" stream)
            (maphash #'(lambda (key value)
                         (write-string " (" stream)
                         (serialize-sexp-internal key stream)
                         (write-string " . " stream)
                         (serialize-sexp-internal value stream)
                         (princ ")" stream))
                     object)
            (write-string " )" stream))
          (write-string " )" stream)))

;;; structures
(defmethod serialize-sexp-internal ((object structure-object) stream)
  (let ((serializable-slots (get-serializable-slots object)))
;	(setf id (set-id object))
	(write-string "(:STRUCT " stream)
	;(prin1 id stream)
	(write-string " :CLASS " stream)
	(print-symbol (class-name (class-of object)) stream)
        (when serializable-slots
          (write-string " :SLOTS (" stream)
          (mapc #'(lambda (slot)
                    (write-string " (" stream)
                    (print-symbol slot stream)
                    (write-string " . " stream)
                    (serialize-sexp-internal (slot-value object slot) stream)
                    (write-string ")" stream))
                serializable-slots))
	(write-string " ) )" stream)))

;;; objects
(defmethod serialize-sexp-internal ((object standard-object) stream)

  (let ((serializable-slots (get-serializable-slots object)))
;	(setf id (set-id object))
	(write-string "(:OBJECT " stream)
;	(prin1 id stream)
	(write-string " :CLASS " stream)
	(print-symbol (class-name (class-of object)) stream)
        (when serializable-slots
          (princ " :SLOTS (" stream)
          (loop :for slot :in serializable-slots
                :do (when (slot-boundp object slot)
                      (write-string " (" stream)
                      (print-symbol slot stream)
                      (write-string " . " stream)
                      (serialize-sexp-internal (slot-value object slot) stream)
                      (write-string ")" stream))))
	(write-string " ) )" stream)))


;;;; DESERIALIZATION

(defun deserialize-sexp-internal (sexp ;deserialized-objects
                                  )
  (if (atom sexp) 
      sexp
      (ecase (first sexp)
        (:sequence (destructuring-bind (;id
                                        &key class size elements
                                        ) (rest sexp)
                     (let ((sequence (make-sequence class size)))
                       ;(setf (gethash id deserialized-objects) sequence)
                       (map-into sequence 
                                 #'(lambda (x) (deserialize-sexp-internal x ;deserialized-objects
                                                                          )) 
                                 elements))))
        (:hash-table (destructuring-bind (;id 
                                          &key test size rehash-size rehash-threshold entries
                                          ) (rest sexp)
                       (let ((hash-table (make-hash-table :size size 
                                                          :test test 
                                                          :rehash-size rehash-size 
                                                          :rehash-threshold rehash-threshold)))
                        ; (setf (gethash id deserialized-objects) hash-table)
                         (dolist (entry entries)
                           (setf (gethash (deserialize-sexp-internal (first entry); deserialized-objects
                                                                     ) 
                                          hash-table)
                                 (deserialize-sexp-internal (rest entry) ;deserialized-objects
                                                            )))
                         hash-table)))
        (:object (destructuring-bind (;id 
                                      &key class slots) (rest sexp)
                   
                   (let ((object (make-instance class)))
                    ; (setf (gethash id deserialized-objects) object)
                     (dolist (slot slots)
                       (when (slot-exists-p object (first slot))
                         (setf (slot-value object (first slot)) 
                               (deserialize-sexp-internal (rest slot) ;deserialized-objects
                                                          ))))
                     
                     object)))
        (:struct (destructuring-bind (;id 
                                      &key class slots
                                      ) (rest sexp)
                   (let ((object (funcall (intern (concatenate 'string "MAKE-" (symbol-name class)) 
                                                  (symbol-package class)))))
                     ;(setf (gethash id deserialized-objects) object)
                     (dolist (slot slots)
                       (when (slot-exists-p object (first slot))
                         (setf (slot-value object (first slot)) 
                               (deserialize-sexp-internal (rest slot) ;deserialized-objects
                                                          ))))
                     object)))
        (:cons (destructuring-bind (;id 
                                    cons-car cons-cdr) (rest sexp)
                 (let ((conspair (cons nil nil)))
                   ;(setf (gethash id deserialized-objects)  conspair)                   
                   (rplaca conspair (deserialize-sexp-internal cons-car ;deserialized-objects
                                                               ))
                   (rplacd conspair (deserialize-sexp-internal cons-cdr ;deserialized-objects
                                                               )))))
        ;(:ref (gethash (rest sexp) deserialized-objects ))
)))



(defun serialize-sexp (object stream)
  "Write a serialized version of object to stream using s-expressions."
  (serialize-sexp-internal object stream)
  )

(defun deserialize-sexp (stream)
  "Read and return an s-expression serialized version of a lisp object from stream."
  (let ((sexp (read stream nil :eof)))
    (when (eq sexp :eof) 
      nil)
    (unless (eq sexp :eof)  
      (deserialize-sexp-internal sexp ;*known-objects*
                                 ))))

