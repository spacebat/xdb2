;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:xdb2)

(defvar *fsync-data* nil)

(defconstant +buffer-size+ 8192)

(deftype word () 'sb-vm:word)

(defstruct (input-stream
            (:predicate nil))
  (fd nil :type word)
  (left 0 :type word)
  (buffer-start (sb-sys:sap-int
                 (sb-alien::%make-alien (* sb-vm:n-byte-bits
                                           (+ +buffer-size+ 3))))
   :type word)
  (buffer-end 0 :type word)
  (buffer-position 0 :type word))

(defstruct (output-stream
            (:predicate nil))
  (fd nil :type word)
  (buffer-start (sb-sys:sap-int
                 (sb-alien::%make-alien (* sb-vm:n-byte-bits
                                           (+ +buffer-size+ 3))))
                :type word)
  (buffer-end 0 :type word)
  (buffer-position 0 :type word))

(defun open-file (file-stream
                  &key direction)
  (if (eql direction :output)
      (let ((output (make-output-stream
                     :fd (sb-sys:fd-stream-fd file-stream))))
        (setf (output-stream-buffer-position output)
              (output-stream-buffer-start output)
              (output-stream-buffer-end output)
              (+ (output-stream-buffer-start output)
                 +buffer-size+))
        output)
      (make-input-stream
       :fd (sb-sys:fd-stream-fd file-stream)
       :left (file-length file-stream))))

(defun close-input-stream (stream)
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "free"
                          (function (values) sb-alien:long))
   (input-stream-buffer-start stream)))

(defun close-output-stream (stream)
  (flush-buffer stream)
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "free"
                          (function (values) sb-alien:long))
   (output-stream-buffer-start stream)))

(declaim (inline stream-end-of-file-p))
(defun stream-end-of-file-p (stream)
  (and (>= (input-stream-buffer-position stream)
           (input-stream-buffer-end stream))
       (zerop (input-stream-left stream))))

(declaim (inline sap-ref-24))
(defun sap-ref-24 (sap offset)
  (declare (optimize speed (safety 0))
           (fixnum offset))
  (mask-field (byte 24 0) (sb-sys:sap-ref-32 sap offset)))

(declaim (inline n-sap-ref))
(defun n-sap-ref (n sap &optional (offset 0))
  (funcall (ecase n
             (1 #'sb-sys:sap-ref-8)
             (2 #'sb-sys:sap-ref-16)
             (3 #'sap-ref-24)
             (4 #'sb-sys:sap-ref-32))
           sap
           offset))

(declaim (inline unix-read))
(defun unix-read (fd buf len)
  (declare (optimize (sb-c::float-accuracy 0)
                     (space 0)))
  (declare (type sb-unix::unix-fd fd)
           (type word len))
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "read"
                          (function sb-alien:int
                                    sb-alien:int sb-alien:long sb-alien:int))
   fd buf len))

(declaim (inline unix-read))
(defun unix-write (fd buf len)
  (declare (optimize (sb-c::float-accuracy 0)
                     (space 0)))
  (declare (type sb-unix::unix-fd fd)
           (type word len))
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "write"
                          (function sb-alien:int
                                    sb-alien:int sb-alien:long sb-alien:int))
   fd buf len))

(defun fill-buffer (stream offset)
  (let ((length (unix-read (input-stream-fd stream)
                           (+ (input-stream-buffer-start stream) offset)
                           (- +buffer-size+ offset))))
    (setf (input-stream-buffer-end stream)
          (+ (input-stream-buffer-start stream) (+ length offset)))
    (decf (input-stream-left stream) length))
  t)

(defun refill-buffer (n stream)
  (declare (type word n)
           (input-stream stream))
  (let ((left-n-bytes (- (input-stream-buffer-end stream)
                         (input-stream-buffer-position stream))))
    (when (> (- n left-n-bytes)
             (input-stream-left stream))
      (error "End of file ~a" stream))
    (unless (zerop left-n-bytes)
      (setf (sb-sys:sap-ref-word (sb-sys:int-sap (input-stream-buffer-start stream)) 0)
            (n-sap-ref left-n-bytes (sb-sys:int-sap (input-stream-buffer-position stream)))))
    (fill-buffer stream left-n-bytes))
  (let ((start (input-stream-buffer-start stream)))
    (setf (input-stream-buffer-position stream)
          (+ start n)))
  t)

(declaim (inline advance-input-stream))
(defun advance-input-stream (n stream)
  (declare (optimize (space 0))
           (type word n)
           (type input-stream stream))
  (let* ((sap (input-stream-buffer-position stream))
         (new-sap (sb-ext:truly-the word (+ sap n))))
    (declare (word sap new-sap))
    (cond ((> new-sap (input-stream-buffer-end stream))
           (refill-buffer n stream)
           (sb-sys:int-sap (input-stream-buffer-start stream)))
          (t
           (setf (input-stream-buffer-position stream)
                 new-sap)
           (sb-sys:int-sap sap)))))

(declaim (inline read-n-bytes))
(defun read-n-bytes (n stream)
  (declare (optimize (space 0))
           (type word n))
  (n-sap-ref n (advance-input-stream n stream)))

(declaim (inline read-n-signed-bytes))
(defun read-n-signed-bytes (n stream)
  (declare (optimize speed)
           (sb-ext:muffle-conditions sb-ext:compiler-note)
           (type (integer 1 4) n))
  (funcall (ecase n
             (1 #'sb-sys:signed-sap-ref-8)
             (2 #'sb-sys:signed-sap-ref-16)
             ;; (3 )
             (4 #'sb-sys:signed-sap-ref-32))
           (advance-input-stream n stream)
           0))

(declaim (inline write-n-signed-bytes))
(defun write-n-signed-bytes (value n stream)
  (declare (optimize speed)
           (sb-ext:muffle-conditions sb-ext:compiler-note)
           (fixnum n))
  (ecase n
    (1 (setf (sb-sys:signed-sap-ref-8 (advance-output-stream n stream) 0)
             value))
    (2 (setf (sb-sys:signed-sap-ref-16 (advance-output-stream n stream) 0)
             value))
    ;; (3 )
    (4 (setf (sb-sys:signed-sap-ref-32 (advance-output-stream n stream) 0)
             value)))
  t)

(defun flush-buffer (stream)
  (unix-write (output-stream-fd stream)
              (output-stream-buffer-start stream)
              (- (output-stream-buffer-position stream)
                 (output-stream-buffer-start stream))))

(declaim (inline advance-output-stream))
(defun advance-output-stream (n stream)
  (declare (optimize (space 0) (safety 0))
           (type word n)
           (type output-stream stream)
           ((integer 1 4) n))
  (let* ((sap (output-stream-buffer-position stream))
         (new-sap (sb-ext:truly-the word (+ sap n))))
    (declare (word sap new-sap))
    (cond ((> new-sap (output-stream-buffer-end stream))
           (flush-buffer stream)
           (setf (output-stream-buffer-position stream)
                 (+ (output-stream-buffer-start stream)
                    n))
           (sb-sys:int-sap (output-stream-buffer-start stream)))
          (t
           (setf (output-stream-buffer-position stream)
                 new-sap)
           (sb-sys:int-sap sap)))))

(declaim (inline write-n-bytes))
(defun write-n-bytes (value n stream)
  (declare (optimize (space 0))
           (type word n))
  (setf (sb-sys:sap-ref-32
         (advance-output-stream n stream)
         0)
        value))
;;;

(declaim (inline copy-mem))
(defun copy-mem (from to length)
  (let ((words-end (- length (rem length sb-vm:n-word-bytes))))
    (loop for i by sb-vm:n-word-bytes below words-end
          do (setf (sb-sys:sap-ref-word to i)
                   (sb-sys:sap-ref-word from i)))
    (loop for i from words-end below length
          do (setf (sb-sys:sap-ref-8 to i)
                   (sb-sys:sap-ref-8 from i)))))

(declaim (inline read-ascii-string-optimized))
(defun read-ascii-string-optimized (length string stream)
  (declare (type fixnum length)
           (optimize speed))
  (sb-sys:with-pinned-objects (string)
    (let ((sap (advance-input-stream length stream))
          (string-sap (sb-sys:vector-sap string)))
      (copy-mem sap string-sap length)))
  string)

;;;

(defmacro with-io-file ((stream file
                         &key append (direction :input))
                        &body body)
  (let ((fd-stream (gensym)))
    `(with-open-file (,fd-stream ,file
                                 :element-type '(unsigned-byte 8)
                                 :direction ,direction
                                 ,@(and (eql direction :output)
                                        `(:if-exists ,(if append
                                                          :append
                                                          :supersede)))
                                 ,@(and append
                                        `(:if-does-not-exist :create)))
       (let ((,stream (open-file ,fd-stream :direction ,direction)))
         (unwind-protect
              (progn ,@body)
           ,@(ecase direction
               (:output
                `((close-output-stream ,stream)
                  (when *fsync-data*
                    (sb-posix:fdatasync
                     (sb-sys:fd-stream-fd ,fd-stream)))))
               (:input
                `((close-input-stream ,stream)))))))))
