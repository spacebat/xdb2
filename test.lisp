(in-package #:xdb)

;;;;Test

(defparameter *tree* nil)

(defun make-doc-test (type key data)
  (let ((doc-obj (make-instance 'document :key key :type type)))
    (dolist (pair data)
      (setf (gethash (first pair) (data doc-obj)) (second pair)))
    doc-obj))

(defun test-store-doc (collection times)
  (dotimes (i times)
    (store-doc collection
               (make-doc-test
                "Test Doc"
                i
                (list
                 (list "id" i)
                 (list "eid" i)
                 (list "aa" (format nil "~R" (random 51234)))
                 (list "bb" (format nil "~R" (random 1234)))
                 (list "cc" (format nil "~R" (random 1234)))
                 (list "dd" (format nil "~R" (random 1234)))
                 (list "ee" (format nil "~R" (random 1234)))
                 (list "ff" (format nil "~R" (random 1234)))
                 (list "gg" (format nil "~R" (random 1234)))
                 (list "hh" (format nil "~R" (random 1234))))))))



(defun test ()
  (let* ((db (make-instance 'xdb :location "/tmp/db-test/"))
         (col (add-collection db "test" :load-from-file-t nil)))
    (vector-push-extend (make-instance 'document2 :key "dd") (docs col))
    (time (test-store-doc col 10000))
    (time (snapshot db))
    ;; (time (sum col "eid"))
    ;; (time (find-doc col "eid" 50))
    ;; (time (sort-collection col))
    ))

#|

(defparameter db )
(defparameter col (add-collection db "test" :load-from-file-t nil))
(time (test-store-doc col 1000))
(time (sum col "eid"))
(time (find-doc col "eid" 50))
(time (sort-collection col))

|#








