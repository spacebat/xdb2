(in-package #:xdb)

(defun open-stream (path)
  (ensure-directories-exist (make-pathname :directory (pathname-directory path)))
  (open path
              :direction :output
              :if-does-not-exist :create
              #+ccl :sharing #+ccl nil
              :if-exists :append))

(defun close-stream (stream)
  (if stream
      (close stream ;:abort t
             )))

