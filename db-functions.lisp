(in-package #:xdb2)

(defgeneric sum (collection &key function &allow-other-keys)
  (:documentation "Applies the function to all the docs in the collection and returns the sum of
the return values."))

(defmethod sum ((collection collection) &key function element)
  (let* ((sum 0)
         (function (or function
                       (lambda (doc)
                         (incf sum (get-val doc element))))))
    (map-docs nil
              function
              collection)
    sum))

(defgeneric max-val (collection &key function element))

(defmethod max-val ((collection collection) &key function element)
  (let* ((max 0)
         (function (or function
                       (lambda (doc)
                         (if (get-val doc element)
                             (if (> (get-val doc element) max)
                                 (setf max (get-val doc element))))))))
    (map-docs nil
              function
              collection)
    max))

