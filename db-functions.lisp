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
              collection
              function)
    sum))
