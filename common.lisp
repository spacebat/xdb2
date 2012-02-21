(in-package #:xdb2)

(defun slot-val (instance slot-name)
  (if (and instance
           (slot-boundp instance slot-name))
      (slot-value instance slot-name)))

(defgeneric get-val (object element &key data-type)
  (:documentation "Returns the value in a object based on the supplied element name and possible type hints."))

(defmethod get-val (object element &key data-type)
  (when object
    (typecase (or data-type object)
      (hash-table
       (gethash element object))
      (standard-object
       (slot-val object element))
      (t
       (if data-type
           (cond 
             ((equal 'alist data-type)
              (assoc element object))
             ((equal 'plist data-type)
              (get object element))
             (t
              (error "Does no handle this type of object")))
           (if (listp object)
               (assoc element object)
               (error "Does no handle this type of object")))))))

(defmethod (setf get-val) (new-value object element &key data-type)
  (typecase (or data-type object)
    (hash-table (setf (gethash element object) new-value))
    (standard-object (setf (slot-value object element) new-value))
    (t
     (if data-type
         (cond ((equal 'alist data-type)
                (assoc element object))
               ((equal 'plist data-type)
                (get object element )))
         (if (listp object)
             (assoc element object))))))

(defun copy-array (array)
  (let ((new-array
          (make-array (array-dimensions array)
                      :element-type (array-element-type array)
                      :adjustable (adjustable-array-p array)
                      :fill-pointer (and (array-has-fill-pointer-p array)
                                         (fill-pointer array)))))
    (loop for i below (array-total-size array)
          do (setf (row-major-aref new-array i)
                   (row-major-aref array i)))
    new-array))