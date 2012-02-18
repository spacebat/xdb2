;;Data aware collection

(defclass document ()
  ((collection :initarg :collection
               :accessor collection)
   (data :initarg :data
         :initform (make-hash-table :test 'equal)
         :accessor data)
   (key :initarg :key
        :accessor key)
   (type :initarg :type
         :initform nil
         :accessor doc-type))
  (:metaclass storable-class))