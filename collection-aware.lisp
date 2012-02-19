(in-package #:xdb2)

;; This class is so we can create updateable collections accross 
(defclass collection-aware (collection)
  ())

(defmethod add-doc ((collection collection-aware) doc &key duplicate-doc-p-func)
  (when doc
    (setf (get-val doc 'collection) collection)

    (if duplicate-doc-p-func
        (let ((dup (find-duplicate-doc collection doc :function duplicate-doc-p-func)))
          (if (not dup)
              (vector-push-extend doc (docs collection))
              (setf dup doc) ;;doing this because ???
              ))
        (vector-push-extend doc (docs collection)))))


(defmethod store-doc ((collection collection-aware) doc
                      &key (duplicate-doc-p-func 'duplicate-doc-p))
  (when doc
    (setf (get-val doc 'collection) collection)
    (let ((dup (and duplicate-doc-p-func
                    (find-duplicate-doc collection doc
                                        :function duplicate-doc-p-func))))
      (if dup
          (setf dup doc)
          (vector-push-extend doc (docs collection)))
      (serialize-doc collection doc))))

(defgeneric store-document (doc
                      &key duplicate-doc-p-func))

(defmethod store-document ((doc document)
                      &key (duplicate-doc-p-func 'duplicate-doc-p))
  (when doc
    (store-doc (get-val doc 'collection) doc 
               :duplicate-doc-p-func duplicate-doc-p-func)))