;;;; package.lisp

(defpackage #:xdb2
  (:use :closer-common-lisp)
  (:export
   :xdb
   :collection
   :map-docs
   :duplicate-doc-p
   :find-duplicate-doc
   :store-doc
   :serialize-doc
   :serialize-docs
   :load-from-file
   :get-collection
   :add-collection
   :snapshot
   :load-db
   :get-docs
   :get-doc
   :get-val
   :set-val
   :sum
   :document
   :data
   :key
   :find-doc
   :find-docs
   :sort-collection
   :docs
   :*fsync-data*))
