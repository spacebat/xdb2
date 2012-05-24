;;;; package.lisp

(defpackage #:xdb2
  (:use :closer-common-lisp)
  (:export
   :xdb
   :collection
   :collection-aware
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
   :max-val
   :document
   :doc-type
   :key
   :find-doc
   :find-docs
   :sort-collection
   :docs
   :*fsync-data*
   :storable-class
   :dbs
   :get-db
   :add-db
   :enable-sequences
   :next-sequence
   :sort-docs))
