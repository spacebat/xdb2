;;;; xdb.asd

(asdf:defsystem #:xdb
  :serial t
  :components ((:file "s-serialize")
               (:file "package")
	       (:file "db-io")
               (:file "xdb")
               (:file "db-functions")))

