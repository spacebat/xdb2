;;;; xdb.asd

(asdf:defsystem #:xdb
  :serial t
  :depends-on (closer-mop ieee-floats)
  :components ((:file "package")
               #+(and sbcl (or x86 x86-64))
               (:file "io-sbcl")
               #-(and sbcl (or x86 x86-64))
               (:file "io-generic")
               (:file "mop")
               (:file "disk")
               (:file "xdb")
               (:file "db-functions")))
