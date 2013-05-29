(in-package #:dyb)

(defparameter *dbs* (make-instance 'dbs :base-path "~/xdb2/"))

(defun system-db ()
  (get-db *dbs* (list "dyb" "system")))

(add-db *dbs* '("dyb" "system"))


