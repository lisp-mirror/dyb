(in-package #:dyb)

(defparameter *dbs* (make-instance 'dbs :base-path "~/xdb2/"))

(defun system-db ()
  (get-db *dbs* (list "dyb" "system")))

(add-db *dbs* '("dyb" "system"))

(xdb2::enable-sequences (system-db))

(defun next-sequence (name)
  (xdb2::next-sequence (system-db) name))
