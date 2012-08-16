(in-package :ems)

(defparameter *dbs* (make-instance 'dbs :base-path "~/Development/ems/data/"))

(add-db *dbs* '("ems" "system"))

(defun system-db* ()
  (get-db *dbs* (list "ems" "system")))

(defun users ()
  (get-collection (system-db*)
                   "users"))
(defun services ()
  (get-collection (system-db*)
                   "services"))