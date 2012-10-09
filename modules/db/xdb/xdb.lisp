(in-package #:ems)

(defparameter *dbs-ems* (make-instance 'dbs :base-path "~/xdb2/"))

(defun system-db ()
  (get-db *dbs-ems* (list "ems" "system")))


(add-db *dbs-ems* '("ems" "system") :load-from-file-p nil)

(xdb2::enable-sequences (system-db))

(defun next-sequence (name)
  (xdb2::next-sequence (system-db) name))


(defparameter *database-ems* nil
  "Special holding the current database. Most functions and macros
operating on a database assume this contains a connected database.")


(defmacro with-connection (spec &body body)
  "Locally establish a database connection, and bind *database* to it."
  `(let ((*database-ems* (apply #'connect ,spec)))
    (unwind-protect (progn ,@body)
      (disconnect *database-ems*))))


