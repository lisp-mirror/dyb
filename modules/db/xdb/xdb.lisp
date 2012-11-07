(in-package #:dyb)

(defparameter *dbs* (make-instance 'dbs :base-path "~/xdb2/"))

(defun system-db ()
  (get-db *dbs* (list "dyb" "system")))


(add-db *dbs* '("dyb" "system") :load-from-file-p nil)

(xdb2::enable-sequences (system-db))

(defun next-sequence (name)
  (xdb2::next-sequence (system-db) name))


(defparameter *database* nil
  "Special holding the current database. Most functions and macros
operating on a database assume this contains a connected database.")


(defmacro with-connection (spec &body body)
  "Locally establish a database connection, and bind *database* to it."
  `(let ((*database* (apply #'connect ,spec)))
    (unwind-protect (progn ,@body)
      (disconnect *database*))))


