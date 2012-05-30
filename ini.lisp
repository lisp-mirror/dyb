(in-package #:ems)

(setf hunchentoot:*catch-errors-p* nil)
(setf cl-who:*prologue* "<!doctype html>")

(start (make-instance 'easy-acceptor :port 8000))

(ensure-directories-exist "~/Development/ems/data/")

(defparameter *twitter-auth* nil)