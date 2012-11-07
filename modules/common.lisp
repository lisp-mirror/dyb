(in-package :dyb)


(defmacro with-html-to-string ((&key prologue (indent '*indent-code*)) &body body)
  `(with-html-output-to-string (*standard-output* nil :indent ,indent
                                                  :prologue ,prologue)
     ,@body))

(defmacro with-html (&body body)
  `(with-html-output (*standard-output* nil :indent *indent-code*)
     ,@body))

(defmacro with-html-string (&body body)
  `(with-html-output-to-string (*standard-output* nil :indent *indent-code*)
     ,@body))


(defun get-access-token (client identifier request-token)
  (let ((service (get-twitter-service client identifier)))
    (if service
        (cl-oauth::obtain-access-token  
         (get-access-token-endpoint service)
         request-token  
         :consumer-token (consumer-token service) ))))