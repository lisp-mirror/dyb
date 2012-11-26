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

(defun handle-endpoint (user request &key error-path result-is-octets-p)
  (let ((result)
        (message))
    (unless (get-val user 'last-access-token)
      (setf message "Missing access token"))

    (when (get-val user 'last-access-token)
      (multiple-value-bind (body)
          request
        (if result-is-octets-p
               (setf result (json::decode-json-from-string (babel:octets-to-string body)))
               (setf result (json::decode-json-from-string body))) 
        (if (or (assoc-path result error-path) 
                (assoc-path result :error) 
                (assoc-path result :errors))
            (setf message (cdr (or (assoc-path result error-path)
                                   (assoc-path result :error :message)
                                   (assoc-path result :errors :message)))))))
    (values result message)))