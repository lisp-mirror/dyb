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
        
        (when body

          (if result-is-octets-p
              (setf result (json::decode-json-from-string (babel:octets-to-string body)))
              (setf result (json::decode-json-from-string body))) 

          (when (listp result)
            (if (or (assoc-path result error-path) 
                    (assoc-path result :error) 
                    (assoc-path result :errors))
                (let ((error-message (or (assoc-path result error-path)
                                         (assoc-path result :error :message)
                                         (if (listp (cdr (assoc-path result :errors)))
                                             (assoc-path (car (cdr (assoc-path result :errors))) :message)
                                             (assoc-path result :errors)))))
                
                  (setf message (if (listp error-message)
                                    (cdr error-message)
                                    error-message))))))
        ;(break "~A~%~A" message (assoc-path (car (cdr (assoc-path result :errors))) :message))
        (unless body
          
          (setf message "Endpoint returned no values."))))
    (values result message)))

(defun handle-endpoint-run-request (user request &key error-path result-is-octets-p)
  (let ((result)
        (message))
    (unless (get-val user 'last-access-token)
      (setf message "Missing access token"))

    (when (get-val user 'last-access-token)
      (multiple-value-bind (body status header uri stream must-close reason-phrase)
          (eval request)
        
        (when body
          (if result-is-octets-p
              (setf result (json::decode-json-from-string (babel:octets-to-string body)))
              (setf result (json::decode-json-from-string body))) 
          (if (or (assoc-path result error-path) 
                  (assoc-path result :error) 
                  (assoc-path result :errors))
              (setf message (cdr (or (assoc-path result error-path)
                                     (assoc-path result :error :message)
                                     (assoc-path result :errors :message))))))
        (unless body
          
          (unless (or (equal status 200) (equal status 201) )
            (setf message "Endpoint returned no values."))
          (when (or (equal status 200) (equal status 201) )
            (setf result reason-phrase)))))
    (values result message)))