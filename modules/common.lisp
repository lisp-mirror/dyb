(in-package :dyb)

(defun ensure-string-reply (reply)
  (etypecase reply
    (string reply)
    ((vector (unsigned-byte 8)) (babel:octets-to-string reply))
    (null nil)))

(defun handle-endpoint (user request &key error-path)
  (let ((result)
        (message)
        (request (ensure-string-reply request)))
    (cond ((not (get-val user 'last-access-token))
           (setf message "Missing access token"))
          ((not request)
           (setf message "Endpoint returned no values."))
          (t
           (setf result (json:decode-json-from-string request))
           (when (and (consp result)
                      (or (assoc-path result error-path) 
                          (assoc-path result :error) 
                          (assoc-path result :errors)))
             (let ((error-message (or (assoc-path result error-path)
                                      (assoc-path result :error :message)
                                      (if (listp (cdr (assoc-path result :errors)))
                                          (assoc-path (car (cdr (assoc-path result :errors))) :message)
                                          (assoc-path result :errors)))))
               (setf message (if (listp error-message)
                                 (cdr error-message)
                                 error-message))))))
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
