(in-package :ems)

(defun get-access-token (client identifier request-token)
  (let ((service (get-twitter-service client identifier)))
    (if service
        (cl-oauth::obtain-access-token  
         (get-access-token-endpoint service)
         request-token  
         :consumer-token (consumer-token service) ))))