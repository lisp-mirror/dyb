(in-package :dyb)
   
(defun twitter-request (user end-point 
                        &key method parameters 
                        content
                        content-type
                        signature-parameters
                        want-stream 
                        preserve-uri
                        (handle-response-p t)
                        (parse-json-p t)
                        (body-to-string-p t))
  :doc "Wraps drakma-request in a oauth aware request."
  (let ((channel (get-social-channel (get-val user 'channel-user-type)))
        (response)) 

    (with-slots (last-access-token last-token-secret) 
        user
      (with-slots (app-id app-secret)
          channel
        
        (setf response (oauth1-drakma-request end-point app-id app-secret  
                                              :access-token last-access-token 
                                              :access-secret last-token-secret
                                              :method method :parameters parameters 
                                              :content content
                                              :content-type content-type
                                              :signature-parameters signature-parameters 
                                              :want-stream want-stream 
                                              :preserve-uri preserve-uri
                                              :body-to-string-p body-to-string-p
                                              :parse-json-p parse-json-p))
        (if handle-response-p
            (handle-endpoint
             user
             response)
            response)))))

(defun set-twitter-request-token (user)
  :doc "Get and save request token and secret for user. 
This is used when requesting an oauth access token."
  (let* ((response (twitter-request-token) ))
    (cond ((equal (status-code response) 200)
           (let ((token-string (parse-query-string (body-or-stream response))))
             (setf (get-val user 'request-token)
                   (cdr (assoc-path token-string "oauth_token")))
             (setf (get-val user 'verification-code)
                   (cdr (assoc-path token-string "oauth_token")))
             (setf (get-val user 'request-secret)
                   (cdr (assoc-path token-string "oauth_token_secret"))))
           (persist user))
          (t
           (error response)))))