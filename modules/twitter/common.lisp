(in-package :dyb)
   
(defun twitter-request (user end-point 
                        &key method parameters signature-parameters
                        want-stream 
                        preserve-uri
                        (handle-response-p t)
                        (parse-json-p t)
                        (body-to-string-p t))
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
