(in-package :ems)



(define-easy-handler (dybcallback :uri "/ems/oauthcallback") (channel)
  (when channel
    (let* ((social-channel (get-social-channel channel))
           (error-code (parameter (get-system-parameter-name 
                                   social-channel 
                                   "Request Token" 
                                   "error")))
           (error-description (parameter (get-system-parameter-name 
                                          social-channel 
                                          "Request Token" 
                                          "error-description")))
           (verification-code (parameter (get-system-parameter-name 
                                          social-channel 
                                          "Request Token" 
                                          "verification-code")))
           (request-token (parameter (get-system-parameter-name 
                                      social-channel 
                                      "Request Token" 
                                      "request-token"))))
    
      (when request-token
        (let ((user (if (string-equal (get-val social-channel 'auth-type) "OAuth2")
                        (get-channel-user-by-user-id  verification-code)
                        (get-channel-user-by-auth-token (parameter "oauth_token"))) )
              (access-token-end (get-end-point social-channel "Access Token")))

          
          (unless user
            (setf error-description 
                  (format nil "User ~A not found in db." verification-code) ))
          (when user
            (setf (get-val user 'request-token) 
                  request-token)

            
            (multiple-value-bind (body status)
                ;;TODO: Change to generic access request
                (if (string-equal (get-val social-channel 'auth-type) "OAuth2")
                    (drakma:http-request 
                     (oauth2-access-token-uri social-channel request-token)
                     :preserve-uri t )
                    (oauth1-access social-channel
                                   (parameter "oauth_token") 
                                   (parameter "oauth_verifier")
                                   :request-secret (get-val user 'request-secret)
                                   ))
              (unless (stringp body) 
                (setf body (babel:octets-to-string body)))
              
              (cond ((equal status 200)

                     (cond ((string-equal (get-val access-token-end 'return-type) 
                                          "Query String")

                            (let ((response (parse-query-string body)))
                              (setf (get-val user 'last-access-token) 
                                    (cdr (assoc-path response 
                                                     (get-system-parameter-name 
                                                      social-channel 
                                                      "Access Token" 
                                                      "access-token"))))
                              (setf (get-val user 'last-token-secret) 
                                    (cdr (assoc-path response 
                                                     (get-system-parameter-name 
                                                      social-channel 
                                                      "Access Token" 
                                                      "access-token-secret"))))
                              (if (assoc-path response 
                                              (get-system-parameter-name 
                                               social-channel 
                                               "Access Token" 
                                               "access-token-expiry"))
                                  (setf (get-val user 'access-token-expiry-date) 
                                        (+ (get-universal-time)
                                           (parse-integer
                                            (cdr (assoc-path response 
                                                             (get-system-parameter-name 
                                                              social-channel 
                                                              "Access Token" 
                                                              "access-token-expiry")))))))))
                           
                           ((string-equal (get-val access-token-end 'return-type) "JSON")
                            )
                           ((string-equal (get-val access-token-end 'return-type) "XML")
                            ))
                     (if (string-equal *installation* "Live Serve")
                         (redirect "http://app.digyourbrand.co.za/ems/channel-users")
                         (redirect "http://local.dataxware.co.za/ems/channel-users")))
                    ((equal status 401)
                     (setf error-description body))))
            (persist user))))

      (when error-description
        (format nil "~A" error-description))
    
  
      )))