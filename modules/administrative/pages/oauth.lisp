(in-package :dyb)


(define-easy-handler (dybcallback :uri "/dyb/oauthcallback") (channel)
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
                        (get-channel-user-by-verification-code verification-code)
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
                            ;;TODO: Must this be implemented?
                            )
                           ((string-equal (get-val access-token-end 'return-type) "XML")
                            ;;TODO: Must this be implemented?
                            ))
                     (cond ((string-equal channel "LinkedIn")
                            (multiple-value-bind (result status ) 
                                (linkedin-profile user)
                       
                              (when result
                                (setf (get-val user 'user-id) (gpv result :id))
                                (setf (get-val user 'channel-user-name) 
                                      (format nil "~A ~A" (gpv result :first-name)
                                              (gpv result :last-name)))
                                (setf (gethash "profile" (get-val user 'user-data)) result)
                                )))
                           ((string-equal channel "Facebook")
                            (multiple-value-bind (result status error) 
                                (facebook-profile user)                       
                              
                              (when result
                                (setf (get-val user 'user-id) (gpv result :id))
                                (setf (get-val user 'channel-user-name) 
                                      (gpv result :username))
                                (setf (gethash "profile" (get-val user 'user-data)) result)

                                (multiple-value-bind (accounts)
                                    (facebook-accounts user)

                                    (when accounts
                                      (setf (gethash "accounts" 
                                                     (get-val user 'user-data)) accounts)
                                      ))
                                )))
                           ((string-equal channel "Twitter")
                            (multiple-value-bind (result status error) 
                                (twitter-verify-credentials
                                 (get-val social-channel 'app-id)
                                 (get-val social-channel 'app-secret)
                                 (get-val user 'last-access-token)
                                 (get-val user 'last-token-secret)
                                 )              
         
                              (if (stringp result) 
                                (setf result (json::decode-json-from-string result))
                                (setf result (json::decode-json-from-string 
                                              (babel:octets-to-string result))))
                              
                              (when result
                                (setf (get-val user 'user-id) (gpv result :id))
                                (setf (get-val user 'channel-user-name) 
                                      (gpv result :screen--name))
                                (setf (gethash "profile" (get-val user 'user-data)) result)
                                ))))

                     (persist user)

                     (if (string-equal *installation* "Live Serve")
                         (redirect "http://app.digyourbrand.co.za/dyb/channel-users")
                         (redirect "http://local.dataxware.co.za/dyb/channel-users")))
                    ((equal status 401)
                     (setf error-description body))))
            
            )))

      (when error-description
        (format nil "~A" error-description))
    
  
      )))