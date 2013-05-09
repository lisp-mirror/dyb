(in-package :dyb)

;;NOTE

;;;http://jaanus.com/post/1451098316/understanding-the-guts-of-twit
;;;https://dev.twitter.com/docs/auth/creating-signature

;;use the following to test signing
;;;http://quonos.nl/oauthTester/

;;What to look out for when getting 401 errors
;;;http://codingthis.com/programming/php/when-oauth-goes-wrong-debugging-signature-mismatch-issues-in-php/

(define-easy-handler (twitter-callback :uri "/dyb/twitter-oauth-callback") ()
  (let* ((error-description)
         ;;Fuck knows why the oauth token is the actual verification code
         (user (get-channel-user-by-verification-code (parameter "oauth_token"))))
  
    (unless user
      (setf error-description 
            (format nil "User ~A not found in db." (parameter "oauth_token")) ))
    (when user

      (let ((token-request (twitter-access-token 
                            (parameter "oauth_token") 
                            (parameter "oauth_verifier")
                            (get-val user 'request-secret))))
            
        (when token-request
          (cond ((equal (status-code token-request) 200)

                 (let ((response (parse-query-string (body-or-stream token-request))))
                   (setf (get-val user 'last-access-token) 
                         (cdr (assoc-path response 
                                          "oauth_token")))
                   (setf (get-val user 'last-token-secret) 
                         (cdr (assoc-path response 
                                          "oauth_token_secret"))))

                 (let ((credentials (twitter-verify-credentials user)))
                   
                   (cond ((equal (status-code credentials) 200)
                          (when credentials
                            (let ((result (body-or-stream credentials)))
                              (setf (get-val user 'user-id) (gpv result :id))
                              (setf (get-val user 'channel-user-name) 
                                    (gpv result :screen--name))
                              (setf (gethash "profile" 
                                             (get-val user 'user-data)) 
                                    result))))
                         (t
                          nil)))

                 (persist user)
                 
                 (if (string-equal *installation* "Live Serve")
                     (redirect "http://app.digyourbrand.co.za/dyb/channel-users")
                     (redirect "http://local.dataxware.co.za/dyb/channel-users")))
                ((equal (status-code token-request) 401)
                 (if (search "expired Token" (body-or-stream token-request))
                     (if (string-equal *installation* "Live Serve")
                         (redirect 
                          "http://app.digyourbrand.co.za/dyb/channel-users?error=expired-token")
                         (redirect 
                          "http://local.dataxware.co.za/dyb/channel-users?error=expired-token")))
                 (setf error-description (body-or-stream token-request)))
                (t
                       
                 (setf error-description (body-or-stream token-request)))))))

    (when error-description
      (format nil "~A" error-description))))
       

     

