(in-package :ems)



(defun facebook-oauth-uri (service-user)
  (format nil "~A?client_id=~A&response_type=code&scope=~A&redirect_uri=~A&state=~A" 
          *facebook-oauth-uri*
          *facebook-client-id*
          "publish_stream,read_stream,read_insights,read_mailbox"
          *facebook-callback-uri*
          (xid service-user)))

(defun facebook-oauth-access-token-uri (code)
  (format nil "~A?client_id=~A&redirect_uri=~A&client_secret=~A&code=~A" 
          *facebook-access-token-uri*
          *facebook-client-id*
          *facebook-callback-uri*
          *facebook-client-secret*
          code))

(define-easy-handler (callback :uri "/ems/fbcallback") ()
  (setf (session-value "facebook-oauth-error") nil)
  (when (parameter "code")
    
    (multiple-value-bind (body status)
        ;;Use the access code to get a access token
        (drakma:http-request 
         (facebook-oauth-access-token-uri (parameter "code")))
      (setf (session-value "facebook-access-token") nil)
      (if (equal status 200)
          (let* ((querystring (split-string body #\&))
                 (access-token (split-string (first querystring) #\=))
                 ;;(expiry (split-string (second querystring) #\=))
                 
                 )
            (setf (session-value "facebook-access-token") access-token)


            ;;Fetch something interesting with your access token.
            (multiple-value-bind (bodyx)
                (drakma:http-request 
                 (format nil "https://graph.facebook.com/me?access_token=~A" 
                         (second access-token)))
              (setf (session-value "facebook-user-id") 
                    (get-post-id (json::decode-json-from-string bodyx)))

             
              (let* ((user (get-service-user-by-id 
                            (parse-integer (parameter "state"))))
                     (old-user (if user
                                   (copy user))))

                
               ;; (setf (get-val user 'user-id) 
                ;;      (get-post-id (json::decode-json-from-string bodyx)))
                ;(break "~A" (second access-token))
                (setf (get-val user 'last-access-token) 
                      (second access-token))

                (if (and old-user (xid old-user))
                    (persist user :old-object old-user)
                  (persist user)))
  
              ;;(format nil "~A" (json::decode-json-from-string bodyx))
              
              (redirect "http://app.digyourbrand.co.za/ems/service-users?")))
          (setf (session-value "facebook-oauth-error") 
                (format nil "Something went wrong:~%~A"
                        (json::decode-json-from-string body)))))))


(defun update-facebook-posts-for-users (&optional grid)
  

;;(find-docs 'list 
;;           (lambda (doc)
;;             (match-context-entities doc))
;;           (service-users-collection))
  (dolist (user (coerce (service-users) 'list ))
        
        (when (and user (string-equal (get-val user 'doc-status) "Active"))
          ;;TODO: How to get error messages in for users without access tokens.
          (when (string-equal (get-val user 'service-user-type) "Facebook")
              (when (get-val user 'last-access-token)

                (multiple-value-bind (bodyx)
                    (drakma:http-request 
                     (format nil "https://graph.facebook.com/~A/feed?limit=2000&access_token=~A" 
                             (url-encode (get-val user 'user-id))
                             (get-val user 'last-access-token)))
              
              
                  (let ((post-list (rest (first (json::decode-json-from-string bodyx)))))

                    (if (populate-generic-db-from-post post-list )
                        (if (string-equal (car (car post-list)) "MESSAGE")
                  
                            (when grid 
                    
                              (setf (error-message grid)
                                    (format nil (error-message grid) "~A~%~A%"
                                            (error-message grid)
                                            (car (cdr post-list))) ) ))
                        ))
                  ))))))