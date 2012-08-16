(in-package :ems)

(defparameter *facebook-client-id* "254949787943221")
(defparameter *facebook-client-secret* "b7ca6b0a7243cb0df0b39d009257e4b2")
(defparameter *facebook-callback-uri* "http://app.digyourbrand.co.za/ems/fbcallback")
(defparameter *facebook-oauth-uri* "https://www.facebook.com/dialog/oauth")
(defparameter *facebook-access-token-uri* "https://graph.facebook.com/oauth/access_token")

(defun facebook-oauth-uri (service-user)
  (format nil "~A?client_id=~A&response_type=code&scope=~A&redirect_uri=~A&state=~A" 
          *facebook-oauth-uri*
          *facebook-client-id*
          "publish_stream,read_stream"
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