(in-package :dyb)

(defun twitter-user-stream (access-token access-secret)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A~A" (random 1234567) stamp))
         (end-point  "https://userstream.twitter.com/1.1/user.json";;"http://api.twitter.com/1/statuses/home_timeline.json"
          
           ))
    
    (drakma:http-request end-point
      
     :method :post
     :additional-headers
     `(("Authorization"
        ,@(build-auth-string
           `(;;("oauth_callback" ,*twitter-callback-uri*)
             ("oauth_consumer_key" ,*twitter-client-id*)
             ("oauth_nonce" ,nonce)
             ("oauth_signature"
              ,(encode-signature
                (hmac-sha1
                 (signature-base-string
                  :uri end-point 
                  :request-method "POST"
                  :parameters `(;;("oauth_callback" ,*twitter-callback-uri*)
                                ("oauth_consumer_key" ,*twitter-client-id*)
                                ("oauth_nonce" ,nonce)
                                ("oauth_signature_method" "HMAC-SHA1")
                                ("oauth_timestamp" ,stamp)
                                ("oauth_token" ,access-token)
                                ("oauth_version" "1.0")))
                 (hmac-key  *twitter-client-secret* 
                            access-secret))
                nil))
             ("oauth_signature_method" "HMAC-SHA1")
             ("oauth_timestamp" ,stamp)
             ("oauth_token" ,access-token)
             ("oauth_version" "1.0")))))
    :want-stream t)))

(defun twitter-home-timeline (access-token access-secret)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A~A" (random 1234567) stamp))
         (end-point  "http://api.twitter.com/1.1/statuses/home_timeline.json?count=800"
          
           ))
    
    (drakma:http-request end-point
      
     :method :get    
     
     :additional-headers
     `(("Authorization"
        ,@(build-auth-string
           `(;;("oauth_callback" ,*twitter-callback-uri*)
             ("oauth_consumer_key" ,*twitter-client-id*)
             ("oauth_nonce" ,nonce)
             ("oauth_signature"
              ,(encode-signature
                (hmac-sha1
                 (signature-base-string
                  :uri end-point 
                  :request-method "GET"
                  :parameters `(;;("oauth_callback" ,*twitter-callback-uri*)
                                ("count" "800")
                                ("oauth_consumer_key" ,*twitter-client-id*)
                                ("oauth_nonce" ,nonce)
                                ("oauth_signature_method" "HMAC-SHA1")
                                ("oauth_timestamp" ,stamp)
                                ("oauth_token" ,access-token)
                                ("oauth_version" "1.0")))
                 (hmac-key  *twitter-client-secret* 
                            access-secret))
                nil))
             ("oauth_signature_method" "HMAC-SHA1")
             ("oauth_timestamp" ,stamp)
             ("oauth_token" ,access-token)
             ("oauth_version" "1.0")))))
    :want-stream nil)))
