(in-package :dyb)

(defun simple-tweet (app-id app-secret access-token access-secret message)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A" (random 1234567)))
         (end-point  "https://api.twitter.com/1.1/statuses/update.json" ))

    (drakma:http-request 
     ;;(format nil "~A?status=~A"  end-point (replace-all message " " "+"))   
      end-point
     :method :post
     :parameters `(("status" . ,message))
     :additional-headers
     `(("Authorization"
        ,@(build-auth-string
           `(
             ("oauth_consumer_key" ,app-id)
             ("oauth_nonce" ,nonce)
             ("oauth_signature"
              ,(encode-signature
                (hmac-sha1
                 (signature-base-string
                  :uri end-point 
                  :request-method "POST"
                  :parameters `(
                                ("oauth_consumer_key" ,app-id)
                                ("oauth_nonce" ,nonce)
                                ("oauth_signature_method" "HMAC-SHA1")
                                ("oauth_timestamp" ,stamp)
                                ("oauth_token" ,access-token)
                                ("oauth_version" "1.0")
                                ("status" ,message)
                                ))
                 (hmac-key  app-secret 
                            access-secret))
                  nil))
             ("oauth_signature_method" "HMAC-SHA1")
             ("oauth_timestamp" ,stamp)
             ("oauth_token" ,access-token)
             ("oauth_version" "1.0")
             
             ))))
    :want-stream nil
    :preserve-uri nil)))

(defun twitter-favourite (app-id app-secret access-token access-secret tweet-id)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A" (random 1234567)))
         (end-point  "https://api.twitter.com/1.1/favorites/create.json" ))

    (drakma:http-request 
     ;;(format nil "~A?id=~A" end-point tweet-id) 
     end-point
     :method :post
     :parameters `(("id" . , tweet-id))
     :additional-headers
     `(("Authorization"
        ,@(build-auth-string
           `(
             ("oauth_consumer_key" ,app-id)
             ("oauth_nonce" ,nonce)
             ("oauth_signature"
              ,(encode-signature
                (hmac-sha1
                 (signature-base-string
                  :uri end-point 
                  :request-method "POST"
                  :parameters `(("id" tweet-id)
                                ("oauth_consumer_key" ,app-id)
                                ("oauth_nonce" ,nonce)
                                ("oauth_signature_method" "HMAC-SHA1")
                                ("oauth_timestamp" ,stamp)
                                ("oauth_token" ,access-token)
                                ("oauth_version" "1.0")
                                
                                ))
                 (hmac-key  app-secret 
                            access-secret))
                  nil))
             ("oauth_signature_method" "HMAC-SHA1")
             ("oauth_timestamp" ,stamp)
             ("oauth_token" ,access-token)
             ("oauth_version" "1.0")
             
             ))))
    :want-stream nil
    :preserve-uri nil)))

(defun retweet (app-id app-secret access-token access-secret tweet-id)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A" (random 1234567)))
         (end-point  (format nil
                             "https://api.twitter.com/1.1/statuses/retweet/~A.json"
                             tweet-id) ))

    (drakma:http-request 
     end-point   
     :method :post
     :additional-headers
     `(("Authorization"
        ,@(build-auth-string
           `(
             ("oauth_consumer_key" ,app-id)
             ("oauth_nonce" ,nonce)
             ("oauth_signature"
              ,(encode-signature
                (hmac-sha1
                 (signature-base-string
                  :uri end-point 
                  :request-method "POST"
                  :parameters `(
                                ("oauth_consumer_key" ,app-id)
                                ("oauth_nonce" ,nonce)
                                ("oauth_signature_method" "HMAC-SHA1")
                                ("oauth_timestamp" ,stamp)
                                ("oauth_token" ,access-token)
                                ("oauth_version" "1.0")
                                
                                ))
                 (hmac-key  app-secret 
                            access-secret))
                  nil))
             ("oauth_signature_method" "HMAC-SHA1")
             ("oauth_timestamp" ,stamp)
             ("oauth_token" ,access-token)
             ("oauth_version" "1.0")
             
             ))))
    :want-stream nil
    :preserve-uri t)))

(defun reply-tweet (app-id app-secret access-token access-secret message at-user)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A" (random 1234567)))
         (end-point  "https://api.twitter.com/1.1/statuses/update.json" ))
    (drakma:http-request
     end-point
     ;;(format nil "~A?status=~A" end-point (replace-all (format nil "D @~A ~A" at-user message) " " "+"))   
     :method :post
     :parameters `(("status" . ,(format nil "D @~A ~A" at-user message)))
     :additional-headers
     `(("Authorization"
        ,@(build-auth-string
           `(
             ("oauth_consumer_key" ,app-id)
             ("oauth_nonce" ,nonce)
             ("oauth_signature"
              ,(encode-signature
                (hmac-sha1
                 (signature-base-string
                  :uri end-point 
                  :request-method "POST"
                  :parameters `(
                                ("oauth_consumer_key" ,app-id)
                                ("oauth_nonce" ,nonce)
                                ("oauth_signature_method" "HMAC-SHA1")
                                ("oauth_timestamp" ,stamp)
                                ("oauth_token" ,access-token)
                                ("oauth_version" "1.0")
                                ("status" ,(format nil "D @~A ~A" at-user message))
                                ))
                 (hmac-key  app-secret 
                            access-secret))
                nil))
             ("oauth_signature_method" "HMAC-SHA1")
             ("oauth_timestamp" ,stamp)
             ("oauth_token" ,access-token)
             ("oauth_version" "1.0")
             
             ))))
     :want-stream nil
     :preserve-uri nil)))

(defun twitter-user-stream (app-id app-secret access-token access-secret)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A~A" (random 1234567) stamp))
         (end-point  "https://userstream.twitter.com/1.1/user.json"
           ))
    
    (drakma:http-request end-point
      
     :method :post
     :additional-headers
     `(("Authorization"
        ,@(build-auth-string
           `(;;("oauth_callback" ,*twitter-callback-uri*)
             ("oauth_consumer_key" ,app-id)
             ("oauth_nonce" ,nonce)
             ("oauth_signature"
              ,(encode-signature
                (hmac-sha1
                 (signature-base-string
                  :uri end-point 
                  :request-method "POST"
                  :parameters `(;;("oauth_callback" ,*twitter-callback-uri*)
                                ("oauth_consumer_key" ,app-id)
                                ("oauth_nonce" ,nonce)
                                ("oauth_signature_method" "HMAC-SHA1")
                                ("oauth_timestamp" ,stamp)
                                ("oauth_token" ,access-token)
                                ("oauth_version" "1.0")))
                 (hmac-key  app-secret 
                            access-secret))
                nil))
             ("oauth_signature_method" "HMAC-SHA1")
             ("oauth_timestamp" ,stamp)
             ("oauth_token" ,access-token)
             ("oauth_version" "1.0")))))
    :want-stream t)))

(defun twitter-home-timeline (app-id app-secret access-token access-secret)
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
             ("oauth_consumer_key" ,app-id)
             ("oauth_nonce" ,nonce)
             ("oauth_signature"
              ,(encode-signature
                (hmac-sha1
                 (signature-base-string
                  :uri end-point 
                  :request-method "GET"
                  :parameters `(;;("oauth_callback" ,*twitter-callback-uri*)
                                ("count" "800")
                                ("oauth_consumer_key" ,app-id)
                                ("oauth_nonce" ,nonce)
                                ("oauth_signature_method" "HMAC-SHA1")
                                ("oauth_timestamp" ,stamp)
                                ("oauth_token" ,access-token)
                                ("oauth_version" "1.0")))
                 (hmac-key  app-secret
                            access-secret))
                nil))
             ("oauth_signature_method" "HMAC-SHA1")
             ("oauth_timestamp" ,stamp)
             ("oauth_token" ,access-token)
             ("oauth_version" "1.0")))))
    :want-stream nil)))


(defun twitter-followers (app-id app-secret access-token access-secret)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A~A" (random 1234567) stamp))
         (end-point  "https://api.twitter.com/1.1/followers/ids.json"
          
           ))
    
    (drakma:http-request end-point
      
     :method :get    
     
     :additional-headers
     `(("Authorization"
        ,@(build-auth-string
           `(
             ("oauth_consumer_key" ,app-id)
             ("oauth_nonce" ,nonce)
             ("oauth_signature"
              ,(encode-signature
                (hmac-sha1
                 (signature-base-string
                  :uri end-point 
                  :request-method "GET"
                  :parameters `(
                                
                                ("oauth_consumer_key" ,app-id)
                                ("oauth_nonce" ,nonce)
                                ("oauth_signature_method" "HMAC-SHA1")
                                ("oauth_timestamp" ,stamp)
                                ("oauth_token" ,access-token)
                                ("oauth_version" "1.0")))
                 (hmac-key  app-secret
                            access-secret))
                nil))
             ("oauth_signature_method" "HMAC-SHA1")
             ("oauth_timestamp" ,stamp)
             ("oauth_token" ,access-token)
             ("oauth_version" "1.0")))))
    :want-stream nil)))


(defun twitter-verify-credentials (app-id app-secret access-token access-secret)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A~A" (random 1234567) stamp))
         (end-point  "https://api.twitter.com/1.1/account/verify_credentials.json"
          
           ))
    
    (drakma:http-request end-point
      
     :method :get    
     
     :additional-headers
     `(("Authorization"
        ,@(build-auth-string
           `(
             ("oauth_consumer_key" ,app-id)
             ("oauth_nonce" ,nonce)
             ("oauth_signature"
              ,(encode-signature
                (hmac-sha1
                 (signature-base-string
                  :uri end-point 
                  :request-method "GET"
                  :parameters `(
                                
                                ("oauth_consumer_key" ,app-id)
                                ("oauth_nonce" ,nonce)
                                ("oauth_signature_method" "HMAC-SHA1")
                                ("oauth_timestamp" ,stamp)
                                ("oauth_token" ,access-token)
                                ("oauth_version" "1.0")))
                 (hmac-key  app-secret
                            access-secret))
                nil))
             ("oauth_signature_method" "HMAC-SHA1")
             ("oauth_timestamp" ,stamp)
             ("oauth_token" ,access-token)
             ("oauth_version" "1.0")))))
    :want-stream nil)))


(defun twitter-profile (app-id app-secret access-token access-secret)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A~A" (random 1234567) stamp))
         (end-point  "https://api.twitter.com/1.1/account/verify_credentials.json"
          
           ))
    
    (drakma:http-request end-point
      
     :method :get    
     
     :additional-headers
     `(("Authorization"
        ,@(build-auth-string
           `(
             ("oauth_consumer_key" ,app-id)
             ("oauth_nonce" ,nonce)
             ("oauth_signature"
              ,(encode-signature
                (hmac-sha1
                 (signature-base-string
                  :uri end-point 
                  :request-method "GET"
                  :parameters `(
                                
                                ("oauth_consumer_key" ,app-id)
                                ("oauth_nonce" ,nonce)
                                ("oauth_signature_method" "HMAC-SHA1")
                                ("oauth_timestamp" ,stamp)
                                ("oauth_token" ,access-token)
                                ("oauth_version" "1.0")))
                 (hmac-key  app-secret
                            access-secret))
                nil))
             ("oauth_signature_method" "HMAC-SHA1")
             ("oauth_timestamp" ,stamp)
             ("oauth_token" ,access-token)
             ("oauth_version" "1.0")))))
    :want-stream nil)))