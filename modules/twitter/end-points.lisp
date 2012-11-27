(in-package :dyb)

(defun picture-tweet-request (app-id app-secret access-token access-secret message 
                              image-path)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A" (random 1234567)))
         (end-point  "https://api.twitter.com/1.1/statuses/update_with_media.json" ))

    (drakma:http-request 
     end-point
     :method :post
     :parameters `(("status" . ,message)
                   ("media[]" . ,(pathname image-path)))
     :additional-headers
     `(("Authorization"
        ,@(build-auth-string
           `(("oauth_consumer_key" ,app-id)
             ("oauth_nonce" ,nonce)
             ("oauth_signature"
              ,(encode-signature
                (hmac-sha1
                 (signature-base-string
                  :uri end-point 
                  :request-method "POST"
                  :parameters `(("oauth_consumer_key" ,app-id)
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
             ("oauth_version" "1.0")))))
    :want-stream nil
    :preserve-uri nil)))

(defun simple-tweet-request (app-id app-secret access-token access-secret message)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A" (random 1234567)))
         (end-point  "https://api.twitter.com/1.1/statuses/update.json" ))

    (drakma:http-request 
     end-point
     :method :post
     :parameters `(("status" . ,message))
     :additional-headers
     `(("Authorization"
        ,@(build-auth-string
           `(("oauth_consumer_key" ,app-id)
             ("oauth_nonce" ,nonce)
             ("oauth_signature"
              ,(encode-signature
                (hmac-sha1
                 (signature-base-string
                  :uri end-point 
                  :request-method "POST"
                  :parameters `(("oauth_consumer_key" ,app-id)
                                ("oauth_nonce" ,nonce)
                                ("oauth_signature_method" "HMAC-SHA1")
                                ("oauth_timestamp" ,stamp)
                                ("oauth_token" ,access-token)
                                ("oauth_version" "1.0")
                                ("status" ,message)))
                 (hmac-key  app-secret 
                            access-secret))
                  nil))
             ("oauth_signature_method" "HMAC-SHA1")
             ("oauth_timestamp" ,stamp)
             ("oauth_token" ,access-token)
             ("oauth_version" "1.0")))))
    :want-stream nil
    :preserve-uri nil)))

(defun post-twitter (user message &key image-path)
  (when user
    (let ((channel (get-social-channel (get-val user 'channel-user-type))))
      (handle-endpoint
       user
       (if image-path
           (picture-tweet-request
            (get-val channel 'app-id)
            (get-val channel 'app-secret)
            (get-val user 'last-access-token)
            (get-val user 'last-token-secret)
            message
            image-path)
           (simple-tweet-request
            (get-val channel 'app-id)
            (get-val channel 'app-secret)
            (get-val user 'last-access-token)
            (get-val user 'last-token-secret)
            message))
       :result-is-octets-p t))))

(defun twitter-favourite-request (app-id app-secret access-token access-secret tweet-id)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A" (random 1234567)))
         (end-point  "https://api.twitter.com/1.1/favorites/create.json" ))

    (drakma:http-request 
     end-point
     :method :post
     :parameters `(("id" . , tweet-id))
     :additional-headers
     `(("Authorization"
        ,@(build-auth-string
           `(("oauth_consumer_key" ,app-id)
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
                                ("oauth_version" "1.0")))
                 (hmac-key  app-secret 
                            access-secret))
                  nil))
             ("oauth_signature_method" "HMAC-SHA1")
             ("oauth_timestamp" ,stamp)
             ("oauth_token" ,access-token)
             ("oauth_version" "1.0")))))
    :want-stream nil
    :preserve-uri nil)))

(defun favourite-twitter (user tweet-id)
  (when user
    (let ((channel (get-social-channel (get-val user 'channel-user-type))))
      (handle-endpoint
       user
       (twitter-favourite-request
                 (get-val channel 'app-id)
                 (get-val channel 'app-secret)
                 (get-val user 'last-access-token)
                 (get-val user 'last-token-secret)
                 tweet-id)
       :result-is-octets-p t))))

(defun retweet-request (app-id app-secret access-token access-secret tweet-id)
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
           `(("oauth_consumer_key" ,app-id)
             ("oauth_nonce" ,nonce)
             ("oauth_signature"
              ,(encode-signature
                (hmac-sha1
                 (signature-base-string
                  :uri end-point 
                  :request-method "POST"
                  :parameters `(("oauth_consumer_key" ,app-id)
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
    :want-stream nil
    :preserve-uri t)))

(defun retweet-twitter (user tweet-id)
  (when user
    (let ((channel (get-social-channel (get-val user 'channel-user-type))))
      (handle-endpoint
       user
       (retweet-request
                 (get-val channel 'app-id)
                 (get-val channel 'app-secret)
                 (get-val user 'last-access-token)
                 (get-val user 'last-token-secret)
                 tweet-id)
       :result-is-octets-p t))))

(defun reply-tweet-request (app-id app-secret access-token access-secret message at-user)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A" (random 1234567)))
         (end-point  "https://api.twitter.com/1.1/statuses/update.json"))
    (drakma:http-request
     end-point
     :method :post
     :parameters `(("status" . ,(format nil "D @~A ~A" at-user message)))
     :additional-headers
     `(("Authorization"
        ,@(build-auth-string
           `(("oauth_consumer_key" ,app-id)
             ("oauth_nonce" ,nonce)
             ("oauth_signature"
              ,(encode-signature
                (hmac-sha1
                 (signature-base-string
                  :uri end-point 
                  :request-method "POST"
                  :parameters `(("oauth_consumer_key" ,app-id)
                                ("oauth_nonce" ,nonce)
                                ("oauth_signature_method" "HMAC-SHA1")
                                ("oauth_timestamp" ,stamp)
                                ("oauth_token" ,access-token)
                                ("oauth_version" "1.0")
                                ("status" ,(format nil "D @~A ~A" at-user message))))
                 (hmac-key  app-secret 
                            access-secret))
                nil))
             ("oauth_signature_method" "HMAC-SHA1")
             ("oauth_timestamp" ,stamp)
             ("oauth_token" ,access-token)
             ("oauth_version" "1.0")))))
     :want-stream nil
     :preserve-uri nil)))

(defun reply-twitter (user message at-user)
  (when user
    (let ((channel (get-social-channel (get-val user 'channel-user-type))))
      (handle-endpoint
       user
       (reply-tweet-request
                 (get-val channel 'app-id)
                 (get-val channel 'app-secret)
                 (get-val user 'last-access-token)
                 (get-val user 'last-token-secret)
                 message at-user)
       :result-is-octets-p t))))

(defun twitter-home-timeline-request (app-id app-secret access-token access-secret)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A~A" (random 1234567) stamp))
         (end-point  "http://api.twitter.com/1.1/statuses/home_timeline.json?count=800"))
    
    (drakma:http-request 
     end-point
     :method :get    
     :additional-headers
     `(("Authorization"
        ,@(build-auth-string
           `(("oauth_consumer_key" ,app-id)
             ("oauth_nonce" ,nonce)
             ("oauth_signature"
              ,(encode-signature
                (hmac-sha1
                 (signature-base-string
                  :uri end-point 
                  :request-method "GET"
                  :parameters `(("count" "800")
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

(defun twitter-home-timeline (user)
  (when user
    (let ((channel (get-social-channel (get-val user 'channel-user-type))))
      (handle-endpoint
       user
       (twitter-home-timeline-request
                 (get-val channel 'app-id)
                 (get-val channel 'app-secret)
                 (get-val user 'last-access-token)
                 (get-val user 'last-token-secret))
       :result-is-octets-p t))))

(defun twitter-followers-request (app-id app-secret access-token access-secret)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A~A" (random 1234567) stamp))
         (end-point  "https://api.twitter.com/1.1/followers/ids.json"))
    
    (drakma:http-request 
     end-point
     :method :get    
     :additional-headers
     `(("Authorization"
        ,@(build-auth-string
           `(("oauth_consumer_key" ,app-id)
             ("oauth_nonce" ,nonce)
             ("oauth_signature"
              ,(encode-signature
                (hmac-sha1
                 (signature-base-string
                  :uri end-point 
                  :request-method "GET"
                  :parameters `(("oauth_consumer_key" ,app-id)
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

(defun twitter-followers (user)
  (when user
    (let ((channel (get-social-channel (get-val user 'channel-user-type))))
      (handle-endpoint
       user
       (twitter-followers-request
                 (get-val channel 'app-id)
                 (get-val channel 'app-secret)
                 (get-val user 'last-access-token)
                 (get-val user 'last-token-secret))
       :result-is-octets-p t))))

(defun twitter-profile-request (app-id app-secret access-token access-secret)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A~A" (random 1234567) stamp))
         (end-point  "https://api.twitter.com/1.1/account/verify_credentials.json"))
    
    (drakma:http-request 
     end-point
     :method :get    
     :additional-headers
     `(("Authorization"
        ,@(build-auth-string
           `(("oauth_consumer_key" ,app-id)
             ("oauth_nonce" ,nonce)
             ("oauth_signature"
              ,(encode-signature
                (hmac-sha1
                 (signature-base-string
                  :uri end-point 
                  :request-method "GET"
                  :parameters `(("oauth_consumer_key" ,app-id)
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

(defun twitter-profile (user)
  (when user
    (let ((channel (get-social-channel (get-val user 'channel-user-type))))
      (handle-endpoint
       user
       (twitter-profile-request
                 (get-val channel 'app-id)
                 (get-val channel 'app-secret)
                 (get-val user 'last-access-token)
                 (get-val user 'last-token-secret))
       :result-is-octets-p t))))


(defun twitter-verify-credentials (app-id app-secret access-token access-secret)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A~A" (random 1234567) stamp))
         (end-point  "https://api.twitter.com/1.1/account/verify_credentials.json"))
    
    (drakma:http-request 
     end-point
     :method :get    
     :additional-headers
     `(("Authorization"
        ,@(build-auth-string
           `(("oauth_consumer_key" ,app-id)
             ("oauth_nonce" ,nonce)
             ("oauth_signature"
              ,(encode-signature
                (hmac-sha1
                 (signature-base-string
                  :uri end-point 
                  :request-method "GET"
                  :parameters `(("oauth_consumer_key" ,app-id)
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


(defun twitter-user-stream (app-id app-secret access-token access-secret)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A~A" (random 1234567) stamp))
         (end-point  "https://userstream.twitter.com/1.1/user.json"))
    
    (drakma:http-request 
     end-point
     :method :post
     :additional-headers
     `(("Authorization"
        ,@(build-auth-string
           `(("oauth_consumer_key" ,app-id)
             ("oauth_nonce" ,nonce)
             ("oauth_signature"
              ,(encode-signature
                (hmac-sha1
                 (signature-base-string
                  :uri end-point 
                  :request-method "POST"
                  :parameters `(("oauth_consumer_key" ,app-id)
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