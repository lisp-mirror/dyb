(in-package :dyb)

(defun twitter-request-token ()
  (let* ((end-point "https://api.twitter.com/oauth/request_token")
         (channel (get-social-channel "Twitter"))
         (response (oauth1-drakma-request
                  end-point
                  (get-val channel 'app-id)
                  (get-val channel 'app-secret)
                  :callback-uri 
                  (if (string-equal *installation* "Live Serve")
                      "http://app.digyourbrand.co.za/dyb/twitter-oauth-callback"
                      "http://local.dataxware.co.za/dyb/twitter-oauth-callback")
                  :method :post
                  :preserve-uri t)))
    response))

(defun twitter-access-token (oauth-token oauth-verifier request-token-secret)
  (let* ((end-point "https://api.twitter.com/oauth/access_token")
         (channel (get-social-channel "Twitter"))

         (response (oauth1-drakma-request
                  end-point
                  (get-val channel 'app-id)
                  (get-val channel 'app-secret) 
                   :access-token oauth-token
                   :access-secret request-token-secret
                   :oauth-verifier oauth-verifier
                  :callback-uri 
                  (if (string-equal *installation* "Live Serve")
                      "http://app.digyourbrand.co.za/dyb/twitter-oauth-callback"
                      "http://local.dataxware.co.za/dyb/twitter-oauth-callback"
                      )
                  :method :post
                  :preserve-uri t)))
    response))

(defun picture-tweet-request (app-id app-secret access-token access-secret message 
                              image-path
                              link-url)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A" (random 1234567)))
         (status (if link-url
                     (format nil "~A ~A" 
                             (string-trim 
                              '(#\space #\tab #\newline 
                                #\linefeed #\return) 
                              message) link-url)
                     (string-trim '(#\space #\tab #\newline 
                                    #\linefeed #\return) 
                                  message)))
         (end-point  "https://api.twitter.com/1.1/statuses/update_with_media.json" ))

    (drakma-request 
     end-point
     :method :post
     :content-type "application/x-www-form-urlencoded; charset=UTF-8"

     :parameters `(("status" . ,(if link-url
                                     (format nil "~A ~A" (string-trim 
                                                          '(#\space #\tab #\newline 
                                                            #\linefeed #\return)
                                                          message) link-url)
                                     (string-trim '(#\space #\tab #\newline 
                                                    #\linefeed #\return) 
                                                  message)))
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
    :preserve-uri t)))


(defun simple-tweet-request (app-id app-secret access-token access-secret message link-url)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A" (random 1234567)))
         (status (if link-url
                     (format nil "~A ~A" 
                             (string-trim 
                              '(#\space #\tab #\newline 
                                #\linefeed #\return) 
                                                          message) link-url)
                     (string-trim '(#\space #\tab #\newline 
                                    #\linefeed #\return) 
                                  message)))
         (end-point  "https://api.twitter.com/1.1/statuses/update.json" )
         )

    (drakma-request 
                  end-point
                  :method :post
                  :content-type "application/x-www-form-urlencoded; charset=UTF-8"
                  :content (alist-to-url-encoded-string `(("status" . ,status))
                                       +utf-8+)
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
                                             ("status" ,status)
                                             
                                             ))
                              (hmac-key  app-secret 
                                         access-secret))
                             nil))
                          ("oauth_signature_method" "HMAC-SHA1")
                          ("oauth_timestamp" ,stamp)
                          ("oauth_token" ,access-token)
                          ("oauth_version" "1.0")))))
                  :want-stream nil
                  :preserve-uri t)))

(defun post-twitter (user message &key image-path link-url)

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
            image-path
            link-url)
           (simple-tweet-request
            (get-val channel 'app-id)
            (get-val channel 'app-secret)
            (get-val user 'last-access-token)
            (get-val user 'last-token-secret)
            message
            link-url))))))

(defun twitter-favourite-request (app-id app-secret access-token access-secret tweet-id)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A" (random 1234567)))
         (end-point  "https://api.twitter.com/1.1/favorites/create.json" ))

    (drakma-request
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
                 tweet-id)))))

(defun retweet-request (app-id app-secret access-token access-secret tweet-id)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A" (random 1234567)))
         (end-point  (format nil
                             "https://api.twitter.com/1.1/statuses/retweet/~A.json"
                             tweet-id) ))

    (drakma-request 
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
                 tweet-id)))))

(defun reply-tweet-request (app-id app-secret access-token access-secret message at-user)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A" (random 1234567)))
         (end-point  "https://api.twitter.com/1.1/statuses/update.json"))
    (drakma-request
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
                 message at-user)))))


(defun twitter-home-timeline (user &key since-id max-id)
  (let* ((end-point (format nil "http://api.twitter.com/1.1/statuses/home_timeline.json?user_id=~A&count=800~A~A&include_rts=true&include_entities=true&contributor_details=true" 
                            (get-val user 'user-id)
                            (if max-id 
                                (format nil "&max_id=~A" (- max-id 1))
                                "")
                            (if since-id 
                                (format nil "&since_id=~A" since-id)
                                ""))))
    (twitter-request
                    user
                    end-point
                    :method :get
                    :signature-parameters 
                    `(("contributor_details" "true")
                      ("count" "800")
                      ("include_entities" "true")
                      ("include_rts" "true")
                      ,@(if max-id
                            `(("max_id" ,(format nil "~A" max-id))))
                      ,@(if since-id
                            `(("since_id" ,(format nil "~A" since-id))))
                      ("user_id" ,(format nil "~A" (get-val user 'user-id)))))))



(defun twitter-mention-timeline (user &key since-id)
  (let* ((end-point (format nil "http://api.twitter.com/1.1/statuses/mentions_timeline.json?user_id=~A&count=800&since_id=~A&include_rts=true&include_entities=true&contributor_details=true" 
                            (get-val user 'user-id)
                            (if since-id
                                since-id
                                1))))
    (twitter-request
                    user
                    end-point
                    :method :get
                    :signature-parameters 
                    `(("contributor_details" "true")
                      ("count" "800")
                      ("include_entities" "true")
                      ("include_rts" "true")
                      ("since_id"  ,(format nil "~A" (if since-id
                                                         since-id
                                                         1)))
                      ("user_id" , (format nil "~A" (get-val user 'user-id)))))))




(defun twitter-followers (user)
  (let* ((end-point "https://api.twitter.com/1.1/followers/ids.json"))
    (twitter-request
                    user
                    end-point
                    :method :get)))


(defun twitter-profile (user)
  (let* ((end-point "https://api.twitter.com/1.1/account/verify_credentials.json"))
    (twitter-request
     user
     end-point
     :method :get)))

(defun twitter-verify-credentials (user)
  (let* ((end-point "https://api.twitter.com/1.1/account/verify_credentials.json"))
    (twitter-request
     user
     end-point
     :method :get
     :handle-response-p nil)))

(defun twitter-user-stream (user)
  (let* ((end-point "https://userstream.twitter.com/1.1/user.json"))
    (twitter-request
     user
     end-point
     :method :post
     :want-stream t
     :handle-response-p nil)))

(defun twitter-search (user &key since-id)
  (let* ((end-point (format nil "http://api.twitter.com/1.1/statuses/home_timeline.json?user_id=~A&count=800&since_id=~A&include_rts=true&contributor_details=true" 
                            (get-val user 'user-id)
                             (if since-id
                                 since-id
                                 1))))
    (twitter-request
     user
     end-point
     :method :get
     :signature-parameters
     `(("contributor_details" "true")
       ("count" "800")
       ("include_rts" "true")
       ("since_id"  ,(format nil "~A" (if since-id
                                          since-id
                                          1)))
       ("user_id" ,(format nil "~A" (get-val user 'user-id))))
     :handle-response-p nil)))

(defun twitter-get-tweet (user &key tweet-id)
  (let* ((end-point  (format nil "http://api.twitter.com/1.1/statuses/show.json?id=~A" 
                             tweet-id)))
    (twitter-request
     user
     end-point
     :method :get
     :signature-parameters `(("id" ,tweet-id))
     :handle-response-p nil)))

(defun twitter-get-history-tweet (channel-user tweet-id)
  (when channel-user
    (when (get-val channel-user 'last-access-token)
                (let* (
             (response (twitter-get-tweet
                        channel-user  :tweet-id tweet-id)))
        (parse-tweets 
         channel-user
         (list response)
         'history-import)))))

(defun twitter-direct-messages-request (app-id app-secret access-token access-secret
                                      &key since-id)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A~A" (random 1234567) stamp))
         (end-point  (format nil "https://api.twitter.com/1.1/direct_messages.json?count=200&since_id=~A&include_entities=true"                             
                             (if since-id
                                 since-id
                                 1)))
         (since (format nil "~A" (if since-id
                                since-id
                                1))))
    (drakma-request 
     end-point
     :method :get 

     :additional-headers
     (request-with-auth1 app-id app-secret 
                         access-token access-secret
                         nonce stamp
                         (signature-base-string
                          :uri end-point 
                          :request-method "GET"
                          :parameters `(
                                        ("count" "200")
                                        ("include_entities" "true")
                                        ("oauth_consumer_key" ,app-id)
                                        ("oauth_nonce" ,nonce)
                                        ("oauth_signature_method" "HMAC-SHA1")
                                        ("oauth_timestamp" ,stamp)
                                        ("oauth_token" ,access-token)
                                        ("oauth_version" "1.0")
                                        ("since_id"  ,since)
                                        )))
    
    :want-stream nil
    :preserve-uri nil)))

(defun twitter-direct-messages (user &key since-id)
  (when user
    (let* ((end-point  
            (format nil "https://api.twitter.com/1.1/direct_messages.json?count=200&since_id=~A&include_entities=true"                             
                    (if since-id
                        since-id
                        1))))
      (twitter-request
       user
       end-point
       :method :get
       :signature-parameters
       `(("count" "200")
         ("include_entities" "true")
         ("since_id"  ,(format nil "~A" (if since-id
                                          since-id
                                          1))))
       :handle-response-p nil))))






