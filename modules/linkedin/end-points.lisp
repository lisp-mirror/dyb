(in-package :dyb)

(defun make-share (comment content-title
                   description submitted-url
                   submitted-image-url)
  (cond ( (and submitted-url submitted-image-url)
         (json:encode-json-to-string
          `(("comment" . ,(if comment
                              comment
                              ""))
            ("content" . (("title" . ,(if content-title
                                          content-title
                                          ""))
                          ("description" . ,(if description
                                                description
                                                ""))
                          ("submitted-url" . ,submitted-url )
                          ("submitted-image-url" . ,submitted-image-url)))
            ("visibility" . (("code" . "anyone"))))))
        (t 
         (json:encode-json-to-string
         `(
           ("comment" . ,(if comment
                             comment
                             ""))
           ("visibility" . (("code" . "anyone"))))))))

(defun linkedin-share-request (app-id app-secret access-token 
                       access-secret comment 
                       &key content-title
                       description submited-url
                       submitted-image-url)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A" (random 1234567)))
         (end-point  "http://api.linkedin.com/v1/people/~/shares" )
         (share (make-share comment content-title
                            description submited-url
                            submitted-image-url)))
   
    (drakma:http-request 
     end-point  
     :content-type "application/json"
     :content share
     :method :post    
     :additional-headers
     `(("x-li-format" . "json")
       ("Authorization"
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
    :preserve-uri nil)))

(defun linkedin-share (user message  &key content-title
                       description submited-url
                       submitted-image-url)
  (let ((channel (if user (get-social-channel (get-val user 'channel-user-type)))))
    (handle-endpoint-run-request
         user
         `(linkedin-share-request 
          ,(get-val channel 'app-id)
          ,(get-val channel 'app-secret)
          ,(get-val user 'last-access-token)
          ,(get-val user 'last-token-secret)
          ,message
          :content-title ,content-title
          :description ,description
          :submited-url ,submited-url
          :submitted-image-url ,submitted-image-url)
         :result-is-octets-p t)))

(defun post-linkedin (user-id message)
  (let ((user (get-channel-user-by-user-id user-id)))
    (linkedin-share
     user
     message)))

(defun linkedin-like-xml ()
  "<?xml version='1.0' encoding='UTF-8'?>
<is-liked>true</is-liked>")

(defun linkedin-like-request (app-id app-secret access-token access-secret 
                         update-id)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A" (random 1234567)))
         (end-point  
          (format nil 
                  "http://api.linkedin.com/v1/people/~~/network/updates/key=~A/is-liked"
                  update-id) ))
    
    (drakma:http-request 
     end-point
     :method :get    
     :content (linkedin-like-xml )
     :additional-headers
     `(;;("x-li-format" . "json")
       ("Authorization"
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
    :want-stream nil
    :preserve-uri nil)))

(defun like-linkedin (user linkedin-update-id)
  (when (and user linkedin-update-id)
    (let ((channel (get-social-channel (get-val user 'channel-user-type))))
      (handle-endpoint
       user
       (linkedin-like-request
        (get-val channel 'app-id)
        (get-val channel 'app-secret)
        (get-val user 'last-access-token)
        (get-val user 'last-token-secret)
        linkedin-update-id)
       :result-is-octets-p t))))

(defun linkedin-comment-xml (comment)
  (format nil "<?xml version='1.0' encoding='UTF-8'?>
<update-comment>
  <comment>~A</comment>
</update-comment> "
          comment))

(defun linkedin-comment-request (app-id app-secret access-token access-secret 
                         update-id
                         comment)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A" (random 1234567)))
         (end-point  
          (format nil 
                  "http://api.linkedin.com/v1/people/~~/network/updates/key=~A/update-comments"
                  update-id) ))
    
    (drakma:http-request 
     end-point
     :method :get    
     :content (linkedin-comment-xml comment)
     :additional-headers
     `(("x-li-format" . "json")
       ("Authorization"
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
    :want-stream nil
    :preserve-uri nil)))

(defun comment-linkein (user update-id comment)
  (when (and user update-id comment)
    (let ((channel (get-social-channel (get-val user 'channel-user-type))))
      (handle-endpoint
       user
       (linkedin-comment
                 (get-val channel 'app-id)
                 (get-val channel 'app-secret)
                 (get-val user 'last-access-token)
                 (get-val user 'last-token-secret) 
                 update-id 
                 comment)
       :result-is-octets-p t))))


(defun linkedin-network-updates-request (app-id app-secret access-token access-secret)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A" (random 1234567)))
         (end-point  "http://api.linkedin.com/v1/people/~/network/updates?type=CMPY&type=PICT&type=SHAR&count=250" ))
    
    (drakma:http-request 
     end-point
     :content-type "application/json"
     :method :get    
     :additional-headers
     `(("x-li-format" . "json")
       ("Authorization"
        ,@(build-auth-string
           `(("oauth_consumer_key" ,app-id)
             ("oauth_nonce" ,nonce)
             ("oauth_signature"
              ,(encode-signature
                (hmac-sha1
                 (signature-base-string
                  :uri end-point 
                  :request-method "GET"
                  :parameters `(("count" "250")
                                ("oauth_consumer_key" ,app-id)
                                ("oauth_nonce" ,nonce)
                                ("oauth_signature_method" "HMAC-SHA1")
                                ("oauth_timestamp" ,stamp)
                                ("oauth_token" ,access-token)
                                ("oauth_version" "1.0")
                                ("type" "CMPY")
                                ("type" "PICT")
                                ("type" "SHAR")))
                 (hmac-key  app-secret 
                            access-secret))
                nil))
             ("oauth_signature_method" "HMAC-SHA1")
             ("oauth_timestamp" ,stamp)
             ("oauth_token" ,access-token)
             ("oauth_version" "1.0")))))
    :want-stream nil
    :preserve-uri t)))

(defun linkedin-network-updates (user)
  (when user
    (let ((channel (get-social-channel (get-val user 'channel-user-type))))
      (handle-endpoint
       user
       (linkedin-network-updates-request
                 (get-val channel 'app-id)
                 (get-val channel 'app-secret)
                 (get-val user 'last-access-token)
                 (get-val user 'last-token-secret))
       :result-is-octets-p t))))

(defun linkedin-connections-request (app-id app-secret
                             access-token 
                             access-secret)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A" (random 1234567)))
         (end-point  "http://api.linkedin.com/v1/people/~/connections"))
        
    (drakma:http-request 
     end-point  
     :content-type "application/json"
     :method :get    
     :additional-headers
     `(("x-li-format" . "json")
       ("Authorization"
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
                                ("oauth_version" "1.0") ))
                 (hmac-key  app-secret 
                            access-secret))
                nil))
             ("oauth_signature_method" "HMAC-SHA1")
             ("oauth_timestamp" ,stamp)
             ("oauth_token" ,access-token)
             ("oauth_version" "1.0")))))
    :want-stream nil
    :preserve-uri t)))

(defun linkedin-connections (user)
  (when user
    (let ((channel (get-social-channel (get-val user 'channel-user-type))))
      (handle-endpoint
       user
       (linkedin-connections-request
                 (get-val channel 'app-id)
                 (get-val channel 'app-secret)
                 (get-val user 'last-access-token)
                 (get-val user 'last-token-secret))
       :result-is-octets-p t))))

(defun linkedin-company-updates-request (app-id app-secret
                                  access-token 
                                  access-secret
                                  company-id)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A" (random 1234567)))
         (end-point  (format nil "http://api.linkedin.com/v1/companies/~A/updates" company-id)))
    
    (drakma:http-request 
     end-point  
     :content-type "application/json"
     :method :get    
     :additional-headers
     `(("x-li-format" . "json")
       ("Authorization"
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
    :want-stream nil
    :preserve-uri t)))

(defun linkedin-company-updates (user)
  (when user
    (let ((channel (get-social-channel (get-val user 'channel-user-type))))
      (handle-endpoint
       user
       (linkedin-company-updates-request
                 (get-val channel 'app-id)
                 (get-val channel 'app-secret)
                 (get-val user 'last-access-token)
                 (get-val user 'last-token-secret)
                 (get-val user 'user-id))
       :result-is-octets-p t))))

(defun linkedin-profile-request (app-id app-secret
                         access-token 
                         access-secret)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A" (random 1234567)))
         (end-point  "http://api.linkedin.com/v1/people/~:(id,first-name,last-name,headline,picture-url,public-profile-url)" ))
      
    (drakma:http-request 
     end-point  
     :content-type "application/json"
     :method :get    
     :additional-headers
     `(("x-li-format" . "json")
       ("Authorization"
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
    :want-stream nil
    :preserve-uri t)))

(defun linkedin-profile (user)
  (when user
    (let ((channel (get-social-channel (get-val user 'channel-user-type))))
      (handle-endpoint
       user
       (linkedin-profile-request
                 (get-val channel 'app-id)
                 (get-val channel 'app-secret)
                 (get-val user 'last-access-token)
                 (get-val user 'last-token-secret))
       :result-is-octets-p t))))


(defun linkedin-companies-request (app-id app-secret
                           access-token 
                           access-secret
                           company-user-name)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A" (random 1234567)))
         (end-point  (format nil "http://api.linkedin.com/v1/companies/universal-name=~A:(id,name,universal-name,email-domains,company-type,website-url,logo-url,twitter-id,locations,description,num-followers)" company-user-name)))
    
    (drakma:http-request 
     end-point  
     :content-type "application/json"
     :method :get    
     :additional-headers
     `(("x-li-format" . "json")
       ("Authorization"
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
    :want-stream nil
    :preserve-uri t)))

(defun linkedin-companies (user company-user-name)
  (when user
    (let ((channel (get-social-channel (get-val user 'channel-user-type))))
      (handle-endpoint
       user
       (linkedin-companies-request
                 (get-val channel 'app-id)
                 (get-val channel 'app-secret)
                 (get-val user 'last-access-token)
                 (get-val user 'last-token-secret)
                 company-user-name)
       :result-is-octets-p t))))

(defun linkedin-company-by-id-request (app-id app-secret
                           access-token 
                           access-secret
                           company-id)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A" (random 1234567)))
         (end-point  (format nil "http://api.linkedin.com/v1/companies/~A:(id,name,universal-name,email-domains,company-type,website-url,logo-url,twitter-id,locations,description,num-followers)" company-id)))
    
    (drakma:http-request 
     end-point  
     :content-type "application/json"
     :method :get    
     :additional-headers
     `(("x-li-format" . "json")
       ("Authorization"
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
    :want-stream nil
    :preserve-uri t)))

(defun linkedin-company-by-id (user company-id)
  (when user
    (let ((channel (get-social-channel (get-val user 'channel-user-type))))
      (handle-endpoint
       user
       (linkedin-company-by-id-request
                 (get-val channel 'app-id)
                 (get-val channel 'app-secret)
                 (get-val user 'last-access-token)
                 (get-val user 'last-token-secret)
                 company-id)
       :result-is-octets-p t))))

(defun linkedin-user-companies-request (app-id app-secret
                           access-token 
                           access-secret)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A" (random 1234567)))
         (end-point  "http://api.linkedin.com/v1/people/~:(positions:(company:(name,id,universal-name)))"))
    
    (drakma:http-request 
     end-point  
     :content-type "application/json"
     :method :get    
     :additional-headers
     `(("x-li-format" . "json")
       ("Authorization"
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
    :want-stream nil
    :preserve-uri t)))

(defun linkedin-user-companies (user )
  (when user
    (let ((channel (get-social-channel (get-val user 'channel-user-type))))
      (handle-endpoint
       user
       (linkedin-user-companies-request
                 (get-val channel 'app-id)
                 (get-val channel 'app-secret)
                 (get-val user 'last-access-token)
                 (get-val user 'last-token-secret))
       :result-is-octets-p t))))


 