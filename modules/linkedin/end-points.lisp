(in-package :dyb)

(defun make-share (comment content-title
                   description submitted-url
                   submitted-image-url)


  (cond ( (and submitted-url submitted-image-url)
         (json:encode-json-to-string
          `(
            ("comment" . ,(if comment
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
           
           ("visibility" . (("code" . "anyone")))))))

  )


(defun linkedin-share (app-id app-secret access-token 
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
     `(("x-li-format" "json")
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
             ("oauth_version" "1.0")))))
    :want-stream nil
    :preserve-uri nil)))

(defun linkedin-like-xml ()
  "<?xml version='1.0' encoding='UTF-8'?>
<is-liked>true</is-liked>")

(defun linkedin-like (app-id app-secret access-token access-secret 
                         update-id)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A" (random 1234567)))
         (end-point  
          (format nil 
                  "http://api.linkedin.com/v1/people/~~/network/updates/key=~A/is-liked"
                  update-id) ))
    
    (drakma:http-request end-point
      
     :method :get    
     :content (linkedin-like-xml )
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
             ("oauth_version" "1.0")))))
    :want-stream nil
    :preserve-uri nil)))


(defun linkedin-comment-xml (comment)
  (format nil "<?xml version='1.0' encoding='UTF-8'?>
<update-comment>
  <comment>~A</comment>
</update-comment> "
          comment))

(defun linkedin-comment (app-id app-secret access-token access-secret 
                         update-id
                         comment)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A" (random 1234567)))
         (end-point  
          (format nil 
                  "http://api.linkedin.com/v1/people/~~/network/updates/key=~A/update-comments"
                  update-id) ))
    
    (drakma:http-request end-point
      
     :method :get    
     :content (linkedin-comment-xml comment)
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
             ("oauth_version" "1.0")))))
    :want-stream nil
    :preserve-uri nil)))


(defun linkedin-network-updates (app-id app-secret access-token access-secret)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A" (random 1234567)))
         (end-point  "http://api.linkedin.com/v1/people/~/network/updates?type=CMPY&type=PICT&type=SHAR&count=250&format=json" ))
    
    (drakma:http-request end-point
      
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
                  :parameters `(("count" "250")
                                ("format" "json")
                                ("oauth_consumer_key" ,app-id)
                                ("oauth_nonce" ,nonce)
                                ("oauth_signature_method" "HMAC-SHA1")
                                ("oauth_timestamp" ,stamp)
                                ("oauth_token" ,access-token)
                                ("oauth_version" "1.0")
                                ;;("start" "250")
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
    :preserve-uri nil)))