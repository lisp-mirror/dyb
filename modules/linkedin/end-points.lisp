(in-package :dyb)

#|
(babel:octets-to-string 
      (linkedin-network-updates 
       "ccp3psrastub" "4VdvtsVhOruXZFmo"
       "c86c3a40-15a1-4c3e-a0ea-a0bba3fc42a3"
       "1d127746-f3f2-4d99-b9f9-7b98945f2826"))

|#

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