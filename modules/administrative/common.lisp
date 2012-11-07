(in-package :dyb)

(defvar +utf-8+ (flexi-streams:make-external-format :utf8 :eol-style :lf))

(defmethod normalize-uri ((uri string))
  (normalize-uri (puri:parse-uri uri)))

(defmethod normalize-uri ((uri puri:uri))
  "9.1.2"
  (let ((*print-case* :downcase) ; verify that this works!!
        (scheme (puri:uri-scheme uri))
        (host (puri:uri-host uri))
        (port (puri:uri-port uri))
        (path (puri:uri-path uri)))
    (values
      (concatenate 'string
        (string-downcase (princ-to-string scheme))
        "://"
        (string-downcase host)
        (cond
          ((null port)
           "")
          ((and (eq scheme :http) (eql port 80))
           "")
          ((and (eq scheme :https) (eql port 443))
           "")
          (t
           (concatenate 'string ":" (princ-to-string port))))
        path)
      )))

(defun url-encode (string &optional (external-format +utf-8+))
  "URL-encodes a string using the external format EXTERNAL-FORMAT."
  (with-output-to-string (s)
    (loop for c across string
          for index from 0
          do (cond ((or (char<= #\0 c #\9)
                        (char<= #\a c #\z)
                        (char<= #\A c #\Z)
                        (find c "-_.~" :test #'char=))
                     (write-char c s))
                   (t (loop for octet across (flexi-streams:string-to-octets string
                                                                             :start index
                                                                             :end (1+ index)
                                                                             :external-format external-format)
                            do (format s "%~2,'0x" octet)))))))

(defun build-query-thingy (parameters)
  (with-output-to-string (stream)
    (loop for separator = "" then "&"
          for (key value) in parameters
          do
          (format stream "~a~a=~a"
                  separator
                  (url-encode key)
                  (url-encode value)))))

(defun signature-base-string (&key uri
                                   (request-method "POST")
                                   parameters)
  (format nil "~:@(~a~)&~a&~a"
          request-method
          (url-encode (normalize-uri uri));;(url-encode uri);;(url-encode (normalize-uri uri))
          (url-encode (build-query-thingy
                       parameters))))

(defun hmac-key (consumer-secret &optional token-secret)
  "9.2"
  (concatenate 'string (url-encode consumer-secret) "&" (url-encode (or token-secret ""))))

(defun hmac-sha1 (s key)
  (let* ((s (babel:string-to-octets s))
         (key (babel:string-to-octets key))
         (hmac (ironclad:make-hmac key 'ironclad:sha1)))
    (ironclad:update-hmac hmac s)
    (ironclad:hmac-digest hmac)))

(defun encode-signature (octets url-encode-p)
  "9.2.1"
  (let ((base64 (cl-base64:usb8-array-to-base64-string octets)))
    (if url-encode-p
      (url-encode base64)
      base64)))


(defun build-auth-string (parameters)
  (format nil "OAuth ~{~A=~S~^, ~}"
          (loop for (key value) in parameters
                collect (url-encode key)
                collect (url-encode value))))


(defun oauth-request-token-url (channel end-point-type values)
  (let* ((end-point (get-end-point channel end-point-type))
         (url (get-val end-point 'uri))
         (count 0))
    (dolist (parameter (get-val end-point 'parameters))
             (let ((key (get-val parameter 'parameter-name))
                   (value (cond ((stringp (get-val parameter 'default-value))
                                 (get-val parameter 'default-value))
                                ((equal (get-val parameter 'default-value) 'app-id)
                                 (get-val channel (get-val parameter 'default-value)))
                                (t
                                 (if values
                                     (cdr (assoc (get-val parameter 'default-value) values)))))))

               (setf url (format nil "~A~A~A=~A" url 
                                 (if (= count 0) 
                                     "?"
                                     "&") 
                                 key (if value (url-encode value)))))
             (incf count))
    url))


(defun oauth2-access-token-uri (channel request-token)
  (let* ((end-point (get-end-point channel "Access Token"))
        (callback-uri (get-end-point-parameter end-point "redirect_uri")))
    
    (format nil "~A?client_id=~A&redirect_uri=~A&client_secret=~A&code=~A" 
            (get-val end-point 'uri)
            (get-val channel 'app-id)
            (if callback-uri
                (url-encode (get-val callback-uri 'default-value))) 
            (get-val channel 'app-secret)
            request-token)))


(defun oauth1-request (channel)
  (let* ((end-point (get-end-point channel "Request App Authentication"))
         (callback (if end-point                       
                       (get-end-point-parameter-value end-point "oauth_callback")) )
         (stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A" 
			(random 999999999)
			))
         (scope (get-end-point-parameter-value end-point "scope")))

    ;;TODO: Some how split out the scope shit for linked in
    (drakma:http-request
     (if scope
         (format nil "~A?scope=~A" (get-val end-point 'uri)
                 (replace-all scope " " "+" ))
         (get-val end-point 'uri))
     :method :post
     :additional-headers
     `(("Authorization"
        ,@(build-auth-string
           `(("oauth_callback" ,callback)
             ("oauth_consumer_key" ,(get-val channel 'app-id))
             ("oauth_nonce" ,nonce)
             ("oauth_signature"
              ,(encode-signature
                (hmac-sha1
                 (signature-base-string
                  :uri (format nil "~A?scope=~A" (get-val end-point 'uri)
                               (replace-all scope " " "+" ))
                  :request-method "POST"
                  :parameters `(("oauth_callback" ,callback)
                                ("oauth_consumer_key" ,(get-val channel 'app-id))
                                ("oauth_nonce" ,nonce)
                                
                                ("oauth_signature_method" "HMAC-SHA1")
                                ("oauth_timestamp" ,stamp)
                                ("oauth_version" "1.0")
                                ,(if scope
                                    `("scope" ,scope)
                                    "")))
                 (hmac-key  (get-val channel 'app-secret)
                            ""))
                nil))
             ("oauth_signature_method" "HMAC-SHA1")
             ("oauth_timestamp" ,stamp)
             ("oauth_version" "1.0")))))
     :redirect-methods '(:get :post :head)
     :preserve-uri t)))

(defun oauth1-access (channel oauth-token oauth-verifier &key (request-secret ""))
  (let* ((end-point (get-end-point channel "Access Token")) 
         (stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A" 
			(random 999999999)
			)))
    (drakma:http-request
     (get-val end-point 'uri)
     :method :post
     :additional-headers
     `(("Authorization"
        ,@(build-auth-string
           `(("oauth_consumer_key" ,(get-val channel 'app-id))
             ("oauth_nonce" ,nonce)
             ("oauth_signature"
              ,(encode-signature
                (hmac-sha1
                 (signature-base-string
                  :uri (get-val end-point 'uri)
                  :request-method "POST"
                  :parameters `(("oauth_consumer_key" ,(get-val channel 'app-id))
                                ("oauth_nonce" ,nonce)
                                ("oauth_signature_method" "HMAC-SHA1")
                                ("oauth_timestamp" ,stamp)
                                ("oauth_token" ,oauth-token)
				("oauth_verifier" ,oauth-verifier)
                                ("oauth_version" "1.0")))
                 (hmac-key  (get-val channel 'app-secret)
                            request-secret))
                nil))
             ("oauth_signature_method" "HMAC-SHA1")
             ("oauth_timestamp" ,stamp)
	     ("oauth_token" ,oauth-token)
	     ("oauth_verifier" ,oauth-verifier)
             ("oauth_version" "1.0")))))
     :redirect-methods '(:get :post :head))))


(defun end-point-url (channel end-point-type values)
  (let* ((end-point (get-end-point channel end-point-type))
         (url (get-val end-point 'uri))
         (count 0))
    (dolist (parameter (get-val end-point 'parameters))
             (let ((key (get-val parameter 'parameter-name))
                   (value (cond ((stringp (get-val parameter 'default-value))
                                 (get-val parameter 'default-value))                               
                                (t
                                 
                                 (if values
                                     (cdr (assoc (get-val parameter 'default-value) values)))))))
               
               (setf url (format nil "~A~A~A~A~A" url 
                                 (if (get-val parameter 'part-of-url-p)
                                     "/"
                                     (if (= count 0) 
                                         "?"
                                         "&")) 
                                 (if (get-val parameter 'part-of-url-p) 
                                     ""
                                     key) 
                                 (if (get-val parameter 'part-of-url-p)
                                     ""
                                     "=")
                                 (if value 
                                     (url-encode value)))))
             (incf count))
    url))