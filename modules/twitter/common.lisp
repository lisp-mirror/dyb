(in-package :ems)

(setf drakma:*header-stream* *standard-output*)

(defconstant +unix-to-universal-time+ 2208988800)

(defun get-unix-time (&optional (ut (get-universal-time)))
  (- ut +unix-to-universal-time+))


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
          (url-encode (normalize-uri uri))
          (url-encode (build-query-thingy
                       (sort-parameters parameters)))))

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

(defun sort-parameters (parameters)
  "Sort PARAMETERS according to the OAuth spec."
  (assert (not (assoc "oauth_signature" parameters :test #'equal)))
  (sort (copy-list parameters) #'string<
        :key #'car))

(defun build-auth-string (parameters)
  (format nil "OAuth ~{~A=~S~^, ~}"
          (loop for (key value) in parameters
                collect (url-encode key)
                collect (url-encode value))))

(defun twitter-oauth (service-user-id)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A~A" 
			(if service-user-id
			    service-user-id
			    (random 1234567))
			stamp)))
    (drakma:http-request
     *twitter-oauth-uri*
     :method :post
     :additional-headers
     `(("Authorization"
        ,@(build-auth-string
           `(("oauth_callback" ,*twitter-callback-uri*)
             ("oauth_consumer_key" ,*twitter-client-id*)
             ("oauth_nonce" ,nonce)
             ("oauth_signature"
              ,(encode-signature
                (hmac-sha1
                 (signature-base-string
                  :uri *twitter-oauth-uri*
                  :request-method "POST"
                  :parameters `( ("oauth_callback" ,*twitter-callback-uri*)
                                ("oauth_consumer_key" ,*twitter-client-id*)
                                ("oauth_nonce" ,nonce)
                                ("oauth_signature_method" "HMAC-SHA1")
                                ("oauth_timestamp" ,stamp)
                                ;;("oauth_token" "")
                                ("oauth_version" "1.0")))
                 (hmac-key  *twitter-client-secret*
                            ""))
                nil))
             ("oauth_signature_method" "HMAC-SHA1")
             ("oauth_timestamp" ,stamp)
             ("oauth_version" "1.0")))))
     :redirect-methods '(:get :post :head))))

;;Test
;;(twitter-oauth 1)

;;;http://jaanus.com/post/1451098316/understanding-the-guts-of-twit
;;;https://dev.twitter.com/docs/auth/creating-signature

;;use the following to test signing
;;;http://quonos.nl/oauthTester/

;;What to look out for when getting 401 errors
;;;http://codingthis.com/programming/php/when-oauth-goes-wrong-debugging-signature-mismatch-issues-in-php/

(defun get-auth-pair (auth-reply)
       (let (
	     (eqlist (split-sequence:split-sequence #\= auth-reply))
	     )
       (if (string= "true" (first (last eqlist))) 
	   (list 
	    (first (split-sequence:split-sequence #\& (second eqlist))) 
	    (first (split-sequence:split-sequence #\& (third eqlist)))) 
	   ())))

;; then we do https://api.twitter.com/oauth/authorize?oauth_token=oauth_token what shows login tw dialog
;; screen_name for additional con(m)fort (if in DB)
;; (format nil "~A&~A=~A" *twitter-oauth-authorize-uri* "oauth_token" user-oauth)

(defun twitter-authorize-uri (oauth-token)
  (format nil "~A?~A=~A" *twitter-oauth-authorize-uri* "oauth_token" oauth-token))
;; administration/service-users.lisp save handler

(define-easy-handler (tw-callback :uri "/ems/twitcallback") ()
       (let ((error-msg "Authorize failed!"))
  (when (parameter "oauth_token")
    (let* 
      (
	 (user-object (get-service-user-by-auth-token (parameter "oauth_token")))
	 (old-user (copy user-object))
    (reply (twitter-oauth-access NIL (parameter "oauth_token") (parameter "oauth_verifier")))
        (access-triplet (get-auth-access-triplet reply)))
      ;;(break "~A" reply)
      (unless (search "error" (write-to-string reply))
	;;(break "nema error ~A" reply)
      (setf (get-val user-object 'last-access-token) (first access-triplet))
      (setf (get-val user-object 'last-token-secret) (second access-triplet))
      (persist user-object :old-object old-user)
      (setf error-msg ())
      (redirect "ems/inbox"))
      (when (search "error" (write-to-string reply))
	;;(break "ima error ~A" reply)
	 (setf error-msg reply))
    )
  )
  (when error-msg
    	 (render (make-widget 'page :name "twitter-callback-page")
		 :body (with-html-to-string ()
			 (str error-msg))))))
       
(defun twitter-oauth-access (service-user-id oauth-token oauth-verifier)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A~A" 
			(if service-user-id
			    service-user-id
			    (random 1234567))
			stamp)))
    (drakma:http-request
     *twitter-access-token-uri*
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
                  :uri *twitter-oauth-uri*
                  :request-method "POST"
                  :parameters `( ;;("oauth_callback" ,*twitter-callback-uri*)
                                ("oauth_consumer_key" ,*twitter-client-id*)
                                ("oauth_nonce" ,nonce)
                                ("oauth_signature_method" "HMAC-SHA1")
                                ("oauth_timestamp" ,stamp)
                                ("oauth_token" ,oauth-token)
				("oauth_verifier" ,oauth-verifier)
                                ("oauth_version" "1.0")))
                 (hmac-key  *twitter-client-secret*
                            ""))
                nil))
             ("oauth_signature_method" "HMAC-SHA1")
             ("oauth_timestamp" ,stamp)
	     ("oauth_token" ,oauth-token)
	     ("oauth_verifier" ,oauth-verifier)
             ("oauth_version" "1.0")))))
     :redirect-methods '(:get :post :head))))
     
(defun get-auth-access-triplet (access-reply)
   (let (
   (eqlist (split-sequence:split-sequence #\= access-reply))
   )
     (list 
      (first (split-sequence:split-sequence #\& (second eqlist))) 
      (first (split-sequence:split-sequence #\& (third eqlist)))
      (first (split-sequence:split-sequence #\& (fourth eqlist)))) 
   ))
   
(defun twitter-get-stream (access-token access-secret)
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

(defun twitter-get-stream-old (access-token access-secret)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A~A" (random 1234567) stamp))
         (end-point  "http://api.twitter.com/1.1/statuses/home_timeline.json"
          
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


(defun twitter-listener-old (service-user)
  (when service-user
    (when (get-val service-user 'last-access-token)
      (let ((result (twitter-get-stream-old (get-val service-user 'last-access-token) (get-val service-user 'last-token-secret))))
        (populate-generic-db-from-tweet (json::decode-json-from-string (flexi-streams:octets-to-string result)))))))

(defun fetch-twitter-users-old ()
  (dolist (user (coerce (service-users) 'list ))
        (when (and user (string-equal (get-val user 'doc-status) "Active"))
          ;;TODO: How to get error messages in for users without access tokens.
          (when (string-equal (get-val user 'service-user-type) "Twitter")
            (when (get-val user 'last-access-token)
              (twitter-listener-old user))))))

(defun twitter-listener (service-user)
  (when service-user
    (when (get-val service-user 'last-access-token)
      (let ((stream (twitter-get-stream (get-val service-user 'last-access-token) (get-val service-user 'last-token-secret))))
        (when stream
            (loop for i below 200
               for line = (read-line stream nil nil) 
               when (and line (> 1 0) (> (length line) 2))
               do (populate-generic-db-from-tweet (list (json::decode-json-from-string line))))
            (close stream)
            ;;(values)
            )))))


(defun start-twitter-listener (user)
  (bordeaux-threads:make-thread  
   (lambda ()
     (loop
        ;(sleep 600)
        (twitter-listener user)))))

(defun listen-twitter-users ()
  (dolist (user (coerce (service-users) 'list ))
        (when (and user (string-equal (get-val user 'doc-status) "Active"))
          ;;TODO: How to get error messages in for users without access tokens.
          (when (string-equal (get-val user 'service-user-type) "Twitter")
            (when (get-val user 'last-access-token)
              (start-twitter-listener user))))))

