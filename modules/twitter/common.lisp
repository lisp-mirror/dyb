(in-package :ems)

(defparameter *twitter-client-id* "H5wWh6azz3n0Go4hOu5kgg")
(defparameter *twitter-client-secret* "m7u7UoEyTPIg5p0Gl1EV73hkl139tu3GkZjMetzS7G8")
(defparameter *twitter-callback-uri* "http://local.dataxware.co.za";; "http://app.digyourbrand.co.za/ems/twcallback"
)
(defparameter *twitter-oauth-uri* "https://api.twitter.com/oauth/request_token")
(defparameter *twitter-access-token-uri* "https://graph.facebook.com/oauth/access_token")


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




(defun signature-base-string (&key uri
                                   (request-method "POST")
                                   parameters)
  (concatenate 'string (string-upcase (princ-to-string request-method))
                       "&" (url-encode
                             (normalize-uri uri))
                        "&" (url-encode (build-query-thingy parameters))))

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



(defun twitter-oauthxx (service-user)
  (let* ((stamp (format nil "~A" (get-unix-time))
           )
        (nonce (format nil "~A~A" (xid service-user) stamp))
         )
    
    (format nil "Authorization: ~A " (build-auth-string (list 
                                                
                                                         (cons "oauth_callback" (url-encode *twitter-callback-uri*))
                                                         (cons "oauth_consumer_key" *twitter-client-id*)
                                                         (cons "oauth_nonce" nonce)
                                                         (cons "oauth_signature" 
                                                               (encode-signature 
                                                                (hmac-sha1 
                                                                 (signature-base-string :uri *twitter-oauth-uri*
                                                                                        :request-method "POST"
                                                                                        :parameters (sort-parameters (list 
                                                                                                                      (cons "oauth_consumer_key" *twitter-client-id* )
                                                                                                                      (cons "oauth_nonce" nonce)
                                                                                                                      (cons "oauth_signature_method" "HMAC-SHA1")
                                                                                                                      (cons "oauth_timestamp" stamp)
                                        ;(cons "oauth_token" "")
                                                                                                                      (cons "oauth_version" "1.0")
                                                                                                                      )))
                                                                 (hmac-key  *twitter-client-secret* 
                                                                            "")) 
                                                                t))
                                                         (cons "oauth_signature_method" "HMAC-SHA1")
                                                         (cons "oauth_timestamp" stamp)
                                                         (cons "oauth_version" "1.0"))))))

(defun sort-parameters (parameters)
  "Sort PARAMETERS according to the OAuth spec. This is a destructive operation."
  (assert (not (assoc "oauth_signature" parameters :test #'equal)))
  (sort parameters #'string< :key (lambda (x)
                                    "Sort by key and value."
                                    (concatenate 'string (princ-to-string (car x))
                                                 (princ-to-string (cdr x))))))

(defun build-auth-string (parameters)
  (format nil "OAuth ~{~A=~S~^, ~}"
          (alexandria:flatten (mapcar
                                (lambda (x y) (list x y))
                                (mapcar (alexandria:compose #'url-encode #'car) parameters)
                                (mapcar (alexandria:compose #'url-encode #'cdr) parameters)))))



(defun twitter-oauth (service-user-id)
  (let* ((stamp (format nil "~A" (get-unix-time)))
        (nonce (format nil "~A~A" service-user-id stamp)))
    

    (drakma:http-request   
       *twitter-oauth-uri*
    
       :method :post
       :additional-headers  
       `(("Authorization" . ,(build-auth-string (list 
                                                 (cons "oauth_callback" (url-encode *twitter-callback-uri*))
                                                 (cons "oauth_consumer_key" *twitter-client-id*)
                                                 (cons "oauth_nonce" nonce)
                                                 (cons "oauth_signature" 
                                                       (encode-signature 
                                                        (hmac-sha1 
                                                         (signature-base-string :uri *twitter-oauth-uri*
                                                                                :request-method "POST"
                                                                                :parameters (sort-parameters (list 
                                                                                                              (cons "oauth_consumer_key" *twitter-client-id* )
                                                                                                              (cons "oauth_nonce" nonce)
                                                                                                              (cons "oauth_signature_method" "HMAC-SHA1")
                                                                                                              (cons "oauth_timestamp" stamp)
                                                                                                              ;;(cons "oauth_token" "")
                                                                                                              (cons "oauth_version" "1.0")
                                                                                                              )))
                                                         (hmac-key  *twitter-client-secret* 
                                                                    "")) 
                                                        t))
                                                 (cons "oauth_signature_method" "HMAC-SHA1")
                                                 (cons "oauth_timestamp" stamp)
                                                 (cons "oauth_version" "1.0"))))
         )
       :redirect-methods '(:get :post :head)
           
       )
    )
  )

;;Test
;;(twitter-oauth 1)

;;;http://jaanus.com/post/1451098316/understanding-the-guts-of-twit
;;;https://dev.twitter.com/docs/auth/creating-signature

;;use the following to test signing
;;;http://quonos.nl/oauthTester/

;;What to look out for when getting 401 errors
;;;http://codingthis.com/programming/php/when-oauth-goes-wrong-debugging-signature-mismatch-issues-in-php/