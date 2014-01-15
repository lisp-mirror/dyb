(in-package :dyb)


(defun ensure-string-reply (reply)
  (etypecase reply
    (string reply)
    ((vector (unsigned-byte 8)) (babel:octets-to-string reply))
    (null nil)))

(defclass drakma-request-result ()
  ((body-or-stream :initarg :body-or-stream
                   :accessor body-or-stream)
    (status-code :initarg :status-code
                 :accessor status-code)
    (headers :initarg :headers
             :accessor headers)
    (uri :initarg :uri
         :accessor uri)
    (stream :initarg :stream
            :accessor result-stream)
    (must-close :initarg :must-close
                :accessor must-close)
    (reason-phrase :initarg :reason-phrase
                   :accessor result-reason-phrase)))

(defun drakma-request (uri &rest args
                       &key method
                            parameters
                            url-encoder
                            content content-type content-length
                            additional-headers
                            want-stream
                            preserve-uri
                            connection-timeout
                            body-to-string-p
                            parse-json-p)
  (declare (ignore method parameters url-encoder content content-type
                   content-length additional-headers preserve-uri
                   connection-timeout))
  (multiple-value-bind (body-or-stream status-code headers
                        uri stream must-close reason-phrase)
      (apply #'drakma:http-request uri :allow-other-keys t args)
    (make-instance 'drakma-request-result
                   :body-or-stream
                   (cond ((or want-stream
                              (not body-to-string-p)
                              (null body-or-stream))
                          body-or-stream)
                         (parse-json-p
                          (json:decode-json-from-string
                           (ensure-string-reply body-or-stream)))
                         (t
                          (ensure-string-reply body-or-stream)))
                   :status-code status-code
                   :headers headers
                   :uri uri
                   :stream stream
                   :must-close must-close
                   :reason-phrase reason-phrase)))

(defun request-with-auth1 (app-id app-secret 
                           access-token access-secret 
                           nonce stamp
                           signature-base-string)
  `(("Authorization"
        ,@(build-auth-string
           `(("oauth_consumer_key" ,app-id)
             ("oauth_nonce" ,nonce)
             ("oauth_signature"
              ,(encode-signature
                (hmac-sha1
                 signature-base-string
                 (hmac-key app-secret 
                           access-secret))
                  nil))
             ("oauth_signature_method" "HMAC-SHA1")
             ("oauth_timestamp" ,stamp)
             ("oauth_token" ,access-token)
             ("oauth_version" "1.0"))))))


(defun sort-parameters (parameters)
  "Sort PARAMETERS according to the OAuth spec. This is a destructive operation."
  
  (sort parameters #'string< :key (lambda (x)
                                    "Sort by key and value."
                                    (concatenate 'string (princ-to-string (car x))
                                                 (princ-to-string (cdr x))))))



(defun oauth1-drakma-request (end-point app-id app-secret 
                              &key  access-token access-secret  
                              callback-uri oauth-verifier 
                              method parameters 
                              content
                              content-type
                              signature-parameters
                              want-stream preserve-uri
                              body-to-string-p
                              parse-json-p)
  (let* ((stamp (format nil "~A" (get-unix-time)))
         (nonce (format nil "~A~A" (random 1234567) stamp))) 

    (let ((final-params (sort-parameters
                         (append  `(,@(if callback-uri
                                          `(("oauth_callback" ,callback-uri)))
                                    ("oauth_consumer_key" ,app-id)
                                    ("oauth_nonce" ,nonce)
                                    ("oauth_signature_method" "HMAC-SHA1")
                                    ("oauth_timestamp" ,stamp)
                                    ,@(if access-token
                                          `(("oauth_token" ,access-token)))
                                    ,@(if oauth-verifier
                                          `(("oauth_verifier" ,oauth-verifier)))
                                    ("oauth_version" "1.0"))
                                  
                                  signature-parameters))))

      
      (drakma-request
       end-point
       :method method 
       :parameters parameters  
       :content content
       :content-type content-type
       :additional-headers
       `(("Authorization"
          ,@(build-auth-string
             `(,@(if callback-uri
                     `(("oauth_callback" ,callback-uri)))
                 ("oauth_consumer_key" ,app-id)
                 ("oauth_nonce" ,nonce)
                 ("oauth_signature"
                  ,(encode-signature
                    (hmac-sha1
                     (signature-base-string
                      :uri end-point 
                      :request-method method
                      :parameters final-params)
                     (hmac-key  app-secret
                                (if access-secret
                                    access-secret
                                    "")))
                    nil))
                 ("oauth_signature_method" "HMAC-SHA1")
                 ("oauth_timestamp" ,stamp)
                 ,@(if access-token
                       `(("oauth_token" ,access-token)))
                 ,@(if oauth-verifier
                       `(("oauth_verifier" ,oauth-verifier)))
                      
                 ("oauth_version" "1.0"))
             )))
       :want-stream want-stream
       :preserve-uri preserve-uri
       :body-to-string-p body-to-string-p
       :parse-json-p parse-json-p))))

(defun handle-endpoint (user request-result &key error-path)
  (let ((result)
        (message))  
    (cond ((not (get-val user 'last-access-token))
           (setf message "Missing access token"))
          ((not request-result)
           (setf message "Endpoint returned no values."))
          ;;TODO: Add more http status codes to check.
          ((typep request-result 'drakma-request-result)
           (if (listp (body-or-stream request-result))
               (setf result (body-or-stream request-result))
               (setf result (json:decode-json-from-string
                             (ensure-string-reply (body-or-stream request-result)))))
           (unless (equal (status-code request-result) 200)
             ;;TODO: Add a parameter to say which api is handling gave the request 
             ;;instead of error path so that the error can be handled better.
             (when (and (consp result)
                        (or (assoc-path result error-path) 
                            (assoc-path result :error) 
                            (assoc-path result :errors)))
               (let ((error-message (or (assoc-path result error-path)
                                        (assoc-path result :error :message)
                                        (if (listp (cdr (assoc-path result :errors)))
                                            (assoc-path 
                                             (car (cdr (assoc-path result :errors)))
                                             :message)
                                            (assoc-path result :errors)))))
 
                 (setf message (if (listp error-message)
                                   (cdr error-message)
                                   error-message))))))
          (t
           (if (listp request-result)
               (setf result request-result)
               (setf result (json:decode-json-from-string
                             (ensure-string-reply request-result))))
           (when (and (consp result)
                      (or (assoc-path result error-path) 
                          (assoc-path result :error) 
                          (assoc-path result :errors)))
             (let ((error-message (or (assoc-path result error-path)
                                      (assoc-path result :error :message)
                                      (if (listp (cdr (assoc-path result :errors)))
                                          (assoc-path (car (cdr (assoc-path result :errors))) :message)
                                          (assoc-path result :errors)))))
               (setf message (if (listp error-message)
                                 (cdr error-message)
                                 error-message))))))
  
    (values result message (if (typep request-result 'drakma-request-result)
                               (status-code request-result)))))

(defun handle-endpoint-run-request (user request-function &key error-path)
  (let ((result)
        (message))
    (if (get-val user 'last-access-token)
        (multiple-value-bind (body status header uri stream must-close reason-phrase)
            (funcall request-function)
          (declare (ignore header uri stream must-close))
          (cond (body
                 (setf result (json:decode-json-from-string
                               (ensure-string-reply body)))
                 (when (or (assoc-path result error-path) 
                           (assoc-path result :error) 
                           (assoc-path result :errors))
                   (setf message (cdr (or (assoc-path result error-path)
                                          (assoc-path result :error :message)
                                          (assoc-path result :errors :message))))))
                ((or (eql status 200) (eql status 201))
                 (setf result reason-phrase))
                (t
                 (setf message "Endpoint returned no values."))))
        (setf message "Missing access token"))
    (values result message)))
