(in-package :ems)

(defun twitter-request-token ()
  (session-value 'twitter-request-token))

(define-easy-handler (callback :uri "/twitter/callback") ()
  (funcall
       (lambda (&rest args)
         (declare (ignore args))
      
         (handler-case
             (cl-oauth:authorize-request-token-from-request 
              (lambda (rt-key)
                (assert (twitter-request-token))
              
                (unless (equal (url-encode rt-key) 
                               (cl-oauth:token-key (twitter-request-token)))
                  (warn "Keys differ: ~S / ~S~%" (url-encode rt-key) 
                        (cl-oauth:token-key (twitter-request-token))))

                (twitter-request-token)))
           (error (c)
             (warn "Couldn't verify request token authorization: ~A" c)))
         (when (cl-oauth:request-token-authorized-p (twitter-request-token))
           ;;TODO: use the *request* to get the id to get a service?????
           (let ((access-token (get-access-token 
                                (current-client)
                                "Test App"
                                (twitter-request-token))))
             
             (when access-token
               (let* ((id (cdr (first (slot-value access-token 
                                                  'cl-oauth::user-data))))
                      (doc (get-twitter-user-by-id (parse-integer id))))
                 (setf (get-val doc 'access-token) access-token)
                 (persist doc)
                 (redirect "/dyb/preview")))))))
  "Could not get access token.")


(defun get-entity (consumer-token access-token)
  (json::decode-json-from-string
   (babel:octets-to-string
    (cl-oauth::access-protected-resource  
     "https://api.twitter.com/1/users/lookup.json?screen_name=haragx&include_entities=true"
     access-token :consumer-token consumer-token))))



(define-easy-handler (ems-authx :uri "/dyb/twitauth") ()
 
  (let* ((twitter-service *app*;(get-twitter-service (current-client) "Test App")
           )
         (request-token (cl-oauth:obtain-request-token  
                         (request-token-endpoint twitter-service)
                         (consumer-token twitter-service) 
                         :callback-uri (callback-uri twitter-service))) 
         (uri (if request-token (cl-oauth:make-authorization-uri 
                                 "https://twitter.com/oauth/authorize"
                                  
                                 request-token))))
;(break "~a ~% ~A" request-token (request-token-endpoint twitter-service) )
    (setf (session-value 'twitter-request-token) request-token)
    
    (if uri
        (redirect (format nil "~A" (puri:uri uri)))
        (with-html-output-to-string (*standard-output*)
          (str "Could not get request token check logs.")
          (:br)))))

(define-easy-handler (emsx :uri "/dyb/twit") ()
  (with-html-output-to-string (*standard-output*)
    ;;TODO:select a user first and then get the token
    (let ((service (get-twitter-service (current-client) "DX"))
          (user (get-twitter-user (current-client) "haragx@gmail.com")))
      (str (if (and service user)
               (get-entity (consumer-token service) (access-token user)))))))
