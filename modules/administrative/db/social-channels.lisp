(in-package :dyb)

(defclass api-credentials ()
  ((api-identity 
     :initarg :api-identity
     :accessor api-identity)
   (secret 
    :initarg :secret
    :accessor secret
    :documentation "At token or password that is used to authenticate the identity.")))

(defclass rest-api ()
  ((auth-type :initarg :auth-type
                :initform nil
                :documentation "OAuth1, OAuth2, Login, Plain Token, None")
   (credentials :initarg :credentials
                :accessor credentials
                :documentation "Contains details needed to authenticate.")))



(defclass uri-parameter ()
  ((parameter-name :initarg :parameter-name)
   (url-encode-p :initarg :url-encode-p)
   (default-value :initarg :default-value)
   (system-parameter :initarg :system-parameter)
   (part-of-url-p :initarg :part-of-url-p))
  (:metaclass storable-class))

(defclass end-point ()
  ((end-point-type :initarg :end-point-type 
                   :documentation "Request App Authentication, Request Token, Access Token, Query")
   (call-type :initarg :call-type 
                   :documentation "GET,POST")
   (uri :initarg :uri)
   (parameters :initarg :parameters)
   (return-parameters :initarg :return-parameters)
   
   (return-type :initarg :return-type
                :documentation "JSON, XML, Query String, Callback"))
  (:metaclass storable-class))

(defclass social-channel (doc)
  ((channel-name :initarg :channel-name 
                :initform nil
                :accessor channel-name)
   (auth-type :initarg :auth-type
                :initform nil
                :documentation "OAuth1, OAuth2, Login, Plain Token, None")
   (app-id :initarg :app-id)
   (app-secret :initarg :app-secret)
   (end-points :initarg :end-points
               :initform nil))
  (:metaclass storable-class))

(defun social-channels-collection ()
  (get-collection (system-db) "social-channels"))

(defmethod doc-collection ((doc social-channel))
  (social-channels-collection))

(defun social-channels ()
  (docs (social-channels-collection)))

(defun make-social-channel (channel-name auth-type
                            &key app-secret app-id end-points)
  (make-instance 'social-channel :key channel-name 
                 :channel-name channel-name
                 :auth-type auth-type
                 :app-secret app-secret 
                 :app-id app-id
                 :end-points end-points))

(defun get-social-channel (channel-name)
  (get-doc (social-channels-collection) 
            channel-name :test #'string-equal))

(defun get-social-channel-by-id (id)
  (get-doc (social-channels-collection) id
                       :element 'xdb2::id))


(add-collection (system-db) "social-channels" 
                :collection-class 'dyb-collection
                :load-from-file-p nil)

(defun get-channels-list ()
  (loop for channel across (social-channels)
       collect (list (get-val channel 'channel-name)
                     (get-val channel 'channel-name))))

(defclass channel-request ()
  ((channel :initarg :channel)
   (request-parameters :initarg :request-parameters)
   (channel-user :initarg :channel-user)))

(defun get-end-point (channel type)
  (dolist (end-point (get-val channel 'end-points))
    (if (string-equal (get-val end-point 'end-point-type) type)
        (return-from get-end-point end-point))))

(defun get-system-parameter (end-point system-parameter)
  (dolist (parameter (get-val end-point 'return-parameters))
    (if (string-equal (get-val parameter 'system-parameter) system-parameter)
        (return-from get-system-parameter parameter))))

(defun get-system-parameter-name (channel end-point-type system-parameter)
  (get-val (get-system-parameter (get-end-point channel end-point-type) system-parameter)
           'parameter-name))

(defun get-end-point-parameter (end-point parameter-name)
  (dolist (parameter (get-val end-point 'parameters))
    (if (string-equal (get-val parameter 'parameter-name) parameter-name)
        (return-from get-end-point-parameter parameter))))

(defun get-end-point-parameter-value (end-point parameter-name)
  (get-val (get-end-point-parameter end-point parameter-name)
           'default-value))

#|(defmacro with-channel-request (channel user &body body)
  
  `(let ,(list 
          `(channel ,channel)
          `(user ,user)
          `())
     ,@body))
|#


(unless (get-social-channel "Facebook")
    (persist
     (make-social-channel 
      "Facebook" "OAuth2" 
      :app-id (if (string-equal *installation* "Live Serve")
                  "254949787943221"
                  "470974262943332")
      :app-secret (if (string-equal *installation* "Live Serve")
                      "b7ca6b0a7243cb0df0b39d009257e4b2"
                      "60e49502079c8b82391e28e273513ce2")
      :end-points
      (list (make-instance 
             'end-point 
             :end-point-type "Request Token"
             :call-type "GET"
             :uri "https://www.facebook.com/dialog/oauth"

             :return-type "Callback"
             :parameters 
             (list (make-instance 'uri-parameter
                                  :parameter-name "client_id"
                                  :url-encode-p nil
                                  :default-value 'app-id)
                   (make-instance 'uri-parameter
                                  :parameter-name "response_type"
                                  :url-encode-p nil
                                  :default-value "code")
                   (make-instance 'uri-parameter
                                  :parameter-name "scope"
                                  :url-encode-p nil
                                  :default-value "user_about_me,friends_about_me,email,user_location,friends_location,user_questions,friends_questions,publish_stream,read_stream,read_insights,read_mailbox,manage_notifications,manage_pages")
                   (make-instance 'uri-parameter
                                  :parameter-name "redirect_uri"
                                  :url-encode-p nil
                                  :default-value 
                                  (if (string-equal *installation* "Live Serve")
                                      "http://app.digyourbrand.co.za/dyb/oauthcallback?channel=facebook"
                                      "http://local.dataxware.co.za/dyb/oauthcallback?channel=facebook"))
                   (make-instance 'uri-parameter
                                  :parameter-name "state"
                                  :url-encode-p nil
                                  :default-value 'user-id))
             :return-parameters 
             (list (make-instance 'uri-parameter
                                  :parameter-name "code"
                                  :url-encode-p nil
                                  :system-parameter "request-token")
                   (make-instance 'uri-parameter
                                  :parameter-name "error"
                                  :url-encode-p nil
                                  :system-parameter "error")
                   (make-instance 'uri-parameter
                                  :parameter-name "error_reason"
                                  :url-encode-p nil)
                   (make-instance 'uri-parameter
                                  :parameter-name "error_description"
                                  :url-encode-p nil
                                  :system-parameter "error-description")
                   (make-instance 'uri-parameter
                                  :parameter-name "state"
                                  :url-encode-p nil
                                  :system-parameter "verification-code")
                   ))
            (make-instance 
             'end-point 
             :end-point-type "Access Token"
             :call-type "GET"
             :uri "https://graph.facebook.com/oauth/access_token"
             :return-type "Query String"
             :parameters 
             (list (make-instance 'uri-parameter
                                  :parameter-name "client_id"
                                  :url-encode-p nil
                                  :default-value "254949787943221")                             
                   (make-instance 'uri-parameter
                                  :parameter-name "redirect_uri"
                                  :url-encode-p nil
                                  :default-value 
                                  (if (string-equal *installation* "Live Serve")
                                      "http://app.digyourbrand.co.za/dyb/oauthcallback?channel=facebook"
                                      "http://local.dataxware.co.za/dyb/oauthcallback?channel=facebook"))
                   (make-instance 'uri-parameter
                                  :parameter-name "client_secret"
                                  :url-encode-p nil
                                  :default-value "b7ca6b0a7243cb0df0b39d009257e4b2")
                   (make-instance 'uri-parameter
                                  :parameter-name "code"
                                  :url-encode-p nil
                                  :default-value 'request-unique-id))
             :return-parameters (list (make-instance 'uri-parameter
                                              :parameter-name "access_token"
                                              :url-encode-p nil
                                              :system-parameter "access-token")
                                      (make-instance 'uri-parameter
                                              :parameter-name "expires"
                                              :url-encode-p nil
                                              :system-parameter "access-token-expiry")))
            (make-instance 
             'end-point 
             :end-point-type "User ID"
             :call-type "GET"
             :uri "https://graph.facebook.com"

             :return-type "JSON"
             :parameters 
             (list 
               (make-instance 'uri-parameter
                             :parameter-name "user-name"
                             :url-encode-p nil
                             :default-value 'user-name
                             :part-of-url-p t))
             :return-parameters (list (make-instance 'uri-parameter
                                              :parameter-name "id"
                                              :url-encode-p nil
                                              :system-parameter "id")
                                      ))
            (make-instance 
             'end-point 
             :end-point-type "Feed"
             :call-type "GET"
             :uri "https://graph.facebook.com"

             :return-type "JSON"
             :parameters 
             (list 
               (make-instance 'uri-parameter
                             :parameter-name "user-name"
                             :url-encode-p nil
                             :default-value 'user-name
                             :part-of-url-p t)
               (make-instance 'uri-parameter
                             :parameter-name "feed"
                             :url-encode-p nil
                             :default-value "feed"
                             :part-of-url-p t)
               (make-instance 'uri-parameter
                             :parameter-name "limit"
                             :url-encode-p nil
                             :default-value "2000")
               (make-instance 'uri-parameter
                             :parameter-name "access_token"
                             :url-encode-p nil
                             :default-value 'last-access-token))
             )
            ))))


(unless (get-social-channel "Twitter")
    (persist
     (make-social-channel 
      "Twitter" "OAuth1" 
      :app-id (if (string-equal *installation* "Live Serve")
                  "4O5D2Awvn4lv1r7RJmWoAA"
                  "H5wWh6azz3n0Go4hOu5kgg"
                  )
      :app-secret (if (string-equal *installation* "Live Serve")
                      "SGATve6iTNXyZ5ZZGTazNmNuf85acHjDHybv68"
                      "m7u7UoEyTPIg5p0Gl1EV73hkl139tu3GkZjMetzS7G8")
      :end-points
      (list (make-instance 
             'end-point 
             :end-point-type "Request App Authentication"
             :call-type "POST"
             :uri "https://api.twitter.com/oauth/request_token"

             :return-type "Query String"
             :parameters 
             (list 
              (make-instance 
               'uri-parameter
               :parameter-name "oauth_callback"
               :url-encode-p nil
               :default-value 
               (if (string-equal *installation* "Live Serve")
                   "http://app.digyourbrand.co.za/dyb/oauthcallback?channel=twitter"
                   "http://local.dataxware.co.za/dyb/oauthcallback?channel=twitter"))
              (make-instance 'uri-parameter
                             :parameter-name "oauth_consumer_key"
                             :url-encode-p nil
                             :default-value 'app-id)
              (make-instance 'uri-parameter
                             :parameter-name "oauth_nonce"
                             :url-encode-p nil
                             :default-value nil)
              (make-instance 'uri-parameter
                             :parameter-name "signature"
                             :url-encode-p nil
                             :default-value nil)
              (make-instance 'uri-parameter
                             :parameter-name "oauth_signature_method"
                             :url-encode-p nil
                             :default-value "HMAC-SHA1")
              (make-instance 'uri-parameter
                             :parameter-name "oauth_timestamp"
                             :url-encode-p nil
                             :default-value 'stamp)
              (make-instance 'uri-parameter
                             :parameter-name "oauth_version"
                             :url-encode-p nil
                             :default-value "1.0"))
             :return-parameters (list (make-instance 'uri-parameter
                                              :parameter-name "oauth_token"
                                              :url-encode-p nil
                                              :system-parameter "request-token")
                                      (make-instance 'uri-parameter
                                              :parameter-name "oauth_token_secret"
                                              :url-encode-p nil
                                              :system-parameter "request-token-secret")))
            (make-instance 
             'end-point 
             :end-point-type "Request Token"
             :call-type "GET"
             :uri "https://api.twitter.com/oauth/authorize"

             :return-type "Query String"
             :parameters 
             (list 
               (make-instance 'uri-parameter
                             :parameter-name "oauth_token"
                             :url-encode-p nil
                             :default-value 'request-token))
             :return-parameters (list (make-instance 'uri-parameter
                                              :parameter-name "oauth_token"
                                              :url-encode-p nil
                                              :system-parameter "request-token")
                                      (make-instance 'uri-parameter
                                              :parameter-name "oauth_verifier"
                                              :url-encode-p nil
                                              :system-parameter "verification-code")))
            (make-instance 
             'end-point 
             :end-point-type "Access Token"
             :call-type "POST"
             :uri "https://api.twitter.com/oauth/access_token"

             :return-type "Query String"
             :parameters 
             (list 
              
              (make-instance 'uri-parameter
                             :parameter-name "oauth_consumer_key"
                             :url-encode-p nil
                             :default-value 'app-id)
              (make-instance 'uri-parameter
                             :parameter-name "oauth_nonce"
                             :url-encode-p nil
                             :default-value nil)
              (make-instance 'uri-parameter
                             :parameter-name "signature"
                             :url-encode-p nil
                             :default-value nil)
              (make-instance 'uri-parameter
                             :parameter-name "oauth_signature_method"
                             :url-encode-p nil
                             :default-value "HMAC-SHA1")
              (make-instance 'uri-parameter
                             :parameter-name "oauth_timestamp"
                             :url-encode-p nil
                             :default-value 'stamp)
              (make-instance 'uri-parameter
                             :parameter-name "oauth_token"
                             :url-encode-p nil
                             :default-value 'request-token)
              (make-instance 'uri-parameter
                             :parameter-name "oauth_verifier"
                             :url-encode-p nil
                             :default-value 'request-secret)
              (make-instance 'uri-parameter
                             :parameter-name "oauth_version"
                             :url-encode-p nil
                             :default-value "1.0"))
             :return-parameters (list (make-instance 'uri-parameter
                                              :parameter-name "oauth_token"
                                              :url-encode-p nil
                                              :system-parameter "access-token")
                                      (make-instance 'uri-parameter
                                              :parameter-name "oauth_token_secret"
                                              :url-encode-p nil
                                              :system-parameter "access-token-secret")))
            (make-instance 
             'end-point 
             :end-point-type "User ID"
             :call-type "GET"
             :uri "http://api.twitter.com/1/users/lookup.json"

             :return-type "JSON"
             :parameters 
             (list 
               (make-instance 'uri-parameter
                             :parameter-name "screen_name"
                             :url-encode-p nil
                             :default-value 'user-name))
             )
            (make-instance 
             'end-point 
             :end-point-type "Feed"
             :call-type "GET"
             :uri "http://api.twitter.com/1.1/statuses/home_timeline.json"

             :return-type "JSON"
             :parameters 
             (list 
               (make-instance 'uri-parameter
                             :parameter-name "count"
                             :url-encode-p nil
                             :default-value "800"
                             :part-of-url-p t)
               
               
               )
             )
            ))))

(unless (get-social-channel "LinkedIn")
    (persist
     (make-social-channel 
      "LinkedIn" "OAuth1" 
      :app-id (if (string-equal *installation* "Live Serve")
                  "02f0rqn8lwzu"
                  "ccp3psrastub")
      :app-secret (if (string-equal *installation* "Live Serve")
                      "RAp56s0Gc4bV1TC6"
                      "4VdvtsVhOruXZFmo")
      :end-points
      (list (make-instance 
             'end-point 
             :end-point-type "Request App Authentication"
             :call-type "POST"
             :uri "https://api.linkedin.com/uas/oauth/requestToken"

             :return-type "Query String"
             :parameters 
             (list 
              (make-instance 
               'uri-parameter
               :parameter-name "oauth_callback"
               :url-encode-p nil
               :default-value 
               (if (string-equal *installation* "Live Serve")
                   "http://app.digyourbrand.co.za/dyb/oauthcallback?channel=LinkedIn"
                   "http://local.dataxware.co.za/dyb/oauthcallback?channel=LinkedIn"))
              (make-instance 'uri-parameter
                             :parameter-name "oauth_consumer_key"
                             :url-encode-p nil
                             :default-value 'app-id)
              (make-instance 'uri-parameter
                             :parameter-name "oauth_nonce"
                             :url-encode-p nil
                             :default-value nil)
              (make-instance 'uri-parameter
                             :parameter-name "scope"
                             :url-encode-p nil
                             :default-value "r_fullprofile w_messages r_network rw_nus")
              (make-instance 'uri-parameter
                             :parameter-name "signature"
                             :url-encode-p nil
                             :default-value nil)
              (make-instance 'uri-parameter
                             :parameter-name "oauth_signature_method"
                             :url-encode-p nil
                             :default-value "HMAC-SHA1")
              (make-instance 'uri-parameter
                             :parameter-name "oauth_timestamp"
                             :url-encode-p nil
                             :default-value 'stamp)
              (make-instance 'uri-parameter
                             :parameter-name "oauth_version"
                             :url-encode-p nil
                             :default-value "1.0"))
             :return-parameters (list (make-instance 'uri-parameter
                                              :parameter-name "oauth_token"
                                              :url-encode-p nil
                                              :system-parameter "request-token")
                                      (make-instance 'uri-parameter
                                              :parameter-name "oauth_token_secret"
                                              :url-encode-p nil
                                              :system-parameter "request-token-secret")))
            (make-instance 
             'end-point 
             :end-point-type "Request Token"
             :call-type "GET"
             :uri "https://www.linkedin.com/uas/oauth/authenticate"

             :return-type "Query String"
             :parameters 
             (list 
               (make-instance 'uri-parameter
                             :parameter-name "oauth_token"
                             :url-encode-p nil
                             :default-value 'request-token))
             :return-parameters (list (make-instance 'uri-parameter
                                              :parameter-name "oauth_token"
                                              :url-encode-p nil
                                              :system-parameter "request-token")
                                      (make-instance 'uri-parameter
                                              :parameter-name "oauth_verifier"
                                              :url-encode-p nil
                                              :system-parameter "verification-code")))
            (make-instance 
             'end-point 
             :end-point-type "Access Token"
             :call-type "POST"
             :uri "https://api.linkedin.com/uas/oauth/accessToken"

             :return-type "Query String"
             :parameters 
             (list 
              
              (make-instance 'uri-parameter
                             :parameter-name "oauth_consumer_key"
                             :url-encode-p nil
                             :default-value 'app-id)
              (make-instance 'uri-parameter
                             :parameter-name "oauth_nonce"
                             :url-encode-p nil
                             :default-value nil)
              (make-instance 'uri-parameter
                             :parameter-name "signature"
                             :url-encode-p nil
                             :default-value nil)
              (make-instance 'uri-parameter
                             :parameter-name "oauth_signature_method"
                             :url-encode-p nil
                             :default-value "HMAC-SHA1")
              (make-instance 'uri-parameter
                             :parameter-name "oauth_timestamp"
                             :url-encode-p nil
                             :default-value 'stamp)
              (make-instance 'uri-parameter
                             :parameter-name "oauth_token"
                             :url-encode-p nil
                             :default-value 'request-token)
              (make-instance 'uri-parameter
                             :parameter-name "oauth_verifier"
                             :url-encode-p nil
                             :default-value 'request-secret)
              (make-instance 'uri-parameter
                             :parameter-name "oauth_version"
                             :url-encode-p nil
                             :default-value "1.0"))
             :return-parameters (list (make-instance 'uri-parameter
                                              :parameter-name "oauth_token"
                                              :url-encode-p nil
                                              :system-parameter "access-token")
                                      (make-instance 'uri-parameter
                                              :parameter-name "oauth_token_secret"
                                              :url-encode-p nil
                                              :system-parameter "access-token-secret")))
            (make-instance 
             'end-point 
             :end-point-type "User ID"
             :call-type "GET"
             :uri "http://api.linkedin.com/v1/people/~:(id)?format=json"

             :return-type "JSON"
             :parameters nil
             
             )
            (make-instance 
             'end-point 
             :end-point-type "Feed"
             :call-type "GET"
             :uri "http://api.linkedin.com/1.1/statuses/home_timeline.json"

             :return-type "JSON"
             :parameters 
             (list 
               (make-instance 'uri-parameter
                             :parameter-name "count"
                             :url-encode-p nil
                             :default-value "800"
                             :part-of-url-p t)
               
               
               )
             )
            ))))