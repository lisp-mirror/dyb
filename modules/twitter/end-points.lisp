(in-package :dyb)

(defun at-least-1 (number)
  (if  number
      (if (stringp number)
          (parse-trim-integer number)
          number)
      1))

(defun twitter-callback-uri ()
  (if (string-equal *installation* "Live Serve")
                      "https://app.digyourbrand.co.za/dyb/twitter-oauth-callback"
                      "https://local.dataxware.co.za/dyb/twitter-oauth-callback"))

(defun twitter-request-token ()
  (let* ((end-point "https://api.twitter.com/oauth/request_token")
         (channel (get-social-channel "Twitter"))
         (response (oauth1-drakma-request
                  end-point
                  (get-val channel 'app-id)
                  (get-val channel 'app-secret)
                  :callback-uri 
                  (twitter-callback-uri)
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
                  (twitter-callback-uri)
                  :method :post
                  :preserve-uri t)))
    response))

(defun twitter-user-stream (user)
  (let* ((end-point "https://userstream.twitter.com/1.1/user.json"))
    (twitter-request
     user
     end-point
     :method :post
     :want-stream t
     :handle-response-p nil)))

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

;;TODO: Implement this correctly;
(defun twitter-search (user &key since-id)
  (let* ((end-point (format nil "https://api.twitter.com/1.1/statuses/home_timeline.json?user_id=~A&count=800&since_id=~A&include_rts=true&contributor_details=true" 
                            (get-val user 'user-id)
                             (at-least-1 since-id))))
    (twitter-request
     user
     end-point
     :method :get
     :signature-parameters
     `(("contributor_details" "true")
       ("count" "800")
       ("include_rts" "true")
       ("since_id"  ,(format nil "~A" (at-least-1 since-id)))
       ("user_id" ,(format nil "~A" (get-val user 'user-id))))
     :handle-response-p nil)))

(defun twitter-mention-timeline (user &key since-id)
  (let* ((end-point (format nil "https://api.twitter.com/1.1/statuses/mentions_timeline.json?user_id=~A&count=800&since_id=~A&include_rts=true&include_entities=true&contributor_details=true" 
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
                      ("since_id"  ,(format nil "~A" (at-least-1 since-id)))
                      ("user_id" , (format nil "~A" (get-val user 'user-id)))))))


(defun twitter-direct-messages (user &key since-id)
  (when user
    (let* ((end-point  
            (format nil "https://api.twitter.com/1.1/direct_messages.json?count=200&since_id=~A&include_entities=true"                             
                    (at-least-1 since-id))))
      (twitter-request
       user
       end-point
       :method :get
       :signature-parameters
       `(("count" "200")
         ("include_entities" "true")
         ("since_id"  ,(format nil "~A" (at-least-1 since-id))))
       :handle-response-p nil))))

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

(defun post-twitter (user message &key image-path)
  (handle-endpoint
       user
       (if image-path
           (tweet-with-picture user
                               message
                               image-path)
           (tweet
            user
            message))))

(defun tweet (user 
              message)
  (let* ((end-point
          "https://api.twitter.com/1.1/statuses/update.json")
         (status (trim-whitespace message)))
    (twitter-request
     user
     end-point
     :method :post
     :content-type "application/x-www-form-urlencoded; charset=UTF-8"
     :content (alist-to-url-encoded-string `(("status" . ,status))
                                       +utf-8+)
     :signature-parameters
     `(("status" ,status))
     :preserve-uri t)))

(defun tweet-with-picture (user 
                           message
                           image-path)
  (let* ((end-point
          "https://api.twitter.com/1.1/statuses/update_with_media.json")
         (status (trim-whitespace message)))
    (twitter-request
     user
     end-point
     :method :post
     :parameters `(("status" . ,status)
                   ("media[]" . ,(pathname image-path)))
     :preserve-uri t)))

(defun twitter-favourite (user 
                          tweet-id)
  (let* ((end-point
          "https://api.twitter.com/1.1/favorites/create.json"))
    (twitter-request
     user
     end-point
     :method :post
     :signature-parameters
     `(("id" ,tweet-id))
     :preserve-uri t)))

(defun retweet (user 
                tweet-id)
  (let* ((end-point
          (format nil
                  "https://api.twitter.com/1.1/statuses/retweet/~A.json"
                  tweet-id)))
    (twitter-request
     user
     end-point
     :method :post
     ;;TODO: Check if this is not needed.
     ;;     :signature-parameters
     ;;     `(("id" . , tweet-id))
     :preserve-uri t)))

(defun tweet-reply (user 
                    message
                    at-user
                    in-reply-to-status-id)
  (let* ((end-point "https://api.twitter.com/1.1/statuses/update.json")
         (status (format nil "@~A ~A" at-user (trim-whitespace message))))

    (twitter-request
     user
     end-point
     :method :post
    ;; :parameters `(("status" ,status)
    ;;               ("in_reply_to_status_id" ,in-reply-to-status-id)) 
     :content-type "application/x-www-form-urlencoded; charset=UTF-8"
     :content (alist-to-url-encoded-string `(("status" . ,status)
       ("in_reply_to_status_id" . ,in-reply-to-status-id))
                                       +utf-8+)

     :signature-parameters
     `(("status" ,status)
       ("in_reply_to_status_id" ,in-reply-to-status-id))   
     :preserve-uri t)))














