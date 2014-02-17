(in-package :dyb)


(defun facebook-like (user post-id)
  (handle-endpoint 
   user
   (drakma-request 
           (format nil "https://graph.facebook.com/~A/likes" 
                   post-id)
           :parameters `(("oauth_token" . ,(get-val user 'last-access-token)))
           :method :post)))

(defun comment-facebook (user post-id message)
  (handle-endpoint 
      user
      (drakma-request  
       (format nil "https://graph.facebook.com/~A/comments" 
               post-id)
       :method :post
       :parameters `(("message" . ,message)
                     ("oauth_token" . ,(get-val user 'last-access-token))))))

(defun facebook-feed (user since)
  (handle-endpoint
   user
   (drakma:http-request 
    (format nil "https://graph.facebook.com/~A/feed?limit=2000&since=~A&access_token=~A" 
            (url-encode (get-val user 'user-id))
            (etypecase since
              (string since)
              (integer
               (universal-time-to-unix-time since))
              (null
               (universal-time-to-unix-time (parse-date "01 Jan 2011"))))
            (get-val user 'last-access-token)))))

(defun facebook-comments (user post-id)
  (handle-endpoint
   user
   (drakma:http-request 
    (format nil "https://graph.facebook.com/~A/comments?summary=1&filter=stream&access_token=~A" 
            post-id
            (get-val user 'last-access-token)))))

(defun facebook-likes (user post-id)
  (handle-endpoint
   user
   (drakma:http-request 
    (format nil "https://graph.facebook.com/~A/likes?summary=1&filter=streamaccess_token=~A" 
            post-id
            (get-val user 'last-access-token)))))

(defun post-facebook (user-id message)
  (let ((user (get-channel-user-by-user-id user-id "Facebook")))
    (handle-endpoint 
     user
     (drakma-request
      (format nil "https://graph.facebook.com/~A/feed"
              user-id)
      :method :post
      :parameters `(("message" . ,message)
                    ("oauth_token" . ,(get-val user 'last-access-token)))))))

(defun facebook-friends (user)
  (handle-endpoint 
   user
   (drakma-request 
           (format nil "https://graph.facebook.com/~A/friends?access_token=~A&fields=bio,email,link,gender,locale,location,first_name,username,name,picture" 
                   (url-encode (get-val user 'user-id))
                   
                   (get-val user 'last-access-token)))))

(defun facebook-profile (user)
  (handle-endpoint
   user
   (drakma-request 
           (format nil "https://graph.facebook.com/me?access_token=~A" 
                   (get-val user 'last-access-token)))))

(defun facebook-accounts (user)
  (handle-endpoint
   user
   (drakma-request 
           (format nil "https://graph.facebook.com/me/accounts?fields=about,access_token,affiliation,app_id,perms,can_post,id,username,talking_about_count,new_like_count,global_brand_like_count,description,likes,is_published,genre,category&access_token=~A" 
                   (get-val user 'last-access-token)))))

(defun post-facebook-image-url (user-id message image-url)
  (let ((user (get-channel-user-by-user-id user-id "Facebook")))
    (handle-endpoint 
     user
     (drakma-request
      (format nil "https://graph.facebook.com/~A/photos"
              user-id)
      :method :post
      :parameters `(("message" . ,message)
                    ("url" . ,image-url)
                    ("oauth_token" . ,(get-val user 'last-access-token)))))))

(defun post-facebook-image (user-id message image-path)
  (let ((user (get-channel-user-by-user-id user-id "Facebook")))
    (handle-endpoint 
     user
     (drakma-request
      (format nil "https://graph.facebook.com/~A/photos"
              user-id)
      :method :post
      :content-type "application/x-www-form-urlencoded; charset=UTF-8"
      :content-length t     
      :parameters `(("message" . ,message)
                    ("type" . "picture")
                    ("source" . ,(pathname image-path))                  
                    ("oauth_token" . ,(get-val user 'last-access-token)))))))



(defun post-facebook-url (user-id message url)
  (let ((user (get-channel-user-by-user-id user-id "Facebook")))
    (handle-endpoint 
     user
     (drakma-request
      (format nil "https://graph.facebook.com/~A/feed"
              user-id)
      :method :post
      :content-length t     
      :parameters `(("message" . ,message)
                    ("link" . ,url)                  
                    ("oauth_token" . ,(get-val user 'last-access-token)))))))

(defun post-facebook-link-image (user-id message url image-path)
  (let ((user (get-channel-user-by-user-id user-id "Facebook")))

    (handle-endpoint 
     user
     (drakma-request
      (format nil "https://graph.facebook.com/~A/photos"
              user-id)
      :method :post
      :content-length t     
      :parameters `(("message" . ,(format nil "~A ~A"  message url))
                    ("type" . "picture")
                    ("source" . ,(pathname image-path))                  
                    ("oauth_token" . ,(get-val user 'last-access-token)))))))

(defun post-facebook-user-profile-no-auth (user-id)
  (let ((profile
         (drakma-request
          (format nil "https://graph.facebook.com/~A"
                  user-id)
          :method :get
          :content-length t)))
    (if profile
        (json:decode-json-from-string profile))))

(defun post-insights (user post-id)
  (handle-endpoint 
     user
     (drakma-request
      (format nil "https://graph.facebook.com/~A/insights?period=week"
              post-id)
      :method :post
      :content-length t     
      :parameters `(("oauth_token" . ,(get-val user 'last-access-token))))))

(defun facebook-page-insights (user since until)
  (handle-endpoint 
   user
   (drakma-request 
           (format nil "https://graph.facebook.com/~A/insights?since=~A&until=~A&limit=10000&access_token=~A" 
                   (url-encode (get-val user 'user-id))
                   (if since
                       (if (stringp since)
                           since
                           (universal-time-to-unix-time since))
                       (universal-time-to-unix-time (parse-date "01 Jan 2011")))
                   (if until
                       (if (stringp until)
                           until
                           (universal-time-to-unix-time until))
                       (universal-time-to-unix-time (parse-date "01 Jan 2011")))
                   (get-val user 'last-access-token)))))

