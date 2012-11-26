(in-package :dyb)


(defun facebook-like (user post-id)
  (handle-endpoint 
   user
   (drakma:http-request 
           (format nil "https://graph.facebook.com/~A/likes" 
                   post-id)
           :parameters `(("oauth_token" . ,(get-val user 'last-access-token)))
           :method :post)))

(defun comment-facebook (user post-id message)
  (handle-endpoint 
     user
     (drakma:http-request 
      (format nil "https://graph.facebook.com/~A/comments" 
              post-id)
      :method :post
      :parameters `(("message" . ,message)
                    ("oauth_token" . ,(get-val user 'last-access-token))))))

(defun facebook-feed (user)
  (handle-endpoint 
   user
   (drakma:http-request 
           (format nil "https://graph.facebook.com/~A/feed?limit=2000&access_token=~A" 
                   (url-encode (get-val user 'user-id))
                   (get-val user 'last-access-token)))))

(defun post-facebook (user-id message)
  (let ((user (get-channel-user-by-user-id user-id)))
    (handle-endpoint 
     user
     (drakma:http-request
      (format nil "https://graph.facebook.com/~A/feed"
              user-id)
      :method :post
      :parameters `(("message" . ,message)
                    ("oauth_token" . ,(get-val user 'last-access-token)))))))

(defun facebook-friends (user)
  (handle-endpoint 
   user
   (drakma:http-request 
           (format nil "https://graph.facebook.com/~A/friends?access_token=~A&fields=bio,email,link,gender,locale,location,first_name,username,name,picture" 
                   (url-encode (get-val user 'user-id))
                   
                   (get-val user 'last-access-token)))))

(defun facebook-profile (user)
  (handle-endpoint
   user
   (drakma:http-request 
           (format nil "https://graph.facebook.com/me?access_token=~A" 
                   (get-val user 'last-access-token)))))

(defun facebook-accounts (user)
  (handle-endpoint
   user
   (drakma:http-request 
           (format nil "https://graph.facebook.com/me/accounts?fields=about,access_token,affiliation,app_id,perms,can_post,id,username,talking_about_count,new_like_count,global_brand_like_count,description,likes,is_published,genre,category&access_token=~A" 
                   (get-val user 'last-access-token)))))