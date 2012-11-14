(in-package :dyb)


(defun facebook-feed (user)
  (let ((feed)
        (message))
    (when (get-val user 'last-access-token)
      (multiple-value-bind (bodyx)
          (drakma:http-request 
           (format nil "https://graph.facebook.com/~A/feed?limit=2000&access_token=~A" 
                   (url-encode (get-val user 'user-id))
                   (get-val user 'last-access-token)))
       (setf feed (json::decode-json-from-string bodyx)) 
       (if (assoc-path feed :error)
           (setf message (cdr (assoc-path feed :error :message))))))
    (values feed message)))


(defun facebook-like (user post-id)
  (let ((feed)
        (message))
    (when (get-val user 'last-access-token)
      (multiple-value-bind (bodyx)
          (drakma:http-request 
           (format nil "https://graph.facebook.com/~A/posts/~A/likes?access_token=~A" 
                   (url-encode (get-val user 'user-id))
                   post-id
                   (get-val user 'last-access-token)))
       (setf feed (json::decode-json-from-string bodyx)) 
       (if (assoc-path feed :error)
           (setf message (cdr (assoc-path feed :error :message))))))
    (values feed message)))

(defun facebook-friends (user)
  (let ((feed)
        (message))
    (when (get-val user 'last-access-token)
      (multiple-value-bind (bodyx)
          (drakma:http-request 
           (format nil "https://graph.facebook.com/~A/friends?access_token=~A&fields=bio,email,link,gender,locale,location,first_name,username,name,picture" 
                   (url-encode (get-val user 'user-id))
                   
                   (get-val user 'last-access-token)))
       (setf feed (json::decode-json-from-string bodyx)) 
       (if (assoc-path feed :error)
           (setf message (cdr (assoc-path feed :error :message))))))
    (values feed message)))