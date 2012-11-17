(in-package :dyb)

(defun facebook-friends-refresh (user)
  (when (and user (string-equal (get-val user 'doc-status) "Active"))
    
      (when (string-equal (get-val user 'channel-user-type) "Facebook")
        (when (get-val user 'last-access-token)

          (multiple-value-bind (friends error)
              (facebook-friends user)
            (unless error
              (unless (get-val user 'user-data)
                  (setf (get-val user 'user-data) (make-hash-table :test 'equal)))
              
              (setf (gethash "friends"
                             (get-val user 'user-data))
                    friends)
              (persist user)))))))

(defun facebook-refresh-feeds ()
  (dolist (user (coerce (channel-users) 'list ))
        
    (when (and user (string-equal (get-val user 'doc-status) "Active"))
      ;;TODO: How to get error messages in for users without access tokens.
      (when (string-equal (get-val user 'channel-user-type) "Facebook")
        (when (get-val user 'last-access-token)
          (facebook-friends-refresh user)
          (multiple-value-bind (posts error)
              (facebook-feed user)
            (unless error
              (parse-facebook-posts user posts 'facebook-feed)
              )))))))
