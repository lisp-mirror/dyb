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

(defun facebook-refresh-friends ()
  (dolist (user (coerce (channel-users) 'list ))
        
    (when (and user (string-equal (get-val user 'doc-status) "Active"))
      ;;TODO: How to get error messages in for users without access tokens.
      (when (string-equal (get-val user 'channel-user-type) "Facebook")
        (when (get-val user 'last-access-token)
          
          (multiple-value-bind (posts error)
              (facebook-friends-refresh user)
            ))))))

(defun facebook-profile-refresh (user)
  (when (and user (string-equal (get-val user 'doc-status) "Active"))
    
      (when (string-equal (get-val user 'channel-user-type) "Facebook")
        (when (get-val user 'last-access-token)

          (multiple-value-bind (profile error)
              (facebook-friends user)
            (unless error
              (unless (get-val user 'user-data)
                  (setf (get-val user 'user-data) (make-hash-table :test 'equal)))
              
              (setf (gethash "profile"
                             (get-val user 'user-data))
                    profile)
              (persist user)))))))

(defun facebook-refresh-profiles ()
  (dolist (user (coerce (channel-users) 'list ))
        
    (when (and user (string-equal (get-val user 'doc-status) "Active"))
      ;;TODO: How to get error messages in for users without access tokens.
      (when (string-equal (get-val user 'channel-user-type) "Facebook")
        (when (get-val user 'last-access-token)
          
          (multiple-value-bind (posts error)
              (facebook-profile-refresh user)
            ))))))

(defun get-last-post-date (user)
  (let ((date 0))
    (find-docs 'list 
               (lambda (doc)
                 (if (string-equal (get-val (get-val doc 'channel-user) 
                                            'channel-user-name) 
                                   (get-val user 'channel-user-name))
                     (if (> (get-val doc 'created-date) date)
                         (setf date (get-val doc 'created-date)))))
               (generic-post-collection))
    date))

(defun facebook-refresh-feeds ()
  (dolist (user (coerce (channel-users) 'list ))
     (when (and user (string-equal (get-val user 'doc-status) "Active"))
      ;;TODO: How to get error messages in for users without access tokens.
      (when (string-equal (get-val user 'channel-user-type) "Facebook")
        (when (get-val user 'last-access-token)
          (let ((last-date (get-last-post-date user)))

            (multiple-value-bind (posts error)
                (facebook-feed user (if (> last-date 0)
                                        last-date))
              (unless error
                (parse-facebook-posts user posts 'facebook-feed)
              
                ))))))))
