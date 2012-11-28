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
              (facebook-profile user)
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


(defun get-last-insight-date (user)
  (let ((date 0))
    (find-docs 'list 
               (lambda (doc)
                 (if (string-equal (get-val (get-val doc 'channel-user) 
                                            'channel-user-name) 
                                   (get-val user 'channel-user-name))
                     (if (get-val doc 'end-time)
                         (if (> (universal-to-gmt-0 (get-val doc 'end-time))
                                (universal-to-gmt-0 date))
                             (setf date (get-val doc 'end-time))))))
               (facebook-insight-value-collection))
    date))

(defun facebook-page-insights-refresh (user last-date)
  (when (and user (string-equal (get-val user 'doc-status) "Active"))
    
      (when (string-equal (get-val user 'channel-user-type) "Facebook")
        (when (get-val user 'last-access-token)

          (multiple-value-bind (insights error)
              (facebook-page-insights user 
                                      (if last-date
                                          (if (> last-date 0)
                                              last-date
                                              ))
                                      (if last-date
                                          (if (> last-date 0)
                                              (+ last-date (* 60 60 24))))
                                      )
            (unless error
              (parse-facebook-insights user insights)))))))

(defun facebook-refresh-page-insights ()
  (dolist (user (coerce (channel-users) 'list ))
        
    (when (and user (string-equal (get-val user 'doc-status) "Active"))
      ;;TODO: How to get error messages in for users without access tokens.
      (when (string-equal (get-val user 'channel-user-type) "Facebook")
        (when (get-val user 'last-access-token)
          (let ((last-date (get-last-insight-date user)))
            
            (multiple-value-bind (insights error)
                (facebook-page-insights-refresh user (if (> last-date 0)
                                                         last-date
                                                         ))
              )))))))

(defun facebook-page-insights-history (from-date days)
  (dolist (user (coerce (channel-users) 'list ))       
    (when (and user (string-equal (get-val user 'doc-status) "Active"))
      ;;TODO: How to get error messages in for users without access tokens.
      (when (string-equal (get-val user 'channel-user-type) "Facebook")
        (when (get-val user 'last-access-token)
          (loop for i upto (+ (get-universal-time) (* 60 60 24 days))
               do
               (multiple-value-bind (insights error)
                   (facebook-page-insights-refresh user from-date)
                 )))))))

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
