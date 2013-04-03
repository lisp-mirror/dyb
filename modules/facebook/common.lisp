(in-package :dyb)

(defun post-scheduled-action (action)
  (with-slots (action-status post-type action-type
               post-id) action
    (when (and action
               (equal action-status "Pending"))
      (let ((from-user (or (get-channel-user-by-user-name
                            (get-val action 'from-user-id)
                            (get-val action 'post-type))
                           (get-channel-user-by-user-id
                            (get-val action 'from-user-id)
                            (get-val action 'post-type)))))
        (multiple-value-bind (result error-message)
            (cond ((equal post-type "Facebook")
                   (cond ((equal action-type "Post")
                          (cond
                            ((and (blank-p (get-val action 'image-url))
                                  (blank-p (get-val action 'post-url)))
                             (post-facebook-link-image (get-val action 'from-user-id)
                                                       (get-val action 'action-content)
                                                       (or (format-short-url
                                                            (get-val action 'short-url))
                                                           (get-val action 'post-url))
                                                       (get-val action 'image-url)))

                            ((blank-p (get-val action 'image-url))
                             (post-facebook-image (get-val action 'from-user-id)
                                                  (get-val action 'action-content)
                                                  (get-val action 'image-url)))
                            ((or (blank-p (get-val action 'short-url))
                                 (blank-p (get-val action 'post-url)))
                             (post-facebook-url (get-val action 'from-user-id)
                                                (get-val action 'action-content)
                                                (or (format-short-url
                                                     (get-val action 'short-url))
                                                    (get-val action 'post-url))))
                            (t
                             (post-facebook (get-val action 'from-user-id)
                                            (get-val action 'action-content)))))
                         ((equal action-type "Comment")
                          (comment-facebook from-user
                                            post-id
                                            ;;  (get-val action 'from-user-id)
                                            (get-val action 'action-content)))
                         ((equal action-type "Like")
                          (facebook-like from-user  post-id
                                         ;;(get-val action 'from-user-id)
                                         ))))

                  ((equal post-type "Twitter")
                   (cond ((or (equal action-type "Tweet")
                              (equal action-type "Post"))
                          (post-twitter
                           from-user
                           (get-val action 'action-content)
                           :image-path (get-val action 'image-url)
                           :link-url (if (or (blank-p (get-val action 'short-url))
                                             (blank-p (get-val action 'post-url)))
                                         (or (format-short-url
                                              (get-val action 'short-url))
                                             (get-val action 'post-url)))))
                         ((equal action-type "Retweet")
                          (retweet-twitter from-user post-id))
                         ((equal action-type "Reply")
                          (reply-twitter
                           from-user
                           (get-val action 'action-content)
                           ;;TODO: Get right user name
                           (get-val action 'to-user-id)))
                         ((equal action-type "Favourite")
                          (favourite-twitter from-user post-id))))
                  ((equal post-type "LinkedIn")
                   (cond ((equal action-type "Post")
                          (if (string-equal (get-val from-user 'profile-type) "Page")
                              (linkedin-company-share
                               from-user
                               (get-val action 'action-content)
                               :submited-url (or (format-short-url
                                                  (get-val action 'short-url))
                                                 (get-val action 'post-url))
                               :submitted-image-url (get-val action 'post-url))
                              (linkedin-share
                               from-user
                               (get-val action 'action-content)
                               :submited-url (or (format-short-url (get-val action 'short-url))
                                                 (get-val action 'post-url))
                               :submitted-image-url (get-val action 'post-url)))))))
          (if error-message
              (add-generic-action-log action "Error" error-message "Pending")
              (add-generic-action-log action "Result" result "Completed")))))))
