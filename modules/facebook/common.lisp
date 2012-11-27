(in-package :dyb)

(defun post-scheduled-action (action)

  (when action
    (when (string-equal (get-val action 'action-status) "Pending")
      
      (let ((from-user (or (get-channel-user-by-user-name (get-val action 'from-user-id))
                           (get-channel-user-by-user-id (get-val action 'from-user-id)))
              ))

        (cond ((string-equal (get-val action 'post-type) "Facebook")
               (cond ((string-equal (get-val action 'action-type) "Post")                  
                      (multiple-value-bind (result error-message)                        
                          (cond
                            ((and (blank-p (get-val action 'image-url))
                                  (blank-p (get-val action 'post-url)))
                             (post-facebook-link-image (get-val action 'from-user-id)
                                                       (get-val action 'action-content)
                                                       (or (format-short-url
                                                            (get-val action 'short-url)) 
                                                           (get-val action 'post-url))
                                                       (get-val action 'image-url) 
                                                       ))

                            ((blank-p (get-val action 'image-url))
                             (post-facebook-image (get-val action 'from-user-id)
                                                  (get-val action 'action-content)
                                                  (get-val action 'image-url)
                                                  ))
                            ((or (blank-p (get-val action 'short-url)) 
                                 (blank-p (get-val action 'post-url)))
                             (post-facebook-url (get-val action 'from-user-id)
                                                (get-val action 'action-content)
                                                (or (format-short-url
                                                     (get-val action 'short-url)) 
                                                    (get-val action 'post-url))
                                                ))
                            (t
                             (post-facebook (get-val action 'from-user-id) 
                                            (get-val action 'action-content)))
                            )
                          
                        (when error-message

                          (add-generic-action-log action 
                                                  "Error"
                                                  error-message
                                                  "Pending"))
                        (unless error-message
                          (add-generic-action-log action 
                                                  "Result"
                                                  result
                                                  "Completed"))))
                     ((string-equal (get-val action 'action-type) "Comment")
                      (multiple-value-bind (result error-message)
                          (comment-facebook (get-val action 'post-id)  
                                            (get-val action 'from-user-id)
                                            (get-val action 'action-content))
                        (when error-message
                          (add-generic-action-log action 
                                              "Error"
                                              error-message
                                              "Pending"))
                        (unless error-message
                          (add-generic-action-log action 
                                              "Result"
                                              result
                                              "Completed"))))
                     ((string-equal (get-val action 'action-type) "Like")
                      (multiple-value-bind (result error-message)
                          (facebook-like (get-val action 'post-id)
                                         (get-val action 'from-user-id))
                        (when error-message
                          (add-generic-action-log action 
                                              "Error"
                                              error-message
                                              "Pending"))
                        (unless error-message
                          (add-generic-action-log action 
                                              "Result"
                                              result
                                              "Completed"))))))
              
              ((string-equal (get-val action 'post-type) "Twitter")
               
               (cond ((or (string-equal (get-val action 'action-type) "Tweet")
                          (string-equal (get-val action 'action-type) "Post"))
                      (multiple-value-bind (result error-message)
                          (post-twitter  
                           from-user
                           (get-val action 'action-content)
                           :image-path (get-val action 'image-url)
                           :link-url (if (or (blank-p (get-val action 'short-url))
                                             (blank-p (get-val action 'post-url)))
                                         (or (format-short-url
                                              (get-val action 'short-url)) 
                                             (get-val action 'post-url))))
                        (when error-message
                          (add-generic-action-log action 
                                              "Error"
                                              error-message
                                              "Pending"))
                        (unless error-message
                          (add-generic-action-log action 
                                              "Result"
                                              result
                                              "Completed"))))
                     ((string-equal (get-val action 'action-type) "Retweet")
                      (multiple-value-bind (result error-message)
                          (retweet-twitter  
                           from-user
                           (get-val action 'post-id))
                        (when error-message
                          (add-generic-action-log action 
                                              "Error"
                                              error-message
                                              "Pending"))
                        (unless error-message
                          (add-generic-action-log action 
                                              "Result"
                                              result
                                              "Completed"))))
                     ((string-equal (get-val action 'action-type) "Reply")
                      (multiple-value-bind (result error-message)
                          (reply-twitter
                           from-user
                           (get-val action 'action-content)
                           ;;TODO: Get right user name
                           (get-val action 'to-user-id)
                                         )
                        (when error-message
                          (add-generic-action-log action 
                                              "Error"
                                              error-message
                                              "Pending"))
                        (unless error-message
                          (add-generic-action-log action 
                                              "Result"
                                              result
                                              "Completed"))))
                     ((string-equal (get-val action 'action-type) "Favourite")
                      (multiple-value-bind (result error-message)
                          (favourite-twitter
                           from-user
                           (get-val action 'post-id)
                                         )
                        (when error-message
                          (add-generic-action-log action 
                                              "Error"
                                              error-message
                                              "Pending"))
                        (unless error-message
                          (add-generic-action-log action 
                                              "Result"
                                              result
                                              "Completed"))))
                     )
                )
              ((string-equal (get-val action 'post-type) "LinkedIn")

               (cond ((string-equal (get-val action 'action-type) "Post")

                      (multiple-value-bind (result error-message)
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
                            :submitted-image-url (get-val action 'post-url)
                           
                            ))
                        (when error-message
                          (add-generic-action-log action 
                                                  "Error"
                                                  error-message
                                                  "Pending"))
                        (unless error-message
                          (add-generic-action-log action 
                                                  "Result"
                                                  result
                                                  "Completed"))))))
          )))))

(defun post-scheduled-actions ()
  (dolist (action (coerce (generic-actions) 'list))
    (when (string-equal (get-val action 'action-status) "Pending")
      (when (< (get-val action 'scheduled-date) (get-universal-time) )
        (post-scheduled-action action)))))