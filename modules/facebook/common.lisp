(in-package :dyb)

(defun post-to-facebook (action)
  (when action
    (when (string-equal (get-val action 'action-status) "Pending")
      
      (let ((from-user (get-channel-user-by-user-name (get-val action 'from-user-id))))

        (cond ((string-equal (get-val action 'post-type) "Facebook")
               (cond ((string-equal (get-val action 'action-type) "Post")
                      (multiple-value-bind (result error-message)
                          (post-facebook (get-val action 'from-user-id) 
                                         (get-val action 'action-content))
                        (when error-message
                          (generic-action-log action 
                                              "Error"
                                              error-message
                                              "Pending"))
                        (unless error-message
                          (generic-action-log action 
                                              "Result"
                                              result
                                              "Completed"))))
                     ((string-equal (get-val action 'action-type) "Comment")
                      (multiple-value-bind (result error-message)
                          (comment-facebook (get-val action 'pid)
                                            (get-val action 'from-user-id)
                                            (get-val action 'action-content))
                        (when error-message
                          (generic-action-log action 
                                              "Error"
                                              error-message
                                              "Pending"))
                        (unless error-message
                          (generic-action-log action 
                                              "Result"
                                              result
                                              "Completed"))))
                     ((string-equal (get-val action 'action-type) "Like")
                      (multiple-value-bind (result error-message)
                          (facebook-like (get-val action 'pid)
                                         (get-val action 'from-user-id))
                        (when error-message
                          (generic-action-log action 
                                              "Error"
                                              error-message
                                              "Pending"))
                        (unless error-message
                          (generic-action-log action 
                                              "Result"
                                              result
                                              "Completed"))))))
              ((string-equal (get-val action 'post-type) "Twitter")
               (cond ((string-equal (get-val action 'action-type) "Tweet")
                      (multiple-value-bind (result error-message)
                          (post-twitter  
                           (get-val action 'from-user-id)
                           (get-val action 'action-content))
                        (when error-message
                          (generic-action-log action 
                                              "Error"
                                              error-message
                                              "Pending"))
                        (unless error-message
                          (generic-action-log action 
                                              "Result"
                                              result
                                              "Completed"))))
                     ((string-equal (get-val action 'action-type) "Retweet")
                      (multiple-value-bind (result error-message)
                          (retweet-twitter  
                           (get-val action 'from-user-id)
                           (get-val action 'pid))
                        (when error-message
                          (generic-action-log action 
                                              "Error"
                                              error-message
                                              "Pending"))
                        (unless error-message
                          (generic-action-log action 
                                              "Result"
                                              result
                                              "Completed"))))
                     ((string-equal (get-val action 'action-type) "Reply")
                      (multiple-value-bind (result error-message)
                          (reply-twitter
                           (get-val action 'from-user-id)
                           (get-val action 'action-content)
                           ;;TODO: Get right user name
                           (get-val action 'to-user-id)
                                         )
                        (when error-message
                          (generic-action-log action 
                                              "Error"
                                              error-message
                                              "Pending"))
                        (unless error-message
                          (generic-action-log action 
                                              "Result"
                                              result
                                              "Completed"))))
                     ((string-equal (get-val action 'action-type) "Favourite")
                      (multiple-value-bind (result error-message)
                          (favourite-twitter
                           (get-val action 'from-user-id)
                           (get-val action 'pid)
                                         )
                        (when error-message
                          (generic-action-log action 
                                              "Error"
                                              error-message
                                              "Pending"))
                        (unless error-message
                          (generic-action-log action 
                                              "Result"
                                              result
                                              "Completed"))))
                     )
                )

          )))))

(defun post-facebook-scheduled-actions ()
  (dolist (action (coerce (generic-actions) 'list))
    (when (or 
             (string-equal (get-val action 'action-status) "")
             (string-equal (get-val action 'action-status) "A"))
      (when (< (get-val action 'scheduled-date) (get-universal-time))
          (post-to-facebook action)))))