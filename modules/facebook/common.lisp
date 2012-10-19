(in-package :ems)

(defun post-to-facebook (action)
  (when action
    (unless (string-equal (get-val action 'action-status) "Completed"))
    (let ((from-user (get-service-user-by-user-name (get-val action 'from-user-id))))
      (multiple-value-bind (body)
          (drakma:http-request 
           (format nil "https://graph.facebook.com/~A/feed&access_token=~A"
                   (get-val from-user 'user-id)
                   (get-val from-user 'last-access-token)
                   )
           :method :post
           :parameters (list (cons "message"  (get-val action 'action-content))))

        (let ((error-message (get-facebook-error body) ))           
          (when error-message
            (setf (get-val action 'action-status) "Error")
            (setf (get-val action 'action-log) (cdr (car (rest error-message)))))

          (unless error-message 
            (setf (get-val action 'action-status) "Completed")
            (setf (get-val action 'action-log) "Posted successfully.")))))))

(defun post-facebook-scheduled-actions ()
  (dolist (action (coerce (generic-actions) 'list))
    (unless (string-equal (get-val action 'action-status) "Completed")
      (when (< (get-val action 'scheduled-date) (get-universal-time))
          (post-to-facebook action)))))