(in-package :dyb)

(defclass fb-post-comment-form (ajax-widget)
  ((grid :initarg :grid
         :initform nil
         :accessor grid)
   (message :initarg :message))
  (:metaclass widget-class))


(defmethod render ((widget fb-post-comment-form) &key )
  (let* ((comment-form (make-widget 'html-simple-framework-form 
                                    :name "facebook-post-comment-form"
                                    :grid-size 12
                                    :form-id "facebook-post-comment-form"
                                    :action "post-facebook-comment"
                                    :action-title "Comment"))
         (form-section (make-widget 'form-section
                                    :name "form-section"))
         
         (current-post (set-current-row (get-val widget 'grid)))
         (post-id (gpv current-post :id)))

    (with-html 
      (when (parameter "action")
          (render comment-form
                  :content
                  (with-html-to-string ()
                    (:div 
                     (:input :type "hidden" :name "post-id" 
                             :value post-id)

                     (render form-section 
                             :label "As User"
                             :input 
                             (with-html-to-string ()
                               (render-edit-field
                                "user-id" 
                                (parameter "user-id")
                                :data (get-channel-users-list "Facebook" nil)
                                :required t
                                :type :select)))

                     (render form-section 
                             :label "Comment"
                             :input 
                             (with-html-to-string ()
                               (render-edit-field
                                "comment" 
                                (parameter "comment")
                                :required t
                                :type :textarea))))))
          (str (get-val widget 'message))))
    (open-dialog widget (grid widget))))

(defun comment-facebook (post-id user-id message)
  (let ((result)
        (error)
        (user (get-channel-user-by-user-id user-id)))

    (when (get-val user 'last-access-token)
      (multiple-value-bind (body)
          (drakma:http-request 
           (format nil "https://graph.facebook.com/~A/comments" 
                   post-id)
           :method :post
           :parameters `(("message" . ,message)
                         ("oauth_token" . ,(get-val user 'last-access-token))
                         ))
       (setf result (json::decode-json-from-string body)) 
       (if (assoc-path result :error)
           (setf error (cdr (assoc-path result :error :message))))))
    (values result error)))


(defmethod action-handler ((widget fb-post-comment-form))
  (when (string-equal (parameter "action") "post-facebook-comment")  
  
    (multiple-value-bind (result error-message)
        (comment-facebook (parameter "post-id")
                          (parameter "user-id")
                          (parameter "comment"))
      (if error-message
          (setf (get-val widget 'message) error-message)
          (defer-js (format nil "$('#~a').dialog('close')" (name widget)))))
    ))



(defmethod handle-action ((grid generic-grid) (action (eql 'facebook-comment)))

  (setf (action-widget grid)
        (make-widget 'fb-post-comment-form 
                     :grid grid 
                     :name "facebook-comment-action-form")))


