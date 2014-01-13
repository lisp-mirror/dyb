(in-package :dyb)

(defclass direct-message-action (grid-editor)
  ((message :initarg :message 
            :accessor message))
  (:metaclass widget-class))

(defmethod render ((widget direct-message-action) &key)
  (let ((row (editing-row (grid widget)))
        (direct-message-form (make-widget 'html-simple-framework-form
                                  :name "direct-message-form"
                                  :grid-size 12
                                  :form-id "direct-message-form"
                                  :action "direct-message"
                                  :action-title "Assign Task"))
        (form-section (make-widget 'form-section
                                   :name "form-section")))
;;(break "~A" row)

    (setf (ajax-render-widget direct-message-form) (editor (grid widget)))
    (render direct-message-form
            :content
            (with-html-string
                    (:div 
                     (:input :type "hidden" :name "post-id" 
                             :value (cond ((string-equal (get-val row 'post-type) 
                                                         "Facebook")
                                           (raw-post-id row 'facebook))
                                          ((string-equal (get-val row 'post-type) 
                                                         "Twitter")
                                           (raw-post-id row 'twitter))
                                          ((string-equal (get-val row 'post-type) 
                                                         "LinkedIn")
                                           (raw-post-id row 'linkedin))))

                     (render form-section 
                             :label "As User"
                             :input 
                             (with-html-string
                               (render-edit-field
                                "user-id" 
                                (parameter "user-id")
                                :data (get-channel-users-list (get-val row 'post-type) nil)
                                :required t
                                :type :select)))

                     (render form-section 
                             :label "Message"
                             :input 
                             (with-html-string
                               (render-edit-field
                                "message" 
                                (parameter "message")
                                :required t
                                :type :textarea))))))))

(defmethod handle-action ((grid generic-grid) (action (eql :direct-message-form)))
  
  (setf (action-widget grid)
        (make-widget 'direct-message-action :name "direct-message-action-form"
                                         :grid grid)))

(defmethod action-handler ((widget direct-message-action))
  (when (equalp (parameter "action") "direct-message")
    (let* ((grid (grid widget))
           (current-post (editing-row grid))
           (user (get-val current-post 'channel-user))
           (action (add-generic-action 
                    user
                    (parameter "post-id") 
                    (get-val current-post 'post-type)
                    (get-val user 'user-id)
                    nil
                    "Direct Message"
                    (parameter "message")
                    "Immediate"
                    (get-universal-time))))
;;(break "~A" current-post)
      (multiple-value-bind (result error-message)
          (cond ((string-equal (get-val current-post 'post-type) "Facebook")
                 (let ((to-user (post-facebook-user-profile-no-auth 
                                 (gpv current-post :from :id))))
                   (if (gpv to-user :username)
                       (cl-smtp:send-email "mail.digyourbrand.co.za"  
                                           "app@digyourbrand.co.za" 
                                           (list 
                                            (format nil "~A@facebook.com" 
                                                    (gpv to-user :username))
                                            (email (current-user))) 
                                           "Direct Message" 
                                           (parameter "message") 
                                           :ssl t :port 587
                                           :authentication '(:login "app@digyourbrand.co.za"
                                                             "d@t@xw@r3")))))
                ((string-equal (get-val current-post 'post-type) "Twitter")
                 (tweet-reply user
                         (parameter "message")
                         (gpv current-post :user :screen--name)))
                 ((string-equal (get-val current-post 'post-type) "LinkedIn")
                  ;;TODO: Implement for personal
                  ))
        (handle-generic-action 
         widget
         action
         result
         error-message)))))
