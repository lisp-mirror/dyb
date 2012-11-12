(in-package :dyb)


(defun like-linkedin (user-id linkedin-update-id)
  (when (and user-id linkedin-update-id)
    (let* ((result)
           (error)
           (user (get-channel-user-by-user-id  (parse-integer  user-id)))
           (channel (if user (get-social-channel (get-val user 'channel-user-type)))) )

      
      (when (and user channel)

        (when (get-val user 'last-access-token)
          (setf result
                (linkedin-like 
                 (get-val channel 'app-id)
                 (get-val channel 'app-secret)
                 (get-val user 'last-access-token)
                 (get-val user 'last-token-secret)
                 linkedin-update-id))
          
          (setf result (json::decode-json-from-string  (babel:octets-to-string result)))
 
          (when (assoc-path result :errors)
            (setf result nil)
            (setf error (cdr (assoc-path result :errors :message))))))
      (values result error))))

(defclass linkedin-like-form (ajax-widget)
  ((grid :initarg :grid
         :initform nil
         :accessor grid)
   (message :initarg :message))
  (:metaclass widget-class))


(defmethod render ((widget linkedin-like-form) &key )
  (let* ((like-form (make-widget 'html-simple-framework-form 
                                    :name "facebook-like-post-form"
                                    :grid-size 6
                                    :form-id "facebook-like-post-form"
                                    :action "like-linkedin"
                                    :action-title "like"))
         (form-section (make-widget 'form-section
                                    :name "form-section"))
         
         (current-post (set-current-row (get-val widget 'grid)))
         (linkedin-update-id (gpv current-post :id))
         )
    (setf (get-val like-form 'grid-size) 2)
    
    (with-html 
      (when (parameter "action")
          (render like-form
                  :content
                  (with-html-to-string ()
                    (:div 
                     (:input :type "hidden" :name "linkedin-update-id" 
                             :value linkedin-update-id)

                     (render form-section 
                             :label "As User"
                             :input 
                             (with-html-to-string ()
                               (render-edit-field
                                "user-id" 
                                (parameter "user-id")
                                :data (get-channel-users-list "Linkedin" nil)
                                :required t
                                :type :select)))

                     )))
          (str (get-val widget 'message))))
    (open-dialog widget (grid widget) :width 500 :height 260)))

(defmethod handle-action ((grid generic-grid) (action (eql 'like-linkedin-form)))

  (setf (action-widget grid)
        (make-widget 'linkedin-like-form 
                     :grid grid 
                     :name "linkedin-like-action-form")))

(defmethod action-handler ((widget linkedin-like-form))
  (setf (get-val widget 'message) nil)

  (when (string-equal (parameter "action") "like-linkedin")  
    (setf (get-val widget 'message) nil)
    

    (multiple-value-bind (result error-message)
        (like-linkedin (parameter "user-id")
                         (parameter "linkedin-update-id"))

      (if error-message
          (setf (get-val widget 'message) error-message)
          (defer-js (format nil "$('#~a').dialog('close')" (name widget)))))
    ))


(defun comment-linkein (user-id message at-user)
  (when (and user-id at-user)
    (let* ((result)
           (error)
           (user (get-channel-user-by-user-id  (parse-integer  user-id)))
           (channel (if user (get-social-channel (get-val user 'channel-user-type)))) )

      
      (when (and user channel)

        (when (get-val user 'last-access-token)
          (setf result
                (comment-tweet 
                 (get-val channel 'app-id)
                 (get-val channel 'app-secret)
                 (get-val user 'last-access-token)
                 (get-val user 'last-token-secret)
                 message
                 at-user))
          
          (setf result (json::decode-json-from-string  (babel:octets-to-string result)))
 
          (when (assoc-path result :errors)
            (setf result nil)
            (setf error (cdr (assoc-path result :errors :message))))))
      (values result error))))



(defclass linkein-comment-form (ajax-widget)
  ((grid :initarg :grid
         :initform nil
         :accessor grid)
   (message :initarg :message))
  (:metaclass widget-class))


(defmethod render ((widget linkein-comment-form) &key )
  (let* ((like-form (make-widget 'html-simple-framework-form 
                                    :name "linkein-comment-post-form"
                                    :grid-size 6
                                    :form-id "linkein-comment-post-form"
                                    :action "comment-linkein"
                                    :action-title "Comment"))
         (form-section (make-widget 'form-section
                                    :name "form-section"))
         
         (current-post (set-current-row (get-val widget 'grid)))
         (at-user (gpv current-post :user :screen--name))
         )
    (setf (get-val like-form 'grid-size) 2)
    
    (with-html 
      (when (parameter "action")
          (render like-form
                  :content
                  (with-html-to-string ()
                    (:div 
                     (:input :type "hidden" :name "at-user" 
                             :value at-user)
                     

                     (render form-section 
                             :label "As User"
                             :input 
                             (with-html-to-string ()
                               (render-edit-field
                                "user-id" 
                                (parameter "user-id")
                                :data (get-channel-users-list "Linkein" nil)
                                :required t
                                :type :select)))
                     (render form-section 
                               :label "Comment"
                               :input 
                               (with-html-to-string ()
                                 (render-edit-field
                                  "message" 
                                  (parameter "message")
                                  :required t
                                  :type :textarea)))

                     )))
          (str (get-val widget 'message))))
    (open-dialog widget (grid widget) :width 600 :height 460)))

(defmethod handle-action ((grid generic-grid) (action (eql 'comment-linkein-form)))

  (setf (action-widget grid)
        (make-widget 'linkein-comment-form 
                     :grid grid 
                     :name "linkein-comment-action-form")))

(defmethod action-handler ((widget linkein-comment-form))
  (setf (get-val widget 'message) nil)

  (when (string-equal (parameter "action") "comment-linkein")  
    (setf (get-val widget 'message) nil)
    

    (multiple-value-bind (result error-message)
        (comment-linkein (parameter "user-id")
                       
                       (parameter "message")
                       (parameter "at-user"))


      (if error-message
          (setf (get-val widget 'message) error-message)
          (defer-js (format nil "$('#~a').dialog('close')" (name widget)))))
    ))
