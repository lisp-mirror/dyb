(in-package :dyb)

(defclass linkedin-like-form (ajax-widget)
  ((grid :initarg :grid
         :initform nil
         :accessor grid)
   (message :initarg :message))
  (:metaclass widget-class))

(defmethod render ((widget linkedin-like-form) &key)
  (let* ((like-form (make-widget 'html-simple-framework-form 
                                    :name "facebook-like-post-form"
                                    :grid-size 6
                                    :form-id "facebook-like-post-form"
                                    :action "like-linkedin"
                                    :action-title "like"
                                    :ajax-render-widget (editor (grid widget))))
         (form-section (make-widget 'form-section
                                    :name "form-section"))
         
         (current-post (set-current-row (get-val widget 'grid)))
         (linkedin-update-id (gpv current-post :id)))
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
                                :type :select))))))
          (str (get-val widget 'message))))))

(defmethod handle-action ((grid generic-grid) (action (eql 'like-linkedin-form)))
  (setf (action-widget grid)
        (make-widget 'linkedin-like-form 
                     :grid grid 
                     :name "linkedin-like-action-form")))

(defmethod action-handler ((widget linkedin-like-form))
   (when (string-equal (parameter "action") "like-linkedin")  
     (let* ((user (get-channel-user-by-user-id 
                   (parameter "user-id")
                   "LinkedIn"))
            (action (add-generic-action 
                     user
                     nil 
                     "LinkedIn"
                     (parameter "user-id")
                     nil
                     "Like"
                     t
                     "Immediate"
                     (get-universal-time))))
       (multiple-value-bind (result error-message)
           (like-linkedin user
                          (parameter "linkedin-update-id"))
        (handle-generic-action 
         widget
         action
         result
         error-message)))))

(defclass linkedin-comment-form (ajax-widget)
  ((grid :initarg :grid
         :initform nil
         :accessor grid)
   (message :initarg :message))
  (:metaclass widget-class))

(defmethod render ((widget linkedin-comment-form) &key )
  (let* ((like-form (make-widget 'html-simple-framework-form 
                                 :name "linkedin-comment-post-form"
                                 :grid-size 6
                                 :form-id "linkedin-comment-post-form"
                                 :action "comment-linkedin"
                                 :action-title "Comment"
                                 :ajax-render-widget (editor (grid widget))))
         (form-section (make-widget 'form-section
                                    :name "form-section"))
         
         (current-post (set-current-row (get-val widget 'grid)))
         (linkedin-update-id (gpv current-post :id)))
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
                   (render form-section 
                           :label "Comment"
                           :input 
                           (with-html-to-string ()
                             (render-edit-field
                              "message" 
                              (parameter "message")
                              :required t
                              :type :textarea))))))
        (str (get-val widget 'message))))))

(defmethod handle-action ((grid generic-grid) (action (eql 'comment-linkedin-form)))
  (setf (action-widget grid)
        (make-widget 'linkedin-comment-form 
                     :grid grid 
                     :name "linkedin-comment-action-form")))

(defmethod action-handler ((widget linkedin-comment-form))
  (setf (get-val widget 'message) nil)

  (when (string-equal (parameter "action") "comment-linkedin")  
    (setf (get-val widget 'message) nil)
    
    (let* ((user (get-channel-user-by-user-id 
                  (parameter "user-id")
                  "LinkedIn"))
          (action (add-generic-action 
                   user
                   nil 
                   "LinkedIn"
                   (parameter "user-id")
                   nil
                   "Comment"
                   t
                   "Immediate"
                   (get-universal-time))))
      (multiple-value-bind (result error-message)
          (comment-linkedin  user
                            (parameter "linkedin-update-id")
                            (parameter "message"))
        (handle-generic-action 
         widget
         action
         result
         error-message)))))
