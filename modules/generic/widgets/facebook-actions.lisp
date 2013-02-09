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


(defmethod action-handler ((widget fb-post-comment-form))
  (when (string-equal (parameter "action") "post-facebook-comment")  
    (let* ((user (get-channel-user-by-user-id 
                  (parameter "user-id")
                  "Facebook"))
           (action (add-generic-action 
                    user
                    (parameter "post-id") 
                    "Facebook"
                    (parameter "user-id")
                    nil
                    "Comment"
                    (parameter "comment")
                    "Immediate"
                    (get-universal-time))))

      (multiple-value-bind (result error-message)
          (comment-facebook
           user
           (parameter "post-id")
           (parameter "comment"))
        (handle-generic-action 
         widget
         action
         result
         error-message)))))

(defmethod handle-action ((grid generic-grid) (action (eql 'facebook-comment)))

  (setf (action-widget grid)
        (make-widget 'fb-post-comment-form 
                     :grid grid 
                     :name "facebook-comment-action-form")))

(defclass fb-like-post-form (ajax-widget)
  ((grid :initarg :grid
         :initform nil
         :accessor grid)
   (message :initarg :message))
  (:metaclass widget-class))


(defmethod render ((widget fb-like-post-form) &key )
  (let* ((like-form (make-widget 'html-simple-framework-form 
                                    :name "facebook-like-post-form"
                                    :grid-size 6
                                    :form-id "facebook-like-post-form"
                                    :action "post-facebook-like"
                                    :action-title "Like"))
         (form-section (make-widget 'form-section
                                    :name "form-section"))
         
         (current-post (set-current-row (get-val widget 'grid)))
         (post-id (gpv current-post :id)))
    (setf (get-val like-form 'grid-size) 2)

    (with-html 
      (when (parameter "action")
          (render like-form
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
                                :type :select))))))
          (str (get-val widget 'message))))
    (open-dialog widget (grid widget) :width 500 :height 260)))

(defmethod handle-action ((grid generic-grid) (action (eql 'post-facebook-like)))

  (setf (action-widget grid)
        (make-widget 'fb-like-post-form 
                     :grid grid 
                     :name "facebook-like-action-form")))

(defmethod action-handler ((widget fb-like-post-form))
  (when (string-equal (parameter "action") "post-facebook-like")  
    (setf (get-val widget 'message) nil)
    (let* ((user (get-channel-user-by-user-id 
                  (parameter "user-id")
                  "Facebook"))
           (action (add-generic-action 
                   user
                   (parameter "post-id") 
                    "Facebook"
                    (parameter "user-id")
                    nil
                    "Like"
                    t
                    "Immediate"
                    (get-universal-time))))
          (multiple-value-bind (result error-message)
              (facebook-like user
                             (parameter "post-id"))

            (handle-generic-action 
             widget
             action
             result
             error-message)))))