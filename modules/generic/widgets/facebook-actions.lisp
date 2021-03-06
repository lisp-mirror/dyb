(in-package :dyb)

(defclass fb-post-comment-form (ajax-widget)
  ((grid :initarg :grid
         :initform nil
         :accessor grid)
   (reply-id :initarg :reply-id
         :initform nil
         :accessor reply-id)
   (message :initarg :message
            :accessor message))
  (:metaclass widget-class))


(defmethod render ((widget fb-post-comment-form) &key)
  (let* ((comment-form (make-widget 'html-simple-framework-form 
                                    :name "facebook-post-comment-form"
                                    :grid-size 12
                                    :form-id "facebook-post-comment-form"
                                    :action "post-facebook-comment"
                                    :action-title "Comment"
                                    :ajax-render-widget (editor (grid widget))))
         (form-section (make-widget 'form-section
                                    :name "form-section"))
         
         (current-post (set-current-row (get-val widget 'grid)))
         
         (post-id (or (reply-id widget) (gpv current-post :id))))

   ;; (break "~A" (reply-id widget))
    (with-html 
      (when (parameter "action")
        (render comment-form
                :content
                (with-html-string
                  (:div 
                   (:input :type "hidden" :name "post-id" 
                           :value post-id)

                   (render form-section 
                           :label "As User"
                           :input 
                           (with-html-string
                             (render-edit-field
                              "user-id" 
                              (parameter "user-id")
                              :data (get-channel-users-list "Facebook" nil)
                              :required t
                              :type :select)))

                   (render form-section 
                           :label "Comment"
                           :input 
                           (with-html-string
                             (render-edit-field
                              "comment" 
                              (parameter "comment")
                              :required t
                              :type :textarea))))))
        (esc (get-val widget 'message))))))

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
         error-message))
      (facebook-refresh-context-feeds)
      )))

(defmethod handle-action ((grid generic-grid) (action (eql :facebook-comment)))

  (setf (action-widget grid)
        (make-widget 'fb-post-comment-form 
                     :grid grid 
                     :name "facebook-comment-action-form")))

(defmethod handle-action ((grid generic-grid) (action (eql :facebook-comment-reply)))
  (let ((form (make-widget 'fb-post-comment-form 
                           :grid grid 
                           :name "facebook-comment-action-form"
                           :reply-id (parameter "comment-id")
                           )))
    ;;(break "~a" form)
    (setf (reply-id form) (parameter "comment-id"))
    (setf (action-widget grid) form
          )))

(defclass fb-like-post-form (ajax-widget)
  ((grid :initarg :grid
         :initform nil
         :accessor grid)
   (like-id :initarg :like-id 
         :initform nil
         :accessor like-id)
   (message :initarg :message
            :accessor message))
  (:metaclass widget-class))


(defmethod render ((widget fb-like-post-form) &key )
  (let* ((like-form (make-widget 'html-simple-framework-form 
                                 :name "facebook-like-post-form"
                                 :grid-size 6
                                 :form-id "facebook-like-post-form"
                                 :action "do-post-facebook-like"
                                 :action-title "Like"
                                 :ajax-render-widget (editor (grid widget))))
         (form-section (make-widget 'form-section
                                    :name "form-section"))
         
         (current-post (set-current-row (get-val widget 'grid)))
         (post-id (or (like-id widget) (gpv current-post :id))))
    (setf (get-val like-form 'grid-size) 2)

    (with-html 
      (when (parameter "action")
        (render like-form
                :content
                (with-html-string
                  (:div 
                   (:input :type "hidden" :name "post-id" 
                           :value post-id)

                   (render form-section 
                           :label "As User"
                           :input 
                           (with-html-string
                             (render-edit-field
                              "user-id" 
                              (parameter "user-id")
                              :data (get-channel-users-list "Facebook" nil)
                              :required t
                              :type :select))))))
        (str (get-val widget 'message))))))

(defmethod handle-action ((grid generic-grid) (action (eql :post-facebook-like)))
  (setf (action-widget grid) (make-widget 'fb-like-post-form 
                     :grid grid 
                     :name "facebook-like-action-form")))


(defmethod handle-action ((grid generic-grid) (action (eql :post-facebook-comment-like)))
  (let ((form (make-widget 'fb-like-post-form 
                     :grid grid 
                     :name "facebook-like-action-form")))
    (setf (like-id form) (parameter "comment-id"))
    (setf (action-widget grid) form)))


(defmethod action-handler ((widget fb-like-post-form))
  (when (string-equal (parameter "action") "do-post-facebook-like")
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
