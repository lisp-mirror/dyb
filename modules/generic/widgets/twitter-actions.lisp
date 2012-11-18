(in-package :dyb)

(defun retweet-twitter (user-id tweet-id)
  (when (and user-id tweet-id)
    (let* ((result)
           (error)
           (user (get-channel-user-by-user-id  (parse-integer  user-id)))
           (channel (if user (get-social-channel (get-val user 'channel-user-type)))) )

      
      (when (and user channel)

        (when (get-val user 'last-access-token)
          (setf result
                (retweet 
                 (get-val channel 'app-id)
                 (get-val channel 'app-secret)
                 (get-val user 'last-access-token)
                 (get-val user 'last-token-secret)
                 tweet-id))
          
          (setf result (json::decode-json-from-string  (babel:octets-to-string result)))
 
          (when (assoc-path result :errors)
            (setf result nil)
            (setf error (cdr (assoc-path result :errors :message))))))
      (values result error))))

(defclass twitter-retweet-form (ajax-widget)
  ((grid :initarg :grid
         :initform nil
         :accessor grid)
   (message :initarg :message))
  (:metaclass widget-class))


(defmethod render ((widget twitter-retweet-form) &key )
  (let* ((like-form (make-widget 'html-simple-framework-form 
                                    :name "facebook-retweet-post-form"
                                    :grid-size 6
                                    :form-id "facebook-retweet-post-form"
                                    :action "retweet-twitter"
                                    :action-title "Retweet"))
         (form-section (make-widget 'form-section
                                    :name "form-section"))
         
         (current-post (set-current-row (get-val widget 'grid)))
         (tweet-id (gpv current-post :id))
         )
    (setf (get-val like-form 'grid-size) 2)
    
    (with-html 
      (when (parameter "action")
          (render like-form
                  :content
                  (with-html-to-string ()
                    (:div 
                     (:input :type "hidden" :name "tweet-id" 
                             :value tweet-id)

                     (render form-section 
                             :label "As User"
                             :input 
                             (with-html-to-string ()
                               (render-edit-field
                                "user-id" 
                                (parameter "user-id")
                                :data (get-channel-users-list "Twitter" nil)
                                :required t
                                :type :select)))

                     )))
          (str (get-val widget 'message))))
    (open-dialog widget (grid widget) :width 500 :height 260)))

(defmethod handle-action ((grid generic-grid) (action (eql 'retweet-twitter-form)))

  (setf (action-widget grid)
        (make-widget 'twitter-retweet-form 
                     :grid grid 
                     :name "twitter-retweet-action-form")))

(defmethod action-handler ((widget twitter-retweet-form))
  (setf (get-val widget 'message) nil)

  (when (string-equal (parameter "action") "retweet-twitter")  
    (setf (get-val widget 'message) nil)
    
    (let ((action (generic-action 
                    nil 
                    "Twitter"
                    (parameter "user-id")
                    nil
                    "Retweet"
                    t
                    "Immediate"
                    (get-universal-time))))
      (multiple-value-bind (result error-message)
          (retweet-twitter (parameter "user-id")
                           (parameter "tweet-id"))

        (when error-message
          (setf (get-val widget 'message) error-message)
          (generic-action-log action 
                              "Error"
                              error-message
                              "Pending"))
        (unless error-message
          (generic-action-log action 
                              "Result"
                              result
                              "Completed")
          (defer-js (format nil "$('#~a').dialog('close')" (name widget))))

        ))
    ))

(defun reply-twitter (user-id message at-user)
  (when (and user-id at-user)
    (let* ((result)
           (error)
           (user (get-channel-user-by-user-id  (parse-integer  user-id)))
           (channel (if user (get-social-channel (get-val user 'channel-user-type)))) )

      
      (when (and user channel)
        (when (get-val user 'last-access-token)
          (setf result
                (reply-tweet 
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

(defclass twitter-reply-form (ajax-widget)
  ((grid :initarg :grid
         :initform nil
         :accessor grid)
   (message :initarg :message))
  (:metaclass widget-class))


(defmethod render ((widget twitter-reply-form) &key )
  (let* ((like-form (make-widget 'html-simple-framework-form 
                                    :name "twitter-like-post-form"
                                    :grid-size 6
                                    :form-id "twitter-like-post-form"
                                    :action "reply-twitter"
                                    :action-title "Reply"))
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
                                :data (get-channel-users-list "Twitter" nil)
                                :required t
                                :type :select)))
                     (render form-section 
                               :label "Message"
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

(defmethod handle-action ((grid generic-grid) (action (eql 'reply-twitter-form)))

  (setf (action-widget grid)
        (make-widget 'twitter-reply-form 
                     :grid grid 
                     :name "twitter-reply-action-form")))

(defmethod action-handler ((widget twitter-reply-form))
  (setf (get-val widget 'message) nil)

  (when (string-equal (parameter "action") "reply-twitter")  
    (setf (get-val widget 'message) nil)
    
    (let ((action (generic-action 
                    nil 
                    "Twitter"
                    (parameter "user-id")
                    nil
                    "Reply"
                    t
                    "Immediate"
                    (get-universal-time))))
      (multiple-value-bind (result error-message)
          (reply-twitter (parameter "user-id")
                       
                       (parameter "message")
                       (parameter "at-user"))

        (when error-message
          (setf (get-val widget 'message) error-message)
          (generic-action-log action 
                              "Error"
                              error-message
                              "Pending"))
        (unless error-message
          (generic-action-log action 
                              "Result"
                              result
                              "Completed")
          (defer-js (format nil "$('#~a').dialog('close')" (name widget))))))))




(defun favourite-twitter (user-id tweet-id)
  (when (and user-id tweet-id)
    (let* ((result)
           (error)
           (user (get-channel-user-by-user-id  (parse-integer  user-id)))
           (channel (if user (get-social-channel (get-val user 'channel-user-type)))) )

      
      (when (and user channel)

        (when (get-val user 'last-access-token)
          (setf result
                (twitter-favourite 
                 (get-val channel 'app-id)
                 (get-val channel 'app-secret)
                 (get-val user 'last-access-token)
                 (get-val user 'last-token-secret)
                 tweet-id))
          
          (setf result (json::decode-json-from-string  (babel:octets-to-string result)))
 
          (when (assoc-path result :errors)
            (setf result nil)
            (setf error (cdr (assoc-path result :errors :message))))))
      (values result error))))

(defclass twitter-favourite-form (ajax-widget)
  ((grid :initarg :grid
         :initform nil
         :accessor grid)
   (message :initarg :message))
  (:metaclass widget-class))


(defmethod render ((widget twitter-favourite-form) &key )
  (let* ((like-form (make-widget 'html-simple-framework-form 
                                    :name "facebook-favourite-post-form"
                                    :grid-size 6
                                    :form-id "facebook-favourite-post-form"
                                    :action "favourite-twitter"
                                    :action-title "favourite"))
         (form-section (make-widget 'form-section
                                    :name "form-section"))
         
         (current-post (set-current-row (get-val widget 'grid)))
         (tweet-id (gpv current-post :id))
         )
    (setf (get-val like-form 'grid-size) 2)
    
    (with-html 
      (when (parameter "action")
          (render like-form
                  :content
                  (with-html-to-string ()
                    (:div 
                     (:input :type "hidden" :name "tweet-id" 
                             :value tweet-id)

                     (render form-section 
                             :label "As User"
                             :input 
                             (with-html-to-string ()
                               (render-edit-field
                                "user-id" 
                                (parameter "user-id")
                                :data (get-channel-users-list "Twitter" nil)
                                :required t
                                :type :select)))

                     )))
          (str (get-val widget 'message))))
    (open-dialog widget (grid widget) :width 500 :height 260)))

(defmethod handle-action ((grid generic-grid) (action (eql 'favourite-twitter-form)))

  (setf (action-widget grid)
        (make-widget 'twitter-favourite-form 
                     :grid grid 
                     :name "twitter-favourite-action-form")))

(defmethod action-handler ((widget twitter-favourite-form))
  (setf (get-val widget 'message) nil)

  (when (string-equal (parameter "action") "favourite-twitter")  
    (setf (get-val widget 'message) nil)
    
    (let ((action (generic-action 
                    (parameter "tweet-id") 
                    "Twitter"
                    (parameter "user-id")
                    nil
                    "Favourite"
                    t
                    "Immediate"
                    (get-universal-time))))
      (multiple-value-bind (result error-message)
          (favourite-twitter (parameter "user-id")
                         (parameter "tweet-id"))

        (when error-message
          (setf (get-val widget 'message) error-message)
          (generic-action-log action 
                              "Error"
                              error-message
                              "Pending"))
        (unless error-message
          (generic-action-log action 
                              "Result"
                              result
                              "Completed")
          (defer-js (format nil "$('#~a').dialog('close')" (name widget))))))
    
    ))