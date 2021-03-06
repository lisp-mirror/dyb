(in-package :dyb)

(defclass twitter-retweet-form (ajax-widget)
  ((grid :initarg :grid
         :initform nil
         :accessor grid)
   (message :initarg :message 
            :accessor message))
  (:metaclass widget-class))


(defmethod render ((widget twitter-retweet-form) &key )
  (let* ((like-form (make-widget 'html-simple-framework-form
                                 :name "facebook-retweet-post-form"
                                 :grid-size 6
                                 :form-id "facebook-retweet-post-form"
                                 :action "retweet-twitter"
                                 :action-title "Retweet"
                                 :ajax-render-widget (editor (grid widget))))
         (form-section (make-widget 'form-section
                                    :name "form-section"))
         (current-post (set-current-row (grid widget)))
         (tweet-id (gpv current-post :id)))
    (setf (get-val like-form 'grid-size) 2)

    (with-html
      (when (parameter "action")
        (render like-form
                :content
                (with-html-string
                  (:div
                   (:input :type "hidden" :name "tweet-id"
                           :value tweet-id)
                   ;;TODO: handle ' intext
                   (:input :type "hidden" :name "tweet-text"
                           :value (str (gpv current-post :text)))

                   (render form-section
                           :label "As User"
                           :input
                           (with-html-string
                             (render-edit-field
                              "user-id"
                              (parameter "user-id")
                              :data (get-channel-users-list "Twitter" nil)
                              :required t
                              :type :select))))))
        (str (get-val widget 'message))))))

(defmethod handle-action ((grid generic-grid) (action (eql :retweet-twitter-form)))
  (setf (action-widget grid)
        (make-widget 'twitter-retweet-form
                     :grid grid
                     :name "twitter-retweet-action-form")))

(defmethod action-handler ((widget twitter-retweet-form))
  (when (string-equal (parameter "action") "retweet-twitter")

    (let* ((user (get-channel-user-by-user-id
                  (parameter "user-id")
                  "Twitter"))
          (action (add-generic-action
                   user
                    nil
                    "Twitter"
                    (parameter "user-id")
                    nil
                    "Retweet"
                    (parameter "tweet-text")
                    "Immediate"
                    (get-universal-time))))
      (multiple-value-bind (result error-message)
          (retweet user
                   (parameter "tweet-id"))
        (handle-generic-action
         widget
         action
         result
         error-message)))))

(defclass twitter-reply-form (ajax-widget)
  ((grid :initarg :grid
         :initform nil
         :accessor grid)
   (message :initarg :message
            :initform nil
            :accessor message))
  (:metaclass widget-class))

(defmethod render ((widget twitter-reply-form) &key)
  (let* ((like-form (make-widget 'html-simple-framework-form
                                 :name "twitter-like-post-form"
                                 :grid-size 6
                                 :form-id "twitter-like-post-form"
                                 :action "reply-twitter"
                                 :action-title "Reply"
                                 :ajax-render-widget (editor (grid widget))))
         (form-section (make-widget 'form-section
                                    :name "form-section"))
         (current-post (set-current-row (grid widget)))
         (at-user (gpv current-post :user :screen--name))
         (in-reply-to-status-id (gpv current-post :id))
         (shorified-message (let ((*site-url* "http://dxw.co.za/"))
                              (shortify-string (parameter "message")))))

    (setf (get-val like-form 'grid-size) 2)
    (with-html
      (render like-form
              :content
              (with-html-string
                (esc (message widget))
                (:div
                 (:input :type "hidden" :name "at-user"
                         :value at-user)
                 (:input :type "hidden" :name "in-reply-to-status-id"
                         :value in-reply-to-status-id)
                 (render form-section
                         :label "As User"
                         :input
                         (with-html-string
                           (render-edit-field
                            "user-id"
                            (or (parameter "user-id")
                                (first (first (get-channel-users-list "Twitter" nil))))
                            :data (get-channel-users-list "Twitter" nil)
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
                            :type :textarea)
                           (:div "Characters:"
                                 (:span :id "message-length"
                                        (str (length shorified-message))))))
                 (render form-section 
                         :label "Processed"
                         :input 
                         (with-html-string
                           (:textarea
                            :style (format nil "width:~A;" "300px")
                            :class nil
                            :disabled t
                            :id "processed-content"
                            :cols 85 :rows 5
                            (esc shorified-message))))
                 (defer-js
                     "$('[name=\"message\"]').bind('change input propertychange',
function() {
var s = shortifyString($('[name=\"message\"]').val());
var length = s.length;
$('#message-length').text(length);
$('#processed-content').text(s)})")
                 (if (get-val widget 'message)
                     (htm (:span :style "color:red;" ;
                                 (get-val widget 'message))))))))))

(defmethod handle-action ((grid generic-grid) (action (eql :reply-twitter-form)))
  (setf (action-widget grid)
        (make-widget 'twitter-reply-form
                     :grid grid
                     :name "twitter-reply-action-form")))

(defmethod action-handler ((widget twitter-reply-form))
  (with-parameters (action user-id at-user message
                           in-reply-to-status-id)
    (when (string-equal action "reply-twitter")
      (let* ((grid (grid widget))
             (user (get-channel-user-by-user-id
                    user-id
                    "Twitter"))
             (message (shortify-string message)))
        (if (> (length message) 140)
            (setf (error-message grid)
                  (frmt "Message too long - ~a." (length message)))
            (multiple-value-bind (result error-message)
                (tweet-reply user
                             message
                             at-user
                             in-reply-to-status-id)
              (handle-generic-action
               widget
               (add-generic-action user in-reply-to-status-id
                                   "Twitter" user-id nil
                                   "Reply" message "Immediaaste"
                                   (get-universal-time))
               result
               error-message)))))))

(defclass twitter-favourite-form (ajax-widget)
  ((grid :initarg :grid
         :initform nil
         :accessor grid)
   (message :initarg :message 
            :accessor message))
  (:metaclass widget-class))

(defmethod render ((widget twitter-favourite-form) &key )
  (let* ((like-form (make-widget 'html-simple-framework-form
                                 :name "facebook-favourite-post-form"
                                 :grid-size 6
                                 :form-id "facebook-favourite-post-form"
                                 :action "favourite-twitter"
                                 :action-title "favourite"
                                 :ajax-render-widget (editor (grid widget))))
         (form-section (make-widget 'form-section
                                    :name "form-section"))

         (current-post (set-current-row (get-val widget 'grid)))
         (tweet-id (gpv current-post :id)))
    (setf (get-val like-form 'grid-size) 2)
    (with-html
      (when (parameter "action")
        (render like-form
                :content
                (with-html-string
                  (:div
                   (:input :type "hidden" :name "tweet-id"
                           :value tweet-id)

                   (render form-section
                           :label "As User"
                           :input
                           (with-html-string
                             (render-edit-field
                              "user-id"
                              (parameter "user-id")
                              :data (get-channel-users-list "Twitter" nil)
                              :required t
                              :type :select))))))
        (str (get-val widget 'message))))))

(defmethod handle-action ((grid generic-grid) (action (eql :favourite-twitter-form)))
  (setf (action-widget grid)
        (make-widget 'twitter-favourite-form
                     :grid grid
                     :name "twitter-favourite-action-form")))

(defmethod action-handler ((widget twitter-favourite-form))
  (setf (get-val widget 'message) nil)

  (when (string-equal (parameter "action") "favourite-twitter")
    (setf (get-val widget 'message) nil)

    (let* ((user (get-channel-user-by-user-id
                  (parameter "user-id")
                  "Twitter"))
          (action (add-generic-action
                   user
                   (parameter "tweet-id")
                   "Twitter"
                   (parameter "user-id")
                   nil
                   "Favourite"
                   t
                   "Immediate"
                   (get-universal-time))))
      (multiple-value-bind (result error-message)
          (twitter-favourite user
                             (parameter "tweet-id"))
        (handle-generic-action
         widget
         action
         result
         error-message)))))
