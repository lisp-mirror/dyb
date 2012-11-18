(in-package :dyb)

(defclass generic-grid (grid)
  ()
  (:metaclass widget-class)
  (:include-css "/appcss/posts.css")
  (:default-initargs :edit-inline nil))

(defclass grid-action (ajax-widget)
  ((grid :initarg :grid
         :initform nil
         :accessor grid))
  (:metaclass widget-class))

(defmethod render ((widget grid-action) &key )
  (with-html
    "Grid Action"))

(defmethod handle-action ((grid generic-grid) (action (eql 'block-user)))
  (setf (action-widget grid)
        (make-widget 'grid-action :grid grid :name "XXX")))

(defun get-generic-data (grid &key filter search)
  (declare (ignore grid search))
  (find-docs 'vector
              (lambda (doc)
                (if (match-context-entities  (channel-user doc) )
                   (cond ((equal filter 'with-audit-data)
                          doc)
                         (t
                          (if (not (string-equal (get-val doc 'doc-status) "superseded"))
                              doc)))))
              (generic-post-collection)))

(defmethod get-rows ((grid generic-grid))
  (setf (rows grid)
	(get-generic-data grid
                          :filter (grid-filter grid)
                          :search (search-term grid))))

(defclass post-form (grid-editor)
  ((message :initarg :message))
  (:metaclass widget-class))

(defmethod handle-action ((grid generic-grid) (action (eql 'new)))
  (setf (action-widget grid)
        (make-widget 'post-form
                     :grid grid
                     :name "post-action-form")
        (editing-row grid) :new))

(defmethod render ((widget post-form) &key)
  (let* ((comment-form (make-widget 'html-simple-framework-form
                                    :name "post-form"
                                    :grid-size 12
                                    :form-id "post-form"
                                    :action "post-to-channel"
                                    :action-title "Post"))
         (form-section (make-widget 'form-section
                                    :name "form-section"))
         (channel-users (make-widget 'channel-user-select
                                    :name "channel-user-select-dropown")))
    (setf (ajax-render-widget comment-form) (editor (grid widget)))
    (render comment-form
            :content
            (with-html-string
              (:div
               (destructuring-bind (service channel-user)
                   (selects channel-users)
                 (render channel-users)

                 (setf (value service) (parameter "service"))
                 (setf (value channel-user) (parameter "channel-user"))

                 (render form-section
                         :label "Post To Channel"
                         :input
                         (with-html-string
                           (render service)))
                 (render form-section
                         :label "Channel User"
                         :input
                         (with-html-string
                           (render channel-user)))

                 (render form-section
                         :label "Post"
                         :input
                         (with-html-string
                           (render-edit-field
                            "post-status"
                            (parameter "post-status")
                            :required t
                            :type :textarea))))
               (if (get-val widget 'message)
                   (htm (:span :style "color:red;"
                               (get-val widget 'message)))))))))

(defun post-facebook (user-id message)

  (let ((result)
        (error)
        (user (get-channel-user-by-user-id user-id)))

    (unless (get-val user 'last-access-token)
      (setf error "Missing access token"))
    (when (get-val user 'last-access-token)

      (multiple-value-bind (body)
          (drakma:http-request
           (format nil "https://graph.facebook.com/~A/feed"
                   user-id)
           :method :post
           :parameters `(("message" . ,message)
                         ("oauth_token" . ,(get-val user 'last-access-token))))

       (setf result (json::decode-json-from-string body))

       (if (assoc-path result :error)
           (setf error (cdr (assoc-path result :error :message))))))
    (values result error)))

(defun post-twitter (user-id message)
  (let* ((result)
         (error)
         (user (get-channel-user-by-user-id user-id))
         (channel (if user (get-social-channel (get-val user 'channel-user-type)))) )

    (when (and user channel)

      (when (get-val user 'last-access-token)
        (setf result
              (simple-tweet
               (get-val channel 'app-id)
               (get-val channel 'app-secret)
               (get-val user 'last-access-token)
               (get-val user 'last-token-secret)
               message))

        (setf result (json::decode-json-from-string  (babel:octets-to-string result)))

        (when (assoc-path result :error)
          (setf result nil)
          (setf error (cdr (assoc-path result :error :message))))))
    (values result error)))


(defun post-linkedin (user-id message)
  (let* ((result)
         (error)
         (user (get-channel-user-by-user-id user-id))
         (channel (if user (get-social-channel (get-val user 'channel-user-type)))) )

    (when (and user channel)

      (when (get-val user 'last-access-token)
        (setf result
              (linkedin-share
               (get-val channel 'app-id)
               (get-val channel 'app-secret)
               (get-val user 'last-access-token)
               (get-val user 'last-token-secret)
               message))
        (when result
          (if (stringp result)
              (setf result (json::decode-json-from-string  (babel:octets-to-string result)))
              (setf result (json::decode-json-from-string  result))
              ))

        (when (assoc-path result :error)
          (setf result nil)
          (setf error (cdr (assoc-path result :error :message))))))
    (values result error)))


(defun add-generic-post-action (channel channel-user
                                   action-type post)
  (add-generic-action channel-user
                      nil 
                      channel
                      (get-val channel-user 'user-id)
                      nil
                      action-type 
                      post
                      "Immediate"
                      (get-universal-time)))

(defun handle-generic-post-action (grid widget action result error-message)
  (when error-message
      (setf (get-val widget 'message) error-message)
      (add-generic-action-log action 
                              "Error"
                              error-message
                              "Pending"))
    (unless error-message
      (add-generic-action-log action 
                              "Result"
                              result
                              "Completed")
      (finish-editing grid)))

(defmethod action-handler ((widget post-form))
  (when (string-equal (parameter "action") "post-to-channel")
    (let ((grid (grid widget))
          (channel-user (get-channel-user-by-user-id (parameter "channel-user"))))
      (cond ((string-equal (parameter "service") "facebook")
             (let ((action (add-generic-post-action (parameter "service")
                                                    channel-user
                                                    "Post"
                                                    (parameter "post-status"))))
               (multiple-value-bind (result error-message)
                   (post-facebook
                    (parameter "channel-user")
                    (parameter "post-status"))
                 (handle-generic-post-action grid widget action 
                                             result error-message))))
            ((string-equal (parameter "service") "twitter")
             (let ((action (add-generic-post-action (parameter "service")
                                                    channel-user
                                                    "Tweet"
                                                    (parameter "post-status"))))
               (multiple-value-bind (result error-message)
                   (post-twitter
                    (parameter "channel-user")
                    (parameter "post-status"))
                 (handle-generic-post-action grid widget action 
                                             result error-message))))
            ((string-equal (parameter "service") "LinkedIn")
             (let ((action (add-generic-post-action (parameter "service")
                                                    channel-user
                                                    "Post Status"
                                                    (parameter "post-status"))))
               (multiple-value-bind (result error-message)
                   (post-linkedin
                    (parameter "channel-user")
                    (parameter "post-status"))
                 (handle-generic-post-action grid widget action 
                                             result error-message))))
            (t
             (setf (error-message grid) "No Channel selected")))
      )))

