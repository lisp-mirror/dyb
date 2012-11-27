(in-package :dyb)

(defclass generic-grid (grid)
  ()
  (:metaclass widget-class)
  (:include-css "/appcss/posts.css")
  (:default-initargs :edit-inline nil))

(defmethod list-grid-filters ((grid generic-grid))
  '(facebook-only
    twitter-only
    linkedin-only
    mentions-only
    with-audit-data))

(defmethod list-grid-actions (grid)
  `(("Export CSV"
     ,(format nil "window.open(\"/dyb/export-csv?grid=~a&script-name=~a\")"
              (name grid)
              (script-name*)))
    ("Quick Post"
     ,(js-render (editor grid)
                 (js-pair "grid-name" (name grid))
                 (js-pair "action" "new")))))

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
                   (cond 
                     ((equal filter 'facebook-only)
                      (if (string-equal (get-val doc 'post-type) "Facebook")
                          doc)
                      )
                     ((equal filter 'twitter-only)
                      (if (string-equal (get-val doc 'post-type) "Twitter")
                          doc)
                      )
                     ((equal filter 'linkedin-only)
                      (if (string-equal (get-val doc 'post-type) "LinkedIn")
                          doc)
                      )
                     ((equal filter 'mentions-only)
                      (if (string-equal (get-val doc 'post-type) "Social-Mention")
                          doc)
                      )
                     ((equal filter 'with-audit-data)
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

(defun handle-generic-action (widget action result error-message)
  (setf (get-val widget 'message) nil)
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
      (defer-js (format nil "$('#~a').dialog('close')" (name widget)))))

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
                   (cond ((get-val action 'image-url)
                                 (post-facebook-image (parameter "channel-user")
                                                      (parameter "post-status")
                                                      (get-val action 'image-url)
                                                      ))
                                ((or (get-val action 'short-url) 
                                     (get-val action 'post-url))
                                 (post-facebook-url (parameter "channel-user")
                                                    (parameter "post-status")
                                                    (or (get-val action 'short-url) 
                                                        (get-val action 'post-url))
                                                      ))
                                (t
                                 (post-facebook (get-val action 'from-user-id) 
                                         (parameter "post-status")))
                                )
                 (handle-generic-post-action grid widget action 
                                             result error-message))))
            ((string-equal (parameter "service") "twitter")
             (let ((action (add-generic-post-action (parameter "service")
                                                    channel-user
                                                    "Tweet"
                                                    (parameter "post-status"))))
               (multiple-value-bind (result error-message)
                   (post-twitter
                    channel-user
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

