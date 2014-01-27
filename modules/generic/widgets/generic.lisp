(in-package :dyb)

(defclass generic-grid (grid)
  ()
  (:metaclass widget-class)
  (:include-css "/appcss/posts.css")
  (:default-initargs :not-sorting-columns '(1)))

(defmethod list-grid-filters ((grid generic-grid))
  '(facebook
    twitter
    linkedin
    web-search
    with-audit-data))

(defmethod list-grid-actions ((grid generic-grid))
  `(("Export CSV"
     (:link
      ,(frmt "/dyb/export-csv?grid=~a&script-name=~a"
             (escape (name grid))
             (escape (script-name*)))))
    ("Quick Post"
     (:js ,(js-render (editor grid)
                      (js-pair "grid-name" (name grid))
                      (js-pair "action" "new"))))))

(defclass grid-action (ajax-widget)
  ((grid :initarg :grid
         :initform nil
         :accessor grid))
  (:metaclass widget-class))

(defmethod render ((widget grid-action) &key )
  (with-html
    "Grid Action"))

(defmethod handle-action ((grid generic-grid) (action (eql :block-user)))
  (setf (action-widget grid)
        (make-widget 'grid-action :grid grid :name "XXX")))

(defun get-generic-data (grid &key filter search)
  (declare (ignore grid search))
  (find-docs 'vector
              (lambda (doc)
                (if (match-context-entities  (channel-user doc) )
                   (cond 
                     ((equal filter 'facebook)
                      (if (string-equal (get-val doc 'post-type) "Facebook")
                          doc)
                      )
                     ((equal filter 'twitter)
                      (if (string-equal (get-val doc 'post-type) "Twitter")
                          doc)
                      )
                     ((equal filter 'linkedin)
                      (if (string-equal (get-val doc 'post-type) "LinkedIn")
                          doc)
                      )
                     ((equal filter 'web-search)
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
  ((message :initarg :message
            :accessor message))
  (:metaclass widget-class))

(defmethod handle-action ((grid generic-grid) (action (eql :new)))
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
                                    :name "channel-user-select-dropown"))
         (shorified-message (let ((*site-url* "http://dxw.co.za/"))
                              (shortify-string (parameter "post-status")))))
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
                         :input (render-to-string service))
                 (render form-section
                         :label "Channel User"
                         :input (render-to-string channel-user))
                 (render form-section
                         :label "Post"
                         :input
                         (with-html-string
                           (render-edit-field
                            "post-status"
                            (parameter "post-status")
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
                     "$('[name=\"post-status\"]').bind('change input propertychange',
function() {
var s = shortifyString($('[name=\"post-status\"]').val());
var length = s.length;
$('#message-length').text(length);
$('#processed-content').text(s)})")
                 (if (get-val widget 'message)
                     (htm (:span :style "color:red;" ;
                                 (get-val widget 'message))))))))))

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
  (cond (error-message
         (setf (message widget) error-message)
         (add-generic-action-log action "Error" error-message "Pending"))
        (t
         (setf (message widget) nil)
         (add-generic-action-log action "Result" result "Completed")
         (finish-editing (grid widget)))))

(defmethod action-handler ((widget post-form))
  (with-parameters (action channel-user service post-status)
    (let ((grid (grid widget)))
      (cond ((not (equal action "post-to-channel")))
            ((empty-p service)
             (setf (error-message grid) "No channel selected"))
            ((empty-p channel-user)
             (setf (error-message grid) "No account selected."))
            (t
             (let ((channel-user (get-channel-user-by-user-id 
                                  channel-user service)))
               (cond ((not channel-user)
                      (setf (error-message grid) "No account found."))
                     ((string-equal service "Facebook")
                      (break "?")
                      (let ((action (add-generic-post-action service
                                                             channel-user
                                                             "Post"
                                                             post-status)))
                        (multiple-value-bind (result error-message)
                            (cond ((get-val action 'image-url)
                                   (post-facebook-image channel-user
                                                        post-status
                                                        (get-val action 'image-url)
                                                        ))
                                  ((or (get-val action 'short-url) 
                                       (get-val action 'post-url))
                                   (post-facebook-url channel-user
                                                      post-status
                                                      (or (get-val action 'short-url) 
                                                          (get-val action 'post-url))))
                                  (t
                                   (post-facebook (get-val action 'from-user-id) 
                                                  post-status)))
                          (handle-generic-post-action grid widget action 
                                                      result error-message))))
                     ((string-equal service "twitter")
                      (let ((post-status (shortify-string post-status)))
                        (if (> (length post-status) 140)
                            (setf (error-message grid)
                                  (frmt "Message too long - ~a." (length post-status)))
                            (multiple-value-bind (result error-message)
                                (post-twitter
                                 channel-user
                                 post-status
                                 :image-path (get-val action 'image-url))
                              (handle-generic-post-action
                               grid widget
                               (add-generic-post-action service
                                                        channel-user "Tweet" post-status)
                               result error-message)))))
                     ((string-equal service "LinkedIn")
                      (let ((action (add-generic-post-action service
                                                             channel-user
                                                             "Post Status"
                                                             post-status)))
                        (multiple-value-bind (result error-message)
                            (post-linkedin channel-user post-status)
                          (handle-generic-post-action grid widget action 
                                                      result error-message)))))))))))

