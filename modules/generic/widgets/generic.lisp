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
    "Grid Action")
  (open-dialog widget (grid widget)))

(defmethod handle-action ((grid generic-grid) (action (eql 'block-user)))
  (setf (action-widget grid)
        (make-widget 'grid-action :grid grid :name "XXX")))

(defun get-generic-data (grid &key filter search)
  (declare (ignore grid search))
  (find-docs 'vector
              (lambda (doc)
               (if (match-context-entities  doc )
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




(defclass post-form (ajax-widget)
  ((grid :initarg :grid
         :initform nil
         :accessor grid)
   (message :initarg :message))
  (:metaclass widget-class))


(defmethod render ((widget post-form) &key )
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

    (with-html 
      (when (parameter "action")
          (render comment-form
                  :content
                  (with-html-to-string ()
                    (:div 
                     (destructuring-bind (service channel-user)
                         (selects channel-users)
                       (render channel-users)

                        (setf (value service) (parameter "service"))
                        (setf (value channel-user) (parameter "channel-user"))

                       (render form-section
                               :label "Post To Channel"
                               :input
                               (with-html-to-string ()
                                 (render service)))
                       (render form-section
                               :label "Channel User"
                               :input
                               (with-html-to-string ()
                                 (render channel-user)))

                       (render form-section 
                               :label "Post"
                               :input 
                               (with-html-to-string ()
                                 (render-edit-field
                                  "post-status" 
                                  (parameter "post-status")
                                  :required t
                                  :type :textarea)))))))
          (str (get-val widget 'message))))
    (open-dialog widget (grid widget))))

(defmethod handle-action ((grid generic-grid) (action (eql 'new)))
  (setf (action-widget grid)
        (make-widget 'post-form 
                     :grid grid 
                     :name "post-action-form")))

(defun post-facebook (user-id message)
  (let ((result)
        (error)
        (user (get-channel-user-by-user-id user-id)))
    
    (when (get-val user 'last-access-token)
      (multiple-value-bind (body)
          (drakma:http-request 
           (format nil "https://graph.facebook.com/~A/feed" 
                   user-id)
           :method :post
           :parameters `(("message" . ,message)
                         ("oauth_token" . ,(get-val user 'last-access-token))
                         ))
        
       (setf result (json::decode-json-from-string body)) 
       (if (assoc-path result :error)
           (setf error (cdr (assoc-path result :error :message))))))
    (values result error)))

(defun post-twitter (user-id message)
  (let* ((result)
         (error)
         (user (get-channel-user-by-user-name user-id))
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
         (user (get-channel-user-by-user-name user-id))
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

(defmethod action-handler ((widget post-form))
  
  (when (string-equal (parameter "action") "post-to-channel")  
    (cond ((string-equal (parameter "service") "facebook")
           (multiple-value-bind (result error-message)
               (post-facebook 
                (parameter "channel-user")
                (parameter "post-status"))
      
             (if error-message
                 (setf (get-val widget 'message) error-message)
                 (defer-js (format nil "$('#~a').dialog('close')" (name widget))))))
          ((string-equal (parameter "service") "twitter")
           (multiple-value-bind (result error-message)
               (post-twitter 
                (parameter "channel-user")
                (parameter "post-status"))
      
             (if error-message
                 (setf (get-val widget 'message) error-message)
                 (defer-js (format nil "$('#~a').dialog('close')" (name widget))))))
          ((string-equal (parameter "service") "LinkedIn")

           (multiple-value-bind (result error-message)
               (post-linkedin 
                (parameter "channel-user")
                (parameter "post-status"))
      
             (if error-message
                 (setf (get-val widget 'message) error-message)
                 (defer-js (format nil "$('#~a').dialog('close')" (name widget)))))))
    ))



(defclass assign-task-action (ajax-widget)
  ((grid :initarg :grid
         :initform nil
         :accessor grid)
   (message :initarg :message))
  (:metaclass widget-class))

(defmethod render ((widget assign-task-action) &key grid)
  (let ((row (set-current-row (get-val widget 'grid)))
        (assign-form (make-widget 'html-simple-framework-form 
                                    :name "assign-task-form"
                                    :grid-size 12
                                    :form-id "assign-task-form"
                                    :action "assign-task"
                                    :action-title "Assign Task"))
        (form-section (make-widget 'form-section
                                   :name "form-section")))
    (when row
        (with-html
          (render assign-form
                  :grid grid
                  :content
                  (with-html-to-string ()
                      
                      
                    (render form-section 
                            :label "Entity"
                            :input 
                            (with-html-to-string ()
                              (render-edit-field
                               "entityx"  
                               (get-val (get-val row 'entity) 'entity-name)
                               :type :span)
                              (:input :type "hidden" :name "entity" 
                                      :value (get-val (get-val row 'entity) 'xid))))
                    (render form-section 
                            :label "Description"
                            :input 
                            (with-html-to-string ()
                              (render-edit-field
                               "task-description" 
                               (parameter "task-description")
                               :required t
                               :type :text)))
                    (render form-section 
                            :label "Instructions"
                            :input 
                            (with-html-to-string ()
                              (render-edit-field
                               "task-instructions" 
                               (or (parameter "task-instructions")
                                   (raw-post-id 
                                    row 
                                    (cond ((string-equal (post-type row) "twitter")
                                           'twitter)
                                          ((string-equal (post-type row) "facebook")
                                           'facebook)
                                          ((string-equal (post-type row) "linkedin")
                                           'twitter)
                                          ((string-equal (post-type row) "social mention")
                                           'social-mention)
                                          )))
                               :required t
                               :type :textarea)))
                      
                    (render 
                     form-section
                     :label "Status"
                     :input (with-html-to-string ()
                              (render-edit-field
                               "task-status" 
                               "Assigned"
                               :type :span)
                              (:input :type "hidden" :name "task-status" 
                                      :value "Assigned")))
                    (render 
                     form-section
                     :label "Assigner"
                     :input (with-html-to-string ()
                              (render-edit-field 
                               "assigning-user"
                               (email (current-user))
                               :type :span)
                              (:input :type "hidden" :name "assigning-user" 
                                      :value (email (current-user)))))

                    (render 
                     form-section
                     :label "Assignee"
                     :input (with-html-to-string ()
                              (render-edit-field 
                               "assigned-user"
                               (parameter "assigned-user")
                               :data (user-list)
                               :type :select)))
                      
                    (render 
                     form-section
                     :label "Assigned Date"
                     :input (with-html-to-string ()
                              (render-edit-field 
                               "assigned-date"
                               (or (parameter "assigned-date")
                                   (format-universal-date-time  (get-universal-time)))
                               :type :span)))

                    (render 
                     form-section
                     :label "Scheduled Date"
                     :input (with-html-to-string ()
                              (render-edit-field 
                               "scheduled-date"
                               (or (parameter "scheduled-date")
                                   (current-date))
                               :type :date
                               :required t))))))))
  (open-dialog widget (grid widget)))

(defmethod handle-action ((grid generic-grid) (action (eql 'assign-task-form)))
  (setf (action-widget grid)
        (make-widget 'assign-task-action :grid grid :name "assign-task-action-form")))

(defmethod  action-handler ((widget assign-task-action))
  

  (when (string-equal (parameter "action") "assign-task")
    (setf (get-val widget 'message) nil)

    (let* ((error-message)
          (entity (get-entity-by-id 
                               (if (stringp (parameter "entity"))
                                   (parse-integer 
                                    (parameter "entity"))
                                   (parameter "entity"))))
          (new-doc (make-task entity 
                       (parameter "task-description")
                       (get-user (parameter "assigning-user"))
                       (get-user (parameter "assigned-user")) 
                       (parameter "scheduled-date")
                       :task-instructions
                       (parameter "task-instructions")
                       :assigned-date
                       (parameter "assigned-date")
                       :task-status
                       (parameter "task-status") 
                       )))
    

      (unless (parameter "entity")
        (setf error-message "Please supply an entity."))

      (when (parameter "entity")
        (persist new-doc))

      (if error-message
          (setf (get-val widget 'message) error-message)
          (defer-js (format nil "$('#~a').dialog('close')" (name widget)))))
    ))