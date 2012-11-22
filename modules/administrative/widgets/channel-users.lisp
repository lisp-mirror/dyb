(in-package :dyb)

(defclass channel-user-grid (grid)
  ()
  (:default-initargs :edit-inline nil))

(defmethod list-grid-filters ((grid channel-user-grid))
  '(status with-audit-data))

(defun get-channel-users-data (grid &key filter search)
  (declare (ignore grid search))

  (if (equal filter 'with-audit-data)
      (let ((docs))
        (dolist (doc (coerce (channel-users) 'list))
          (when (match-context-entities doc)
            (setf docs (append docs (list doc)))
            (when (old-versions doc)
              (setf docs (append docs (old-versions doc))))))
        (coerce docs 'vector))
    (find-docs 'vector
               (lambda (doc)

                 (if (match-context-entities doc)
                     (cond (t
                            (if (not (string-equal
                                      (get-val doc
                                               'doc-status) "superseded"))

                                doc)))))
               (channel-users-collection))))

(defmethod get-rows ((grid channel-user-grid))
  (setf (rows grid) (get-channel-users-data grid
                                       :filter (grid-filter grid)
                                       :search (search-term grid))))

(defun entity-mine-list ()
  (let ((e-list))
    (dolist (doc (coerce (entities) 'list))
      (if (and (not (string-equal (get-val doc 'doc-status) "superseded"))
            (string-equal (get-val (get-val doc 'entity-type) 'entity-type-name) "Mine"))
          (setf e-list (append e-list (list (list (get-val doc 'xid)
                                                  (get-val doc 'entity-name)))))))
    e-list))

(defmethod render-row-editor ((grid channel-user-grid) row)
  (let ((form (make-widget 'html-framework-form :name "channel-user-formv"
                           :grid-size 12
                           :header "Reporting channel-users"
                           :form-id "reporting-channel-user-form"
                           :grid-name (name grid)))
        (form-section (make-widget 'form-section
                                   :name "form-section"))
        (tabs (make-instance 'html-framework-tab-box
                              :name "channel-user-tabs"
                              :header "Channel Users"
                              :icon "card--pencil")))

    (render
     form
     :grid grid
     :content
     (with-html-to-string ()
       (render form-section
               :label "Entity"
               :input
               (with-html-string
                 (render-edit-field
                  "entity"
                  (or (parameter "entity") (get-val (get-val row 'entity) 'xid))
                  :data (if (get-val (current-user) 'super-user-p)
                            (admin-entity-list) 
                            (entity-list))
                  :required t
                  :blank-allowed t
                  :type :select)))

       (render form-section
               :label "Social Channel"
               :input (with-html-to-string ()
                        (render-edit-field
                         "channel-user-type"
                         (get-val row 'channel-user-type)
                         :data (get-channels-list)
                         :required t
                         :blank-allowed t
                         :type :select)))

       (render form-section
               :label "Profile Type"
               :input (with-html-to-string ()
                        (render-edit-field
                         "profile-type"
                         (get-val row 'profile-type)
                         :data (list (list "User" "User")
                                     (list "Page" "Page"))
                         :required t
                         :blank-allowed nil
                         :type :select)))

       (render form-section
               :label "User Name"
               :input (with-html-to-string ()
                        (render-edit-field
                         "channel-user-name"
                         (or (parameter "channel-user-name")
                             (get-val row 'channel-user-name))
                         :type :span)))
               
       (render form-section
               :label "User Id"
               :input (with-html-to-string ()
                        (render-edit-field
                         "user-id"
                         (get-val row 'user-id)
                         :type :span)))

       (render form-section
               :label "Access Token"
               :input (with-html-to-string ()
                        (render-edit-field
                         "last-access-token"
                         (get-val row 'last-access-token)
                         :type :span)))

       (render form-section
               :label "Access Token Expiry Date"
               :input (with-html-to-string ()
                        (render-edit-field
                         "access-token-expiry-date"
                         (format-universal-date-time 
                          (get-val row 'access-token-expiry-date))
                         :type :span)))

       (if (blank-p (get-val row 'last-access-token))
           (if (string-equal (get-val row 'channel-user-type) "Facebook")
               (render form-section
                       :label "Pull in pages connected to user."
                       :input (with-html-to-string ()
                                (:button
                                 :class "btn btn-info"
                                 :onclick
                                 (js-render-form-values 
                                  (editor grid)
                                  "reporting-channel-user-form"
                                  (js-pair "grid-name" (name grid))
                                  (js-pair "action" "pull-pages")
                                  (scroll-to grid))
                                 "Pull")))))
       (if (xid row)
           (render form-section
                   :label "Get Id and Oauth"
                   :input
                   (with-html-to-string ()
                     (let* ((channel (get-social-channel
                                      (or (parameter "channel-user-type")
                                          (get-val row 'channel-user-type))))
                            (url 
                             (oauth-request-token-url
                              channel
                              "Request Token"
                              (if (string-equal 
                                   (get-val channel 'channel-name)
                                   "facebook")
                                  (list 
                                   (cons 
                                    'user-id 
                                    (format nil "~A" 
                                            (get-val row 'verification-code))))
                                  (list (cons 'request-token
                                              (get-val row 'request-token)))))))

                       (htm (:a :href url
                                (str "Authenticate using >> "))
                            (str url))))))))
    
    ))


(defun get-social-user-id (channel user-name)
  (let ((url (end-point-url  channel "User ID" (list (cons 'user-name user-name))))
        (end-point (get-end-point channel  "User ID")))
    (when url
      (http-call url :get :return-type (get-val end-point 'return-type)))))

(defmethod handle-action ((grid channel-user-grid) (action (eql 'pull-pages)))
  (cond ((string-equal (parameter "channel-user-type") "Facebook")
         (multiple-value-bind (accounts)
             (facebook-accounts (editing-row grid))
           (when accounts
             (setf (gethash "accounts" 
                            (get-val (editing-row grid) 'user-data))  accounts)
             (dolist (account (gpv accounts :data))
        
               (unless (string-equal (gpv account :category) "Application")
                 (let ((user (get-channel-user-by-user-id (gpv account :id))))

                   (when user
                     (setf (get-val user 'last-access-token) (gpv account :access-token))
                     (when (gpv account :access-token)
                         (multiple-value-bind (result status error) 
                             (facebook-profile user)
                           ;;TODO: Process errors.
                           (when result
                             (setf (get-val user 'user-id) (gpv result :id))
                             (setf (get-val user 'channel-user-name) 
                                   (gpv result :username))
                             (setf (gethash "profile" (get-val user 'user-data)) result)) ))
                     (persist user))

                   
                   (unless user
                     (persist (make-channel-user
                               (get-entity-by-id
                                (if (stringp (parameter "entity"))
                                    (parse-integer
                                     (parameter "entity"))
                                    (parameter "entity")))
                               (gpv account :username)
                               "Facebook"
                               "Page"
                               (gpv account :id)
                               :last-access-token (gpv account :access--token)
                               )))))))))
        ((string-equal (parameter "channel-user-type") "LinkedIn")

         )))

(defmethod handle-action ((grid channel-user-grid) (action (eql 'save)))
  (when (string-equal (parameter "entity") "")
    (setf (error-message grid) "Select an Entity."))

  (when (string-equal (parameter "channel-user-type") "")
    (setf (error-message grid) "Select a Service."))

  (when (and (blank-p (parameter "entity"))
             (blank-p (parameter "channel-user-type")))
 
    (let ((channel (get-social-channel
                      (parameter "channel-user-type")))
              (new-doc (editing-row grid)))
              (synq-edit-data new-doc)
        
              (setf (get-val new-doc 'entity)
                      (get-entity-by-id
                       (if (stringp (parameter "entity"))
                           (parse-integer
                            (parameter "entity"))
                           (parameter "entity"))))

              (setf (key new-doc) (list (xid (get-val new-doc 'entity))
                                        (parameter "channel-user-type")
                                        (parameter "channel-user-name")))
              
              (when (string-equal (get-val channel 'auth-type) "OAuth1")
                
                (let* ((response (parse-query-string (oauth1-request channel))))
                  (setf (get-val new-doc 'request-token)
                        (cdr (assoc-path response "oauth_token")))
                  (setf (get-val new-doc 'request-secret)
                        (cdr (assoc-path response "oauth_token_secret")))))

              (unless (get-val new-doc 'verification-code)
                (when (string-equal (get-val channel 'auth-type) "OAuth2")
                  (setf (get-val new-doc 'verification-code) 
                        (format nil "~A-~A" (get-universal-time) (random 9999999999)))))

              (persist new-doc)

              (finish-editing grid))))

(defmethod export-csv ((grid channel-user-grid))
  (let* ((data (grid-filtered-rows grid)))
    (when data
      (with-output-to-string (stream)
        (format stream "~A~%" "entity-name|channel-user-name|channel-user-type|channel-user-type|start-date|end-date|status")
        (loop
           for doc across data
           do (format stream "~A|~A|~A|~A|~A|~A|~A|~%"
                          (get-val (get-val doc 'entity) 'entity-name)
                          (get-val doc 'channel-user-name)
                          (get-val doc 'channel-user-type)
                          (get-val doc 'description)
                          (format-universal-date (get-val doc 'start-date))
                          (format-universal-date (get-val doc 'end-date))
                          (get-val doc 'status)))))))

(defmethod handle-action ((grid channel-user-grid) (action (eql 'cancel)))
  (finish-editing grid))

(defmethod handle-action ((grid channel-user-grid) (action (eql 'refresh-profile)))
  (let ((user (editing-row grid)))
    (when user
      (when (string-equal (get-val user 'channel-user-type)
                        "Facebook")
        (multiple-value-bind (result status error) 
            (facebook-profile user)                       
                              
          (when result
            (setf (get-val user 'user-id) (gpv result :id))
            (setf (get-val user 'channel-user-name) 
                  (gpv result :username))
            (setf (gethash "profile" (get-val user 'user-data)) result)

            (facebook-friends-refresh user)
            (persist user))))))
(finish-editing grid)
 )