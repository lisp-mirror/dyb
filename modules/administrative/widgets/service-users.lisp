(in-package :ems)

(defclass service-user-grid (grid)
  ()
  (:default-initargs :edit-inline nil))

(defmethod list-grid-filters ((grid service-user-grid))
  '(status with-audit-data))

(defun get-service-users-data (grid &key filter search)
  (declare (ignore grid search))

  (if (equal filter 'with-audit-data)
      (let ((docs))
        (dolist (doc (coerce (service-users) 'list))
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
               (service-users-collection))))

(defmethod get-rows ((grid service-user-grid))
  (setf (rows grid) (get-service-users-data grid 
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

(defmethod render-row-editor ((grid service-user-grid) row)
  (let ((form (make-widget 'html-framework-form :name "service-user-formv"
                           :grid-size 12
                           :header "Reporting Service-Users"
                           :form-id "reporting-service-user-form"
                           :grid-name (name grid)))
        (form-section (make-widget 'form-section
                                   :name "form-section")))
    (render form
             :grid grid
             :content
             (with-html-to-string ()   
               
               
               (render form-section 
                       :label "Entity"
                       :input 
                       (if (get-val row 'entity)
                           (with-html-to-string ()
                             (render-edit-field
                              "entity-name"  
                              (get-val (get-val row 'entity) 'entity-name)
                              :type :span)
                             (:input :type "hidden" :name "entity-xid" 
                                     :value (get-val (get-val row 'entity) 'xid)))
                           (with-html-to-string ()
                             (render-edit-field
                              "entity" 
                              (get-val (get-val row 'entity) 'xid)
                              :data (entity-list)
                              :required t
                              :blank-allowed t
                              :type :select))))

               (render form-section 
                       :label "User Name"
                       :input (with-html-to-string ()
                                (render-edit-field 
                                 "service-user-name" 
                                 (or (parameter "service-user-name") 
                                     (get-val row 'service-user-name))
                                 :required t)))

               (render form-section 
                       :label "Social Channel"
                       :input (with-html-to-string ()
                                (render-edit-field 
                                 "service-user-type" 
                                 (get-val row 'service-user-type)
                                 :data (get-channels-list)
                                 :required t
                                 :blank-allowed t
                                 :type :select)))
               
               (render form-section 
                       :label "User Id"
                       :input (with-html-to-string ()
                                (render-edit-field
                                 "user-id" 
                                 (get-val row 'user-id))))
               (render form-section 
                       :label "Access Token"
                       :input (with-html-to-string ()
                                (render-edit-field
                                 "last-access-token" 
                                 (get-val row 'last-access-token))))
               (if (xid row)
                   (render form-section
                           :label "Get Id and Oauth"
                           :input 
                           (with-html-to-string ()
                             (let* ((channel (get-social-channel
                                              (or (parameter "service-user-type") 
                                                                 (get-val row 'service-user-type))))
                                    (url (oauth-request-token-url 
                                         channel
                                         "Request Token"
                                         (if (string-equal (get-val channel 'channel-name)
                                                           "facebook")
                                             (list (cons 'user-id (get-val row 'user-id)))
                                             (list (cons 'request-token
                                                         (get-val row 'request-token)))))))

                               (htm (:a :href url  
                                        (str "Authenticate using >> "))
                                    (str url))))))))))


(defun get-social-user-id (channel user-name)
  (let ((url (end-point-url channel "User ID" (list (cons 'user-name user-name)))))
    (when url
      (multiple-value-bind (body)
          (drakma:http-request 
           url :preserve-uri t)
        (if body
            (let ((decoded-body (if (stringp body)
                                    (json::decode-json-from-string body)
                                    (json::decode-json-from-string 
                                     (babel:octets-to-string body)))))
          
              (if (consp (caar decoded-body))
                  (setf decoded-body (car decoded-body)))

          
          
              (if (or (assoc-path (first (cdr (assoc-path  decoded-body :errors))) 
                                  :message) 
                      (assoc-path  decoded-body :error :message))
                  (values nil 
                          (cdr 
                           (or 
                            (assoc-path 
                             (first (cdr (assoc-path  decoded-body :errors))) :message) 
                            (assoc-path  decoded-body :error :message))))
                  (values  (cdr (assoc-path decoded-body :id) ) nil))))))))

(defmethod handle-action ((grid service-user-grid) (action (eql 'save)))
  (when (string-equal (parameter "entity") "")
    (setf (error-message grid) "Select an Entity."))

  (when (string-equal (parameter "service-user-type") "")
    (setf (error-message grid) "Select a Service."))

  (when (and (or (blank-p (parameter "entity"))
                 (blank-p (parameter "entity-xid")))
             (blank-p (parameter "service-user-type")))
 
    (when (and (parameter "service-user-name") 
               (not (string-equal (parameter "service-user-name") "")))
      
      (let ((channel (get-social-channel
                      (parameter "service-user-type"))))
        
        (multiple-value-bind (id error)
            (get-social-user-id channel (parameter "service-user-name"))
          
          (unless (string-equal (parameter "service-user-type") "LinkedIn")
            (unless id
              (setf (error-message grid) 
                    (format nil "User could not be found on Social Channel.~%ERROR: ~A"
                            error))))
          (when (or (string-equal (parameter "service-user-type") "LinkedIn")
                    id)
            (let ((new-doc (editing-row grid)))
              (synq-edit-data new-doc)
        
              (unless (parameter "entity-xid") 
                (setf (get-val new-doc 'entity) 
                      (get-entity-by-id 
                       (if (stringp (parameter "entity"))
                           (parse-integer 
                            (parameter "entity"))
                           (parameter "entity")))))

              (setf (key new-doc) (list (xid (get-val new-doc 'entity))
                                        (parameter "service-user-type")
                                        (parameter "service-user-name")))
              (setf (get-val new-doc 'user-id) id)
              
              (when (string-equal (get-val channel 'auth-type) "OAuth1")
                
                (let* ((response (parse-query-string (oauth1-request channel)))
                       )
                  
                  (setf (get-val new-doc 'request-token)
                        (cdr (assoc-path response "oauth_token")))
                  (setf (get-val new-doc 'request-secret)
                        (cdr (assoc-path response "oauth_token_secret")))))
              (persist new-doc)

              (finish-editing grid))))

        ))))

(defmethod export-csv ((grid service-user-grid))
  (let* ((data (grid-filtered-rows grid)))
    (when data
      (with-output-to-string (stream)
        (format stream "~A~%" "entity-name|service-user-name|service-user-type|service-user-type|start-date|end-date|status")
        (loop 
           for doc across data 
           do (format stream "~A|~A|~A|~A|~A|~A|~A|~%"
                          (get-val (get-val doc 'entity) 'entity-name)
                          (get-val doc 'service-user-name)
                          (get-val doc 'service-user-type)
                          (get-val doc 'description)
                          (format-universal-date (get-val doc 'start-date))
                          (format-universal-date (get-val doc 'end-date))
                          (get-val doc 'status)))))))

(defmethod handle-action ((grid service-user-grid) (action (eql 'cancel)))
  (finish-editing grid))
