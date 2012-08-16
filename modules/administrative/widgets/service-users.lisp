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
  (let ((form (make-widget 'peach-form :name "service-user-formv"
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
                                 (get-val row 'service-user-name)
                                 :required t)))

               (render form-section 
                       :label "Service"
                       :input (with-html-to-string ()
                                (render-edit-field 
                                 "service-user-type" 
                                 (get-val row 'service-user-type)
                                 :data (list (list "Facebook" "Facebook")
                                             (list "Twitter" "Twitter"))
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
                           :input (with-html-to-string ()
                                    (cond ((string-equal 
                                            (if (get-val row 'service-user-type)
                                                (get-val row 'service-user-type)
                                                (parameter "service-user-type")) 
                                            "Facebook")
                                           (htm (:a :href  (facebook-oauth-uri row)  
                                                    (str "Authenticate using >> ")) (str (facebook-oauth-uri row))))
                                          ((string-equal 
                                            (if (get-val row 'service-user-type)
                                                (get-val row 'service-user-type)
                                                (parameter "service-user-type")) 
                                            "Twitter")
                                           (htm (:a :href  (facebook-oauth-uri row)  
                                                    (str "Authenticate using >> ")) (str (facebook-oauth-uri row))))))))))))


(defun get-facebook-id (username)
  (multiple-value-bind (body)
                (drakma:http-request 
                 (format nil "https://graph.facebook.com/~A" username))
    (if body
        (let ((decoded-body (json::decode-json-from-string body)))
          (if (string-equal (first (first decoded-body)) "ERROR")
              (cdr (second (first decoded-body)))
              (cdr (first decoded-body)))))))

(defmethod handle-action ((grid service-user-grid) (action (eql 'save)))
  

  (when (string-equal (parameter "entity") "")
    (setf (error-message grid) "Select an Entity."))

  (when (string-equal (parameter "service-user-type") "")
    (setf (error-message grid) "Select a Service."))

  (when (and (parameter "service-user-name") (not (string-equal (parameter "service-user-name") "")))
    (let ((facebook-user-id (get-facebook-id (parameter "service-user-name"))))
      (if (find #\# facebook-user-id)
          (setf (error-message grid) facebook-user-id)
          (unless (string-equal (parameter "entity") "")
            (let ((new-doc (editing-row grid))
                  (old-doc (copy (editing-row grid))))
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
              (setf (get-val new-doc 'user-id) facebook-user-id)
              (if (xid old-doc)
                  (persist new-doc :old-object old-doc)
                  (persist new-doc))

              (finish-editing grid)))))))




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