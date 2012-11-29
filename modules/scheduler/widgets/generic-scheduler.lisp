(in-package :dyb)

(defclass generic-actions-grid (grid)
  ((parent-grid :initarg :parent-grid)
   (current-doc :initarg nil))
  (:default-initargs :edit-inline nil))

(defmethod list-grid-filters ((grid generic-actions-grid))
  '(completed-actions
    all-actions
    with-audit-data))

(defun get-generic-actions-data (grid &key filter search)
  (declare (ignore grid search))
  (find-docs 
   'vector
   (lambda (doc)

     (if (or (not (get-val doc 'channel-user))
             (if (stringp (get-val doc 'channel-user))
                 nil
                 (match-context-entities (get-val doc 'channel-user) )))
         (cond ((equal filter 'with-audit-data)
                doc)
               ((equal filter 'completed-actions)
                (if (string-equal (get-val doc 'action-status) "Completed")
                    doc))
               ((equal filter 'all-actions)
                doc)
               (t 
                (if (not (string-equal 
                          (get-val doc 'doc-status) "superseded"))
                    (if (string-equal (get-val doc 'action-status) "Pending")
                        doc))))))
   (generic-actions-collection)))

(defmethod get-rows ((grid generic-actions-grid))
  (setf (rows grid)
	(get-generic-actions-data grid 
                          :filter (grid-filter grid)  
                          :search (search-term grid))))

(defclass generic-action-logs-grid (grid)
  ((parent-grid :initarg :parent-grid)
   (current-doc :initarg nil))
  (:default-initargs :edit-inline nil))

(defun get-generic-action-logs-data (grid &key filter search)
  (declare (ignore filter search))
  (when (and (get-val grid 'current-doc))
    (setf (rows grid)
          (loop for log across 
               (coerce (get-val 
                        (get-val grid 'current-doc) 
                        'action-log) 'vector)
             collect log))))

(defmethod get-rows ((grid generic-action-logs-grid))
  (setf (rows grid)
	(get-generic-action-logs-data grid 
                          :filter (grid-filter grid)  
                          :search (search-term grid))))

(defmethod render-row-editor ((grid generic-actions-grid) row)
  (let ((action-form (make-widget 
                       'html-framework-form :name "schedule-action-formx"
                       :grid-size 12
                       :header "Schedule New Message"
                       :form-id "schedule-action-form"
                       :form-data t
                       :ajax-submit nil))
        (form-section (make-widget 'form-section
                                   :name "form-section"))
        (current-doc (get-val grid 'current-doc))
        (channel-users (make-widget 'channel-user-select 
                                    :name "channel-user-select-dropown"))
        (tabs (make-instance 'html-framework-tab-box
                              :name "actions-edit-tabs"
                              :header "Action"
                              :icon "card--pencil")))
    (setf (tabs tabs)
          `(("Action" ,
             (with-html-string
               (render action-form
                       :grid grid
                       :content
                       (with-html-string
                         (:input :type "hidden" :name "from-user-id" 
                                 :value (get-val 
                                         (get-val 
                                          (get-val current-doc 'payload) 'from) 'id))
                         (:input :type "hidden" :name "action-type" 
                                 :value "Post")
                         (:input :type "hidden" :name "to-user-id" 
                                 :value (if (get-val (get-val current-doc 'payload) 'to)
                                            (if (listp (get-val 
                                                        (get-val current-doc 'payload) 'to))
                                                (get-val 
                                                 (first (get-val 
                                                         (get-val current-doc 'payload) 'to)) 'id)
                                                (get-val (get-val 
                                                          (get-val current-doc 'payload) 'to) 'id))))
                      

                         (destructuring-bind (service channel-user)
                             (selects channel-users)
                           (render channel-users)

                           (setf (value service) (or (parameter "service")
                                                     (get-val row 'post-type)))
                           (setf (value channel-user) (or (parameter "channel-user")
                                                          (get-val row 'from-user-id)))
                           (render form-section
                                   :label "Select To Channel"
                                   :input
                                   (with-html-string
                                     (render service)))
                           (render form-section
                                   :label "Select Account"
                                   :input
                                   (with-html-string
                                     (render channel-user))))

                
                         (render form-section 
                                 :label "Message"
                                 :input 
                                 (with-html-string
                                   (render-edit-field
                                    "action-content" 
                                    (or (parameter "action-content") 
                                        (get-val row 'action-content))
                                    :required t
                                    :type :textarea)))
                         (render 
                          form-section
                          :label "Add Image"
                          :input (with-html-string
                                   (when (image-url row)
                                     (htm (:div 
                                           (:img :src 
                                                 (format nil "/dyb/images/~a" 
                                                         (file-namestring  (image-url row)))
                                                 :width 250
                                                 :height 250))))
                                   (:input :type "file" :name "file" :id "file"
                                           :style "display: inline-block;")))
                         (render 
                          form-section
                          :label "Add Link"
                          :input (with-html-string
                                   (render-edit-field 
                                    "post-url"
                                    (or (parameter "post-url")
                                        (get-val row 'post-url))
                                 
                                    :required nil)))
                         
                         (render 
                          form-section
                          :label "Shortened Link"
                          :input (with-html-string
                                   (render-edit-field 
                                    "short-url"
                                    (if (and (or (blank-p (parameter "post-url"))
                                                 (blank-p (get-val row 'post-url)))
                                               (short-url row))
                                      (format-short-url (short-url row)))
                                    :type :span)))
                         (render 
                          form-section
                          :label "Select Date"
                          :input (with-html-string
                                   (render-edit-field 
                                    "scheduled-date"
                                    (or (parameter "scheduled-date")
                                        (if (get-val row 'scheduled-date)
                                            (format-universal-date 
                                             (get-val row 'scheduled-date)))
                                        (current-date))
                                    :type :date
                                    :required t)
                                   ))
                         (render 
                          form-section
                          :label "Select Time"
                          :input (with-html-string
                                   (render-edit-field 
                                    "scheduled-time"
                                    (or (parameter "scheduled-time")
                                        (if (get-val row 'scheduled-date)
                                            (multiple-value-bind 
                                                  (second minute hour day month year)
                                                (decode-universal-time 
                                                 (universal-to-my-gmt
                                                  (get-val row 'scheduled-date))
                                                 0)
                                              (declare (ignore second day month year))
                                              (format nil "~2,'0d:~2,'0d" hour minute)))
                                        "00:00"
                                        )
                                    :type :text
                                    :required t) 
                                   (str "hh:mm")))))))
            ("Action Logs" ,
             (with-html-string
               (:div :class "section _100" 
                     (let* ((columns
                             (list
                              (make-instance 'grid-column
                                             :name 'lable
                                             :header "Lable")

                              (make-instance 'grid-column
                                             :name 'message
                                             :header "message")
                              (make-instance 'grid-column
                                             :name 'stamp
                                             :header "Stamp")))
                            (logs-grid (make-widget 'generic-action-logs-grid 
                                                       :name "generic-actions-log-gridx"
                                                       ;;:columns columns
                                                       :title "Logs"
                                                       :row-object-class 'generic-action-log)))
                       (setf (get-val logs-grid 'css-span) 7)
                       (setf (get-val logs-grid 'columns)
                             columns)
                       (setf (get-val logs-grid 'title)
                             (format nil "Logs for (~A)" 
                                     (get-val (editing-row grid) 'post-id)))

                       (setf (get-val logs-grid 'parent-grid) grid)
                       (setf (get-val logs-grid 'current-doc) (editing-row grid))
                        
                       (render logs-grid)))))
            ))
    (render tabs)
    
    ))

(defparameter *tmp-directory* #p"~/hunchentoot-upload/")

(defun authorized-dispatcher (dispatch-fn)
  (lambda (request)
    (when (current-user)
      (funcall dispatch-fn request))))

(defmethod make-auth-dispathcer (path)
  (push
   (authorized-dispatcher (create-static-file-dispatcher-and-handler
                           (format nil "/dyb/images/~A" (file-namestring path))
                           path))
   *dispatch-table*))

(defun handle-upload (parameters)
  (when parameters
    (destructuring-bind (path name application-type) parameters
      (declare (ignore application-type))
      (ensure-directories-exist *tmp-directory*)
      (let ((new-path (merge-pathnames (format nil "~(~32r~32r~)-~a"
                                               (random 99999) (get-universal-time)
                                               name)
                                       *tmp-directory*)))
        (rename-file path new-path)
        (when (probe-file new-path)
          (make-auth-dispathcer new-path)
          new-path)))))

(defmethod handle-action ((grid generic-actions-grid) (action (eql 'save)))
  (setf (error-message grid) nil)
  (when (and (string-equal (parameter "form-id") "schedule-action-form"))

    
    (unless (blank-p (parameter "scheduled-date"))
      (setf (error-message grid) "Please enter a valid date."))
    
    (unless (blank-p (parameter "service"))
      (setf (error-message grid) "Please enter a channel to post to."))
    
    (unless (blank-p (parameter "channel-user"))
      (setf (error-message grid) "Please enter a user to post as."))

    (when (and (blank-p (parameter "service")) 
               (blank-p (parameter "channel-user")) 
               (blank-p (parameter "scheduled-date")))
      (let (
            (to-user nil)
            (image (handle-upload (post-parameter "file")))
            (doc (editing-row grid))
            (short-url (if (blank-p (parameter "post-url"))
                           (make-short-url (parameter "post-url")))))
        
        (when doc
          (let ((date-time nil))
            (multiple-value-bind (year month day)
                (decode-date-string (parameter "scheduled-date"))
              (multiple-value-bind (second minute hour)
                  (decode-time-string (format nil "~A:00" (parameter "scheduled-time")))
                (when second
                  (setf date-time 
                        (encode-universal-time 
                         second minute hour day month year 
                         0))

                  (cond ((xid doc)

                         (synq-edit-data doc)
                         (setf
                          (channel-user doc) (get-channel-user-by-user-id (parameter "channel-user"))
                          (post-type doc) (parameter "service")
                               (from-user-id doc) (parameter "channel-user")
                               (scheduled-date doc) date-time
                               (image-url doc) (or image
                                                   (image-url doc))
                               (short-url doc) short-url)
                         (persist doc))
                        (t
                         
                         (persist (make-generic-action    
                                   (get-channel-user-by-user-id (parameter "channel-user"))
                                   nil 
                                    (parameter "service")
                                    
                                   (parameter "channel-user") 
                                   to-user 
                                   (parameter "action-type")
                                   (parameter "action-content")
                                   "Timed"
                                   date-time
                                   :image-url image
                                   :post-url (parameter "post-url")
                                   :short-url short-url)))))
                (unless second
                  (setf (error-message grid) minute))))

            )
          (finish-editing grid))
        ;; (unless (or from-user to-user)
        ;;     (setf (error-message grid) "User does not exist."))
        ))))
 
