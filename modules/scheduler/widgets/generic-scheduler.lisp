(in-package :dyb)

(defclass generic-actions-grid (grid)
  ((current-doc :initarg nil)
   (date :initarg :date
               :initform nil
               :accessor date))
  (:default-initargs :edit-inline nil))

(defmethod list-grid-filters ((grid generic-actions-grid))
  '(24-hours
    7-days
    30-days
    pending-actions
    completed-actions
    all-actions
    with-audit-data))

(defun least-universal-date (date)
  (multiple-value-bind (year month day) (decode-date date)
    (encode-universal-time 0 0 0 day month year (time-zone))))

(defun greatest-universal-date (date)
  (multiple-value-bind (year month day) (decode-date date)
    (encode-universal-time 59 59 23 day month year (time-zone))))

(defun make-day-date-range (date)
  (when date
    (values (least-universal-date date)
            (greatest-universal-date date))))

(defun get-generic-actions-data (grid &key filter search)
  (declare (ignore search))
  (multiple-value-bind (start-time end-time) (make-day-date-range (date grid))
    (find-docs 
     'vector
     (lambda (doc)
       (cond ((and (get-val doc 'channel-user)
                   (not (stringp (get-val doc 'channel-user)))
                   (not (match-context-entities (get-val doc 'channel-user))))
              nil)
             ((and start-time end-time
                   (or (not (scheduled-date doc))
                       (not (<= start-time
                                (scheduled-date doc)
                                end-time))))
              nil)
             ((equal filter 'with-audit-data)
              doc)
             ((equal filter 'completed-actions)
              (if (string-equal (get-val doc 'action-status) "Completed")
                  doc))
             ((equal filter 'pending-actions)
              (if (string-equal (get-val doc 'action-status) "Pending")
                  doc))
             ((equal filter '24-hours)
              (if (string-equal (get-val doc 'action-status) "Pending")
                  (if (and (> (scheduled-date doc) (get-universal-time))
                           (< (scheduled-date doc) (+ (get-universal-time) (* 60 60 24))))
                      doc)))
             ((equal filter '7-days)
              (if (string-equal (get-val doc 'action-status) "Pending")
                  (if (and (> (scheduled-date doc) (get-universal-time))
                           (< (scheduled-date doc) (+ (get-universal-time) (* 60 60 (* 24 7)))))
                      doc)))
             ((equal filter '30-days)
              (if (string-equal (get-val doc 'action-status) "Pending")
                  (if (and (> (scheduled-date doc) (get-universal-time))
                           (< (scheduled-date doc) (+ (get-universal-time) (* 60 60 (* 24 30)))))
                      doc)))
             ((equal filter 'all-actions)
              doc)
             (t 
              (if (not (string-equal 
                        (get-val doc 'doc-status) "superseded"))
                  (if (string-equal (get-val doc 'action-status) "Pending")
                      doc)))))
     (generic-actions-collection))))

(defmethod get-rows ((grid generic-actions-grid))
  (setf (rows grid)
	(get-generic-actions-data grid 
                          :filter (grid-filter grid)  
                          :search (search-term grid))))

(defclass generic-action-logs-grid (grid)
  ((current-doc :initarg nil))
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

(defun determine-current-image (doc)
  (with-parameters (action)
    (cond ((not (equal action "save"))
           (setf (session-value 'uploaded-image) nil)
           (image-url doc))
          ((or (session-value 'uploaded-image)
               (image-url doc))))))

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
                              :icon "card--pencil"))
        (image (determine-current-image row)))
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
                                                          (get-val 
                                                           (get-val row 'channel-user)
                                                           'user-id)))
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
                                        (if (not (empty-p (get-val row 'post-url)))
                                                      
                                                      (format nil "~A ~A" 
                                                              (get-val row 'action-content)
                                                              (get-val row 'post-url))
                                                      (get-val row 'action-content)))
                                    :required t
                                    :type :textarea)
                                   (:div "Characters:"
                                         (:span :id "message-length"
                                                (fmt "~a~@[ (including the image)~]"
                                                     (+ (length 
                                                         (or 
                                                          (parameter "action-content") 
                                                          (if (not (empty-p (get-val row 'post-url)))
                                                      
                                                              (format nil "~A ~A" 
                                                                      (get-val row 'action-content)
                                                                      (get-val row 'post-url))
                                                              (get-val row 'action-content))))
                                                        (if image
                                                            23
                                                            0))
                                                     image)))
                                   (defer-js
                                       "$('[name=\"action-content\"], #file').bind('change input propertychange',
function() {
var s = shortifyString($('[name=\"action-content\"]').val());
var length = s.length;
if ($('#current-image').length || $('#file').val())
  length = length + 23 + ' (including the image)';   
$('#message-length').text(length);
$('#processed-content').text(s)})")))
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
                                    (esc 
                                     (shortify-string 
                                      (if (empty-p (get-val row 'post-url))
                                          (or  (parameter "action-content") 
                                               (get-val row 'action-content))
                                          (format nil "~A ~A" 
                                                  (or (parameter "action-content") 
                                                      (get-val row 'action-content))
                                                  (format-short-url (short-url row)))))))))

                         (render 
                          form-section
                          :label "Add Image"
                          :input
                          (with-html-string
                            (when image
                              (htm
                               (:input :type "hidden"
                                       :id "current-image"
                                       :name "current-image" :value "t")
                               (:div :id "image"
                                     (:img :src 
                                           (format nil "/dyb/images/~a" 
                                                   (file-namestring image))
                                           :width 250
                                           :height 250)
                                     (:button :id "remove-image"
                                              :class "btn btn-info"
                                              "Remove image"))
                               (defer-js
                                   "$('#remove-image').click(function(){$('#current-image').remove();
                                      $('#image').remove();
                                      $('[name=\"action-content\"]').trigger('propertychange');})")))
                            (:input :type "file" :name "file" :id "file"
                                    :style "display: inline-block;")))
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
                                    :required t)))
                         (render 
                          form-section
                          :label "Select Time"
                          :input (with-html-string
                                   (render-edit-field   
                                    "scheduled-time"
                                    (or (parameter "scheduled-time")
                                        (if (get-val row 'scheduled-date)
                                            (format-universal-time 
                                             (get-val row 'scheduled-date)))
                                        "00:00"
                                        )
                                    :type :time
                                    :required t)
                                   
                                   (str "hh:mm")))

                         (if (get-val row 'id)
                             (htm
                              (:input :type "hidden" :name "action-status" 
                                 :value (get-val row 'action-status)))
                             
                             (htm
                              (:input :type "hidden" :name "action-status" 
                                 :value "Pending")))))))
            ("Action Logs" ,
             (with-html-string
               (:div :class "section _100" 
                     (let* ((columns
                             (list
                              (make-instance 'grid-column
                                             :name 'label)
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
                        
                       (render logs-grid)))))))
    (render tabs)))

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
                                                (replace-all name " " ""))
                                       *tmp-directory*)))
        (rename-file path new-path)
        (when (probe-file new-path)
          (make-auth-dispathcer new-path)
          new-path)))))

(defun parse-action-date ()
  ;; Add better error checking
  (multiple-value-bind (year month day)
      (decode-date-string (parameter "scheduled-date"))
    (multiple-value-bind (hour minute second)
        (decode-time-string (parameter "scheduled-time"))
      (handler-case
          (encode-universal-time second minute hour day month year (time-zone))
        (t (c)
          (grid-error (princ-to-string c)))))))

(defun handle-image-upload (doc)
  ;; Upload the image early and store into a session parameter
  ;; so that upon any errors the user won't have to reupload it
  (with-parameters (file current-image)
    (cond (file
           (setf (session-value 'uploaded-image) (handle-upload file)))
          ((not current-image)
           (setf (session-value 'uploaded-image) nil))
          (doc
           (image-url doc)))))

(defmethod handle-action ((grid generic-actions-grid) (action (eql 'save)))
  (setf (error-message grid) nil)
  (let* ((doc (editing-row grid))
         (image (handle-image-upload doc)))
    (when (and doc
               (equal (parameter "form-id") "schedule-action-form")
               (member (parameter "service") '("twitter" "Facebook" "LinkedIn")
                       :test #'equalp))
      (when (empty-p (parameter "scheduled-date"))
        (grid-error "Please enter a valid date."))
      (when (empty-p (parameter "service"))
        (grid-error "Please enter a channel to post to."))
      (when (empty-p (parameter "channel-user"))
        (grid-error "Please enter a user to post as."))
      (let* ((action-content (sanitize-string (parameter "action-content")))
             (short-content (shortify-string action-content))
             (len (+ (length short-content)
                     (if image 
                         23
                         0)
                     (if (not-empty-p (parameter "post-url"))
                         20
                         0)))
             (to-user nil)
             (date-time (parse-action-date)))
        (when (and (equalp (parameter "service") "twitter")
                   (> len 140))
          (grid-error "Message too long - ~A. (Remember that image URLs are also counted.)"
                      len))
        (cond ((xid doc)
               (synq-edit-data doc)
               (setf
                (channel-user doc) (get-channel-user-by-user-id 
                                    (parameter "channel-user")
                                    (parameter "service"))
                (get-val doc 'action-content) action-content
                (get-val doc 'action-status) "Pending"
                (post-type doc) (parameter "service")
                (from-user-id doc) (parameter "channel-user")
                (image-url doc) image
                (scheduled-date doc) date-time
                (image-url doc) image)
               (setf (get-val doc 'processed-content)
                     short-content)
               (setf (get-val 
                      doc 
                      'action-log)
                     nil)
               (persist doc))
              (t
               (persist (make-generic-action
                         (get-channel-user-by-user-id 
                          (parameter "channel-user")
                          (parameter "service"))
                         nil 
                         (parameter "service")
                                    
                         (parameter "channel-user") 
                         to-user 
                         (parameter "action-type")
                         action-content
                         short-content
                         "Timed"
                         date-time
                         :image-url image
                         :post-url (parameter "post-url")
                         :action-status (parameter "action-status"))))))
      (finish-editing grid))))


(defclass date-selector (ajax-widget)
  ((grid :initarg :grid
         :initform nil
         :accessor grid)))
(defmethod render ((widget date-selector) &key)
  (let* ((raw-date (get-parameter "date"))
         (date (and raw-date
                    (parse-date raw-date))))
    (setf (date (grid widget)) date)
    (with-html
      (:label "Date:"
              (:input :type "text"
                      :class "datepicker"
                      :id "grid-date"
                      :value (and (date (grid widget))
                                  (format-universal-date (date (grid widget))))
                      :onchange (js-render widget (js-value "grid-date"))))
      (defer-js "$('.datepicker').datepicker({dateFormat: 'dd M yy'})"))))

(defmethod action-handler ((widget date-selector))
  (let* ((raw-date (post-parameter "grid-date"))
         (date (and raw-date
                    (parse-date raw-date))))
    (when raw-date
      (setf (date (grid widget)) date)
      (update-table (grid widget)))))
