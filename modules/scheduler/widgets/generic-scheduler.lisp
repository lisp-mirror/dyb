(in-package :dyb)

(defclass generic-actions-grid (grid)
  ((current-doc :initarg nil)
   (date :initarg :date
               :initform nil
               :accessor date))
  (:default-initargs :edit-inline nil))

(defmethod list-grid-filters ((grid generic-actions-grid))
  '(completed-actions
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
                                        (get-val row 'action-content))
                                    :required t
                                    :type :textarea)
                                   (:div "Characters:"
                                         (:span :id "message-length"
                                               (str (length (or (parameter "action-content") 
                                                                (get-val row 'action-content))))))
                                   (defer-js
                                       "$('[name=\"action-content\"]').bind('input propertychange',
function() {$('#message-length').text($(this).val().length)})")))
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
                                                 :height 250)))
                                     (render-edit-field 
                                        "image-file"
                                        (format nil "/dyb/images/~a" 
                                                         (file-namestring  
                                                          (or (parameter "image-file")
                                            (get-val row 'image-url))))
                                        
                                 
                                        :required nil))
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
                                            (format-universal-time 
                                             (get-val row 'scheduled-date)))
                                        "00:00"
                                        )
                                    :type :time
                                    :required t)
                                   
                                   (str "hh:mm")))
                         (render 
                          form-section
                          :label "Status"
                          :input (with-html-string
                                   (render-edit-field 
                                    "action-status"
                                    (or (parameter "action-status")
                                        (get-val row 'action-status))
                                    :type :select
                                    :data (list (list "Pending" "Pending")
                                                (list "Aborted" "Aborted")
                                                (list "Completed" "Completed"))
                                    :required t)))))))
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

(defvar *string-substitution*
  '((#\FIGURE_DASH "-")
    (#\EN_DASH "-")
    (#\EM_DASH "-")
    (#\HYPHEN "-")
    (#\MACRON "-")
    (#\DOUBLE_LOW_LINE "_")
    (#\COMBINING_GRAVE_ACCENT "'")
    (#\COMBINING_ACUTE_ACCENT "'")
    (#\RIGHT_SINGLE_QUOTATION_MARK "'")
    (#\LEFT_SINGLE_QUOTATION_MARK "'")
    (#\GREEK_TONOS "'")
    (#\SINGLE_HIGH-REVERSED-9_QUOTATION_MARK "'")
    (#\SINGLE_LOW-9_QUOTATION_MARK "'")
    (#\DOUBLE_ACUTE_ACCENT "\"")
    (#\LEFT_DOUBLE_QUOTATION_MARK "\"")
    (#\RIGHT_DOUBLE_QUOTATION_MARK "\"")
    (#\DOUBLE_LOW-9_QUOTATION_MARK "\"")
    (#\… "...")
    ))



(defun sanitize-string (string)
  (if #+sbcl(typep string 'simple-base-string)
      #-sbcl nil
      string
      (with-output-to-string (str)
        (loop for char across string
              for subst = (cadr (assoc char *string-substitution* :test #'char=))
              do (if subst
                     (write-string subst str)
                     (write-char char str))))))



(defmethod handle-action ((grid generic-actions-grid) (action (eql 'save)))
  (setf (error-message grid) nil)
  (when (and (string-equal (parameter "form-id") "schedule-action-form"))
    (when (empty-p (parameter "scheduled-date"))
      (grid-error "Please enter a valid date."))
    (when (empty-p (parameter "service"))
      (grid-error "Please enter a channel to post to."))
    (when (empty-p (parameter "channel-user"))
      (grid-error "Please enter a user to post as."))
    (let ((len (+ (length (parameter "action-content") )
                  (if (or (post-parameter "file") 
                          (blank-p (parameter "image-file"))) 
                      20
                      0) 
                  (if (blank-p (parameter "post-url")) 
                      20
                      0))))
      (if (and (string-equal (parameter "service") "twitter")
               (>= len 140))
          (grid-error "Message to long - ~A. (Remember that image and link url's are also counted.)"
                      len))
      (when (or (member (parameter "service") '("twitter" "Facebook" "LinkedIn")
                        :test #'string-equal))
        (let ((to-user nil)
              (image (if (post-parameter "file")
                         (handle-upload (post-parameter "file"))
                         (if (not (blank-p (parameter "image-file")))
                             nil
                             (format nil "~~/hunchentoot-upload/~A"
                                     (file-namestring (parameter "image-file"))))))
              (doc (editing-row grid))
              (short-url (if (blank-p (parameter "post-url"))
                             (make-short-url (parameter "post-url")))))
            
          (when doc
            (let ((date-time (parse-action-date)))
              (cond ((xid doc)
                     (synq-edit-data doc)
                     (setf
                      (channel-user doc) (get-channel-user-by-user-id 
                                          (parameter "channel-user")
                                          (parameter "service"))
                      (get-val doc 'action-content) 
                      (string-trim 
                       '(#\space #\tab #\newline 
                         #\linefeed #\return) 
                       (sanitize-string (parameter "action-content")))
                      (get-val doc 'action-status) (parameter "action-status")
                      (post-type doc) (parameter "service")
                      (from-user-id doc) (parameter "channel-user")
                      (get-val doc 'image-url) image
                      (scheduled-date doc) date-time
                      (image-url doc) image
                      (short-url doc) short-url)
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
                               (string-trim 
                                '(#\space #\tab #\newline 
                                  #\linefeed #\return) 
                                (sanitize-string (parameter "action-content")))
                               "Timed"
                               date-time
                               :image-url image
                               :post-url (parameter "post-url")
                               :short-url short-url
                               :action-status (parameter "action-status"))))))
            (finish-editing grid)))))))


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
