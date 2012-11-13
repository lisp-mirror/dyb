(in-package :dyb)

(defclass generic-actions-grid (grid)
  ((parent-grid :initarg :parent-grid)
   (current-doc :initarg nil))
  (:default-initargs :edit-inline nil))

(defun get-generic-actions-data (grid &key filter search)
  (declare (ignore grid search))
  (find-docs 'vector
              (lambda (doc)
             ;;   (if (match-context-entities (get-val doc 'payload) ))
                (cond ((equal filter 'with-audit-data)
                           doc)
                          (t 
                           (if (not (string-equal (get-val doc 'doc-status) "superseded"))
                               doc))))
              (generic-actions-collection)))

(defmethod get-rows ((grid generic-actions-grid))
  (setf (rows grid)
	(get-generic-actions-data grid 
                          :filter (grid-filter grid)  
                          :search (search-term grid))))

(defmethod render-row-editor ((grid generic-actions-grid) row)
  (let ((comment-form (make-widget 'html-framework-form :name "schedule-action-formx"
                                                        :grid-size 12
                                                        :header "Schedule Action Against Post"
                                                        :form-id "schedule-action-form"
                                                        :form-data t
                                                        :ajax-submit nil))
        (form-section (make-widget 'form-section
                                   :name "form-section"))
        (current-doc (get-val grid 'current-doc))
        (channel-users (make-widget 'channel-user-select 
                                    :name "channel-user-select-dropown")))
    
    (render comment-form
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
                        :label "Post To"
                        :input
                        (with-html-string
                          (render service)))
                (render form-section
                        :label "Channel User"
                        :input
                        (with-html-string
                          (render channel-user))))

                
              (render form-section 
                      :label "Post"
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
               :label "Image"
               :input (with-html-string
                        ;; (render-edit-field 
                        ;;  "image-url"
                        ;;  (or (parameter "image-url")
                        ;;      (get-val row 'image-url))
                                 
                        ;;  :required t)
                        (when (image-url row)
                          (htm (:div (:img :src (format nil "/dyb/images/~a" (file-namestring  (image-url row)))
                                           :width 250
                                           :height 250))))
                        (:input :type "file" :name "file" :id "file"
                                :style "display: inline-block;")))
              (render 
               form-section
               :label "Post Url"
               :input (with-html-string
                        (render-edit-field 
                         "post-url"
                         (or (parameter "post-url")
                             (get-val row 'post-url))
                                 
                         :required nil)))
              (render 
               form-section
               :label "Short Url"
               :input (with-html-string
                        (render-edit-field 
                         "short-url"
                         (when (short-url row)
                           (format-short-url (short-url row)))
                         :type :span)))
              (render 
               form-section
               :label "Scheduled Date"
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
               :label "Scheduled Time"
               :input (with-html-string
                        (render-edit-field 
                         "scheduled-time"
                         (or (parameter "scheduled-time")
                             (if (get-val row 'scheduled-date)
                                 (multiple-value-bind 
                                       (second minute hour day month year)
                                     (decode-universal-time 
                                      (get-val row 'scheduled-date))
                                   (declare (ignore second day month year))
                                   (format nil "~2,'0d:~2,'0d" hour minute)))
                             "00:00"
                             )
                         :type :text
                         :required t) 
                        (str "hh:mm")))))))

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
      (let ( ;; (from-user (if (and (string-equal (parameter "service") "facebook")
            ;;                     (parameter "channel-user"))
            ;;                (get-facebook-access-token-by-user (parameter "channel-user"))))
            (to-user nil)
            (image (handle-upload (post-parameter "file")))
            (doc (editing-row grid))
            (short-url (if (blank-p (parameter "post-url"))
                           (make-short-url (parameter "post-url"))
                                     )))
        (when t ;; (or from-user to-user)
          (let ((date-time nil))
            (multiple-value-bind (year month day)
                (decode-date-string (parameter "scheduled-date"))
              (multiple-value-bind (second minute hour)
                  (decode-time-string (format nil "~A:00" (parameter "scheduled-time")))
                (when second
                  (setf date-time 
                        (encode-universal-time second minute hour day month year))
                  (cond ((xid doc)
                         (synq-edit-data doc)
                         (setf (post-type doc) (parameter "service")
                               (from-user-id doc) (parameter "channel-user")
                               (scheduled-date doc) date-time
                               (image-url doc) (or image
                                                   (image-url doc))
                               (short-url doc) short-url)
                         (persist doc))
                        (t
                         (persist (make-generic-action nil
                                                       (parameter "service")
                                                       (parameter "channel-user") 
                                                       to-user
                                                       (parameter "action-type")
                                                       (parameter "action-content")
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
 
