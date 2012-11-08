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
             ;;   (if (match-context-entities (get-val doc 'payload) ))
                (cond ((equal filter 'with-audit-data)
                           doc)
                          (t 
                           (if (not (string-equal (get-val doc 'doc-status) "superseded"))
                               doc))))
              (generic-post-collection)))

(defmethod get-rows ((grid generic-grid))
  (setf (rows grid)
	(get-generic-data grid 
                          :filter (grid-filter grid)  
                          :search (search-term grid))))

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


(defmethod action-handler ((widget post-form))
  
  (when (string-equal (parameter "action") "post-to-channel")  
    (if (string-equal (parameter "service") "facebook")
        (multiple-value-bind (result error-message)
            (post-facebook 
             (parameter "channel-user")
             (parameter "post-status"))
      
          (if error-message
              (setf (get-val widget 'message) error-message)
              (defer-js (format nil "$('#~a').dialog('close')" (name widget))))))
    ))



