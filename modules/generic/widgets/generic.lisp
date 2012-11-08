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


(defclass fb-post-comment-form (ajax-widget)
  ((grid :initarg :grid
         :initform nil
         :accessor grid))
  (:metaclass widget-class))


(defmethod render ((widget fb-post-comment-form) &key )
  (let* ((comment-form (make-widget 'html-simple-framework-form 
                                    :name "facebook-post-comment-form"
                                    :grid-size 12
                                    :form-id "facebook-post-comment-form"
                                    :action "post-facebook-comment"
                                    :action-title "Comment"))
         (form-section (make-widget 'form-section
                                    :name "form-section"))
         (current-post (editing-row (get-val widget 'grid)))
         (post-id (gpv current-post :id)))

    (with-html 
      (if (parameter "action")
          (render comment-form
                  :content
                  (with-html-to-string ()
                    (:div 
                     (:input :type "hidden" :name "post-id" 
                             :value post-id)
                     (:input :type "hidden" :name "from-user-id" 
                             :value (gpv current-post :from :id))
                     (:input :type "hidden" :name "to-user-id" 
                             :value (gpv current-post :to :id))
                     (:input :type "hidden" :name "action-type" 
                             :value "Facebook Comment")

                     (render form-section 
                             :label "Comment"
                             :input 
                             (with-html-to-string ()
                               (render-edit-field
                                "comment" 
                                (parameter "comment")
                                :required t
                                :type :textarea))))))))
    (open-dialog widget (grid widget))))

(defmethod action-handler ((widget fb-post-comment-form))

  (when (string-equal (parameter "action") "post-facebook-comment")
   (break "~A" (parameter "action"))
    #|
    (comment-facebook (get-val widget 'current-post)
                      (if (get-val (get-val widget 'current-post) 'to)
                          (get-val (first (get-val (get-val widget 'current-post) 'to)) 'id)
                          (get-val (get-val (get-val widget 'current-post) 'from) 'id))
                      (parameter "comment"))
    |#
    (defer-js (format nil "$('#~a').dialog('close')" (name widget)))
    ))

(defmethod handle-action ((grid generic-grid) (action (eql 'post-facebook-comment)))
  (break "fuck"))
(defmethod handle-action ((grid generic-grid) (action (eql 'facebook-comment)))

  (setf (action-widget grid)
        (make-widget 'fb-post-comment-form 
                     :grid grid 
                     :name "facebook-comment-form")))



(defmethod handle-action (grid (action (eql 'like)))
 (break "?") )


