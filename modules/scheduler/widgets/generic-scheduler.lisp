(in-package #:ems)

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
                                       ))
        (form-section (make-widget 'form-section
                                   :name "form-section"))
        (current-doc (get-val grid 'current-doc))
        (service-users (make-widget 'service-user-select 
                                    :name "service-user-select-dropown")))
    
    (render comment-form
                    :grid grid
                    :content
                    (with-html-to-string ()
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
                      

                      (destructuring-bind (service service-user)
                          (selects service-users)
                        (render service-users)
                        (setf (value service) (or (parameter "service")
                                                  (get-val row 'post-type)))
                        (setf (value service-user) (or (parameter "service-user")
                                                        (get-val row 'from-user-id)))
                        (render form-section
                                :label "Post To"
                                :input
                                (with-html-to-string ()
                                  (render service)))
                        (render form-section
                                :label "Service User"
                                :input
                                (with-html-to-string ()
                                  (render service-user))))

                      
                      (render form-section 
                              :label "Post"
                              :input 
                              (with-html-to-string ()
                                (render-edit-field
                                 "action-content" 
                                 (or (parameter "action-content") (get-val row 'action-content))
                                 :required t
                                 :type :textarea)))
                      (render 
                       form-section
                       :label "Scheduled Date"
                       :input (with-html-to-string ()
                                (render-edit-field 
                                 "scheduled-date"
                                 (get-val row 'scheduled-date)
                                 :type :date)
                                ))
                      
                      ))))





(defmethod handle-action ((grid generic-actions-grid) (action (eql 'save)))
  (setf (error-message grid) nil)

  (when (and (string-equal (parameter "form-id") "schedule-action-form"))
    (let ((from-user (if (and (string-equal (parameter "service") "facebook")
                              (parameter "service-user"))
                         (get-facebook-access-token-by-user (parameter "service-user"))))
          (to-user nil))
      (when (or from-user to-user)

        (if (xid (editing-row grid))
            (let ((new-doc (editing-row grid))
                  (old-doc (copy (editing-row grid))))
              (synq-edit-data new-doc)
              (setf (get-val new-doc 'post-type) (parameter "service"))
              (setf (get-val new-doc 'from-user-id) (parameter "service-user"))
              (persist new-doc :old-object old-doc))
            (persist (make-generic-action  nil
                                          (parameter "service")
                                          (parameter "service-user") 
                                          to-user
                                          (parameter "action-type")
                                          (parameter "action-content")
                                          (parameter "scheduled-date"))))
        (finish-editing grid))
      (unless (or from-user to-user)
          (setf (error-message grid) "User does not exist.")))))
 