(in-package #:ems)

(defclass tasks-grid (grid)
  ()
  (:default-initargs :edit-inline nil))

(defun get-tasks-data (grid &key filter search)
  (declare (ignore grid search))
  (find-docs 'vector
              (lambda (doc)
                (if (match-context-entities doc)
                    (cond ((equal filter 'with-audit-data)
                           doc)
                          (t 
                           (if (not (string-equal (get-val doc 'doc-status) "superseded"))
                               doc)))))
              (tasks-collection)))

(defmethod get-rows ((grid tasks-grid))
  (setf (rows grid)
	(get-tasks-data grid 
                        :filter (grid-filter grid)  
                        :search (search-term grid))))

(defmethod render-row-editor ((grid tasks-grid) row)
  (let ((comment-form (make-widget 'html-framework-form :name "task-form"
                                       :grid-size 12
                                       :header "Assign a Task"
                                       :form-id "task-form"
                                       ))
        (form-section (make-widget 'form-section
                                   :name "form-section")))
    
    (render comment-form
                    :grid grid
                    :content
                    (with-html-to-string ()
                      
                      
                      (render form-section 
                       :label "Entity"
                       :input 
                       (if (get-val row 'entity)
                           (with-html-to-string ()
                             (render-edit-field
                              "entityx"  
                              (get-val (get-val row 'entity) 'entity-name)
                              :type :span)
                             (:input :type "hidden" :name "entity" :value (get-val (get-val row 'entity) 'xid)))
                           (with-html-to-string ()
                             (render-edit-field
                              "entity" 
                              (get-val (get-val row 'entity) 'xid)
                              :data (entity-list)
                              :required t
                              :blank-allowed t
                              :type :select))))
                      (render form-section 
                              :label "Description"
                              :input 
                              (with-html-to-string ()
                                (render-edit-field
                                 "task-description" 
                                 (or (parameter "task-description") 
                                     (get-val row 'task-description))
                                 :required t
                                 :type :text)))
                      (render form-section 
                              :label "Instructions"
                              :input 
                              (with-html-to-string ()
                                (render-edit-field
                                 "task-instructions" 
                                 (or (parameter "task-instructions") 
                                     (get-val row 'task-instructions))
                                 :required t
                                 :type :textarea)))
                      
                      (render 
                       form-section
                       :label "Status"
                       :input (with-html-to-string ()
                             (render-edit-field
                              "task-status" 
                              (get-val row 'task-status)
                              :data (list (list "Assigned" "Assigned")
                                          (list "In Progress" "In Progress")
                                          (list "Completed" "Completed")
                                          (list "Abandoned" "Abandoned"))
                              :required t
                              :blank-allowed t
                              :type :select)))
                      (render 
                       form-section
                       :label "Assigner"
                       :input (with-html-to-string ()
                                (render-edit-field 
                                 "assigning-user"
                                 (or (parameter "assigning-user")
                                     (get-val row 'assigning-user))
                                 :type :span)))

                      (render 
                       form-section
                       :label "Assignee"
                       :input (with-html-to-string ()
                                (render-edit-field 
                                 "assigned-user"
                                 (or (parameter "assigned-user")
                                     (get-val row 'assigned-user))
                                 :data (user-list)
                                 :type :select)))
                      
                      (render 
                       form-section
                       :label "Assigned Date"
                       :input (with-html-to-string ()
                                (render-edit-field 
                                 "assigned-date"
                                 (or (parameter "assigned-date")
                                     (if (get-val row 'assigned-date)
                                         (format-universal-date 
                                          (get-val row 'assigned-date))))
                                 :type :span)))

                      (render 
                       form-section
                       :label "Scheduled Date"
                       :input (with-html-to-string ()
                                (render-edit-field 
                                 "scheduled-date"
                                 (or (parameter "scheduled-date")
                                     (if (get-val row 'scheduled-date)
                                         (format-universal-date 
                                          (get-val row 'scheduled-date))))
                                 :type :date
                                 :required t)))

                      (render 
                       form-section
                       :label "Completed Date"
                       :input (with-html-to-string ()
                                (render-edit-field 
                                 "completed-date"
                                 (or (parameter "completed-date")
                                     (if (get-val row 'completed-date)
                                         (format-universal-date 
                                          (get-val row 'completed-date))))
                                 :type :date)))
                      
                      
                      ))))





(defmethod handle-action ((grid tasks-grid) (action (eql 'save)))
  (setf (error-message grid) nil)

  (when (and (string-equal (parameter "form-id") "task-form"))
    (let ((new-doc (editing-row grid)))
      (synq-edit-data new-doc)
      (setf (get-val new-doc 'entity) 
            (get-entity-by-id 
             (if (stringp (parameter "entity"))
                 (parse-integer 
                  (parameter "entity"))
                 (parameter "entity"))))
      (unless (get-val new-doc 'xid)
        (setf (get-val new-doc 'assigned-date) (get-universal-time)))
      (persist new-doc))
    (finish-editing grid)
    ))
 