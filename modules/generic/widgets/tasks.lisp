(in-package :dyb)

(defclass assign-task-action (grid-editor)
  ()
  (:metaclass widget-class))

(defmethod render ((widget assign-task-action) &key)
  (let ((row (editing-row (grid widget)))
        (assign-form (make-widget 'html-simple-framework-form
                                  :name "assign-task-form"
                                  :grid-size 12
                                  :form-id "assign-task-form"
                                  :action "assign-task"
                                  :action-title "Assign Task"))
        (form-section (make-widget 'form-section
                                   :name "form-section")))


    (setf (ajax-render-widget assign-form) (editor (grid widget)))
    (render assign-form
            :content
            (with-html-string
              (render form-section
                      :label "Entity"
                      :input
                      (with-html-string
                        (render-edit-field
                         "entityx"
                         (get-val (get-val (get-val row 'channel-user) 'entity) 'entity-name)
                         :type :span)
                        (:input :type "hidden" :name "entity"
                                :value (get-val (get-val (get-val row 'channel-user) 'entity) 'xid))))
              (render form-section
                      :label "Description"
                      :input
                      (with-html-string
                        (render-edit-field
                         "task-description"
                         (parameter "task-description")
                         :required t
                         :type :text)))
              (render form-section
                      :label "Instructions"
                      :input
                      (with-html-string
                        (render-edit-field
                         "task-instructions"
                         (or (parameter "task-instructions")
                             (raw-post-id
                              row
                              (cond ((string-equal (post-type row) "twitter")
                                     'twitter)
                                    ((string-equal (post-type row) "facebook")
                                     'facebook)
                                    ((string-equal (post-type row) "linkedin")
                                     'twitter)
                                    ((string-equal (post-type row) "social mention")
                                     'social-mention)
                                    )))
                         :required t
                         :type :textarea)))

              (render
               form-section
               :label "Status"
               :input (with-html-string
                        (render-edit-field
                         "task-status"
                         "Assigned"
                         :type :span)
                        (:input :type "hidden" :name "task-status"
                                :value "Assigned")))
              (render
               form-section
               :label "Assigner"
               :input (with-html-string
                        (render-edit-field
                         "assigning-user"
                         (email (current-user))
                         :type :span)
                        (:input :type "hidden" :name "assigning-user"
                                :value (email (current-user)))))

              (render
               form-section
               :label "Assignee"
               :input (with-html-string
                        (render-edit-field
                         "assigned-user"
                         (parameter "assigned-user")
                         :data (user-list)
                         :type :select)))
              (render
               form-section
               :label "Assigned Date"
               :input (with-html-string
                        (render-edit-field
                         "assigned-date"
                         (or (parameter "assigned-date")
                             (format-universal-date-time  (get-universal-time)))
                         :type :span)))

              (render
               form-section
               :label "Scheduled Date"
               :input (with-html-string
                        (render-edit-field
                         "scheduled-date"
                         (or (parameter "scheduled-date")
                             (current-date))
                         :type :date
                         :required t)))))))

(defmethod handle-action ((grid generic-grid) (action (eql :assign-task-form)))
  (setf (action-widget grid)
        (make-widget 'assign-task-action :name "assign-task-action-form"
                                         :grid grid)))

(defmethod action-handler ((widget assign-task-action))
  (when (equalp (parameter "action") "assign-task")
    (let* ((grid (grid widget))
           (entity-id (ensure-parse-integer (parameter "entity")))
           (entity (and entity-id
                        (get-entity-by-id entity-id)))
           (new-doc (and entity
                         (make-task entity
                                    (parameter "task-description")
                                    (get-user (parameter "assigning-user"))
                                    (get-user (parameter "assigned-user"))
                                    (parameter "scheduled-date")
                                    :task-instructions
                                    (parameter "task-instructions")
                                    :assigned-date
                                    (parameter "assigned-date")
                                    :task-status
                                    (parameter "task-status")))))
      (cond (new-doc
             (persist new-doc)
             (finish-editing grid))
            ((not entity)
             (setf (error-message  grid)
                   "Please supply an entity."))))))
