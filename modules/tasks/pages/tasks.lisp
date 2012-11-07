(in-package #:ems)

(define-easy-handler (tasks-page :uri "/dyb/tasks") ()
  (let* ((columns
          (list
                              
           (make-instance 'grid-column
                          :name 'task-description
                          :header "Description")
           (make-instance 'grid-column
                          :name 'assigned-user
                          :header "Assigned User")
           (make-instance 'grid-column
                          :name 'task-status
                          :header "Status")
           (make-instance 'grid-column
                          :name 'scheduled-date
                          :header "Scheduled Date"
                          :printer #'format-universal-date
                          )
           (make-instance 'grid-column
                          :name 'completed-date
                          :header "Completed Date"
                          :printer #'format-universal-date
                          )
           ))
         (grid (make-widget 'tasks-grid :name "tasks-grid"
                            :edit-inline nil
                            :title "Schedule Actions"
                            :row-object-class 'task)))

    (setf (get-val grid 'columns) columns)
    
    (render (make-widget 'page :name "tasks-page")
            :body (with-html-to-string ()
                    
                    (str (render grid))))))


