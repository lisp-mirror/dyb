(in-package :dyb)

(define-easy-handler (generic-scheduler-page :uri "/dyb/generic-scheduler") ()
  (let* ((columns
          (list
           (make-instance 'grid-column
                          :name 'post-type
                          :header "Channel")
           (make-instance 'grid-column
                          :name 'channel-user
                          :header "Channel Id"
                          :printer (lambda (user)
                                     (get-val user 'channel-user-name)))
           (make-instance 'grid-column
                          :name 'user
                          :header "User")
           (make-instance 'grid-column
                          :name 'schedule-type
                          :header "Scheduled Type")
           (make-instance 'grid-column
                          :name 'action-type
                          :header "Action Type")                 
           (make-instance 'grid-column
                          :name 'action-content
                          :header "Message"
                          :width "40%")
           (make-instance 'grid-column
                          :name 'scheduled-date
                          :header "Scheduled Date"
                          :printer #'format-universal-date-time)
           
           (make-instance 'grid-column
                          :name 'action-status
                          :header "Status")))
         (grid (make-widget 'generic-actions-grid :name "generic-actions-gridx"
                            ;;:columns columns
                            :title "Schedule Messages"
                            :row-object-class 'generic-action))
         (date-selector (make-widget 'date-selector
                                     :grid grid)))

    
    (setf (get-val grid 'columns) columns)
    (setf (sort-keys grid) '(5 scheduled-date))
    (setf (initial-sort-column grid) '(5 :ascending))
    (setf (toolbar-widget grid) date-selector)

    (render (make-widget 'page :name "generic-scheduler-page")
            :body (with-html-string
                    
                    (str (render grid))))))


