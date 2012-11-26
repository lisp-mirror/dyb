(in-package :dyb)

(define-easy-handler (generic-scheduler-page :uri "/dyb/generic-scheduler") ()
  (let* ((columns
          (list
           (make-instance 'grid-column
                          :name 'channel-user
                          :header "User"
                          :printer (lambda (user)
                                     (get-val user 'channel-user-name)))  
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
                          :printer #'format-universal-date)
           (make-instance 'grid-column
                          :name 'scheduled-date
                          :header "Scheduled Time"
                          :printer
                          (lambda (doc)
                            (unless (stringp doc)
                                                   
                              (multiple-value-bind 
                                    (second minute hour day month year)
                                  (decode-universal-time 
                                   doc)
                                (declare 
                                 (ignore second day month year))
                                (format nil "~2,'0d:~2,'0d" hour minute))))
                          )
           (make-instance 'grid-column
                          :name 'action-status
                          :header "Status")))
         (grid (make-widget 'generic-actions-grid :name "generic-actions-gridx"
                            ;;:columns columns
                            :edit-inline nil
                            :title "Schedule Messages"
                            :row-object-class 'generic-action)))
    (setf (get-val grid 'columns) columns)
    
    (render (make-widget 'page :name "generic-scheduler-page")
            :body (with-html-to-string ()
                    
                    (str (render grid))))))


