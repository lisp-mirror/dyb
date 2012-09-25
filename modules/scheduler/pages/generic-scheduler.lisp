(in-package #:ems)

(define-easy-handler (generic-scheduler-page :uri "/ems/generic-scheduler") ()
  (let* ((columns
                             (list
                              (make-instance 'grid-column
                                             :name 'pid
                                             :header "Post Id")
                              (make-instance 'grid-column
                                             :name 'action
                                             :header "Action")
                              (make-instance 'grid-column
                                             :name 'scheduled-date
                                             :header "Scheduled Date"
                                             )))
         (grid (make-widget 'generic-actions-grid :name "generic-action-grid"
                                       ;;:columns columns
                                       :edit-inline nil
                                       :title "Schedule Actions"
                                       :row-object-class 'generic-action)))
    (setf (get-val grid 'columns) columns)
    
    (render (make-widget 'page :name "generic-scheduler-page")
            :body (with-html-to-string ()
                    
                    (str (render grid))))))


