(in-package :ems)

(define-easy-handler (period-page :uri "/ems/reporting-periods") ()
  (let* ((columns
           (list
            
            (make-instance 'grid-column
                           :name 'entity
                           :header "Entity"
                           :printer 'print-entity-name)
            (make-instance 'grid-column
                           :name 'period-name
                           :header "Period Name")
            (make-instance 'grid-column
                           :name 'period-type
                           :header "Period Type")
            (make-instance 'grid-column
                           :name 'description
                           :header "Description" )
            (make-instance 'grid-column
                           :name 'start-date
                           :printer #'format-universal-date
                           :header "Start Date")
            (make-instance 'grid-column
                           :name 'end-date
                           :printer #'format-universal-date
                           :header "End Date")
            (make-instance 'grid-column
                           :name 'status
                           :header "Status")))
         (grid (make-widget 'period-grid :name "period-gridxz"
                                       :columns columns
                                       :edit-inline nil
                                       :title "Reporting Period"
                                       :row-object-class 'period)))
    (setf (sort-key-function grid)
          (lambda (doc)
                      (format nil "~A ~A"  
                              (get-val (get-val doc 'entity) 'entity-name)
                              (get-val doc 'period-name))
                      ))
    (render (make-widget 'page :name "period-page")
            :body (render-to-string grid))))