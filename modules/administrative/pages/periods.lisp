(in-package :ems)

(define-easy-handler (period-page :uri "/ems/reporting-periods") ()
  (let* ((columns
           (list
            (make-instance 'grid-column
                           :name 'key
                           :header "Key")
            (make-instance 'grid-column
                           :name 'entity-relationship
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
         (grid (make-widget 'period-grid :name "period-gridxxx"
                                       :columns columns
                                       :edit-inline nil
                                       :title "Reporting Period"
                                       :row-object-class 'period)))
    (render (make-widget 'page :name "period-pagexx")
            :body (render-to-string grid))))