(in-package :ems)

(define-easy-handler (service-user-page :uri "/ems/service-users") ()
  (let* ((columns
           (list
            
            (make-instance 'grid-column
                           :name 'entity
                           :header "Entity"
                           :printer 'print-entity-name)
            (make-instance 'grid-column
                           :name 'service-user-name
                           :header "User Name")
            (make-instance 'grid-column
                           :name 'service-user-type
                           :header "User Type")
            (make-instance 'grid-column
                           :name 'user-id
                           :header "User Id" )))
         (grid (make-widget 'service-user-grid :name "service-user-gridc"
                                       :columns columns
                                       :edit-inline nil
                                       :title "Service User"
                                       :row-object-class 'service-user)))
    (setf (sort-key-function grid)
          (lambda (doc)
            (if (stringp (get-val doc 'service-user-name))
                (get-val doc 'service-user-name)
                (write-to-string (get-val doc 'service-user-name)))))
    
    (render (make-widget 'page :name "service-user-page")
            :body (render-to-string grid))))