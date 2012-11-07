(in-package :dyb)

(define-easy-handler (companies-page :uri "/dyb/companies") ()
  (let* ((columns
           (list
            (make-instance 'grid-column
                           :name 'entity
                           :header "Entity")
            (make-instance 'grid-column
                           :name 'company-name
                           :header "Name")
            (make-instance 'grid-column
                           :name 'contact-person
                           :header "Contact Person")
            (make-instance 'grid-column
                           :name 'contact-number
                           :header "Contact Number")
            ))
         (grid (make-widget 'companies-grid :name "companies-gridxxx"
                                       :columns columns
                                       :edit-inline nil
                                       :title "Companies"
                                       :row-object-class 'company)))
    (render (make-widget 'page :name "companies-page")
            :body (render-to-string grid))))