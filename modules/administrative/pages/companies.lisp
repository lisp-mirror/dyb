(in-package :ems)

(define-easy-handler (companies-page :uri "/ems/companies") ()
  (let* ((columns
           (list
            (make-instance 'grid-column
                           :name 'entity
                           :header "Company Name"
                           :printer 'print-entity-name)
            (make-instance 'grid-column
                           :name 'registration-no
                           :header "Registration No")
            (make-instance 'grid-column
                           :name 'vat-no
                           :header "Vat No")
            ))
         (grid (make-widget 'company-grid :name "company-gridx"
                                       :columns columns
                                       :edit-inline nil
                                       :title "Companies"
                                       :row-object-class 'company)))
    (render (make-widget 'page :name "companies-page")
            :body (render-to-string grid))))