(in-package :ems)

(define-easy-handler (companies-page :uri "/ems/companies") ()
  (let* ((columns
           (list
            (make-instance 'grid-column
                           :name 'name
                           :header "Name")
            ))
         (grid (make-widget 'companies-grid :name "companies-grid"
                                       :columns columns
                                       :edit-inline nil
                                       :title "Companies")))
    (render (make-widget 'page :name "companies-page")
            :body (render-to-string grid))))