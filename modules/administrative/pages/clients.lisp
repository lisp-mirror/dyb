(in-package :dyb)

(define-easy-handler (clients-page :uri "/dyb/clients") ()
  (let* ((columns
           (list
            (make-instance 'grid-column
                           :name 'entity-name
                           :header "Name")
            ))
         (grid (make-widget 'clients-grid :name "clients-gridxx"
                                       :columns columns
                                       :title "Clients"
                                       :row-object-class 'entity)))
    
    (render (make-widget 'page :name "clients-page")
            :body (render-to-string grid))))
