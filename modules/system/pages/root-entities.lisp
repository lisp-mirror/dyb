(in-package :dyb)

(define-easy-handler (root-entities-page :uri "/dyb/root-entities") ()
  (let* ((columns
           (list
            (make-instance 'grid-column
                           :name 'entity-name
                           :header "Name")
            ))
         (grid (make-widget 'root-entities-grid :name "root-entities-gridxx"
                                       :columns columns
                                       :edit-inline nil
                                       :title "Root-Entities"
                                       :row-object-class 'entity)))
    
    (render (make-widget 'page :name "root-entities-page")
            :body (render-to-string grid))))