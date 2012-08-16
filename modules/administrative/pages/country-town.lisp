(in-package :ems)

(define-easy-handler (country-town-page :uri "/ems/country-town") ()
  (let* ((columns
          (list
           (make-instance 'grid-column
                          :name 'country
                          :header "Country")            
           (make-instance 'grid-column
                          :name 'province
                          :header "Province")
           (make-instance 'grid-column
                          :name 'town
                          :header "Town")     
           (make-instance 'grid-column
                          :name 'guess-p
                          :header "Guess")                      
           (make-instance 'grid-column
                          :name 'longitude  
                          :header "Longitude"
                          :printer #'gps-cord-formatter)          
           (make-instance 'grid-column
                          :name 'latitude
                          :header "Latitude"
                          :printer #'gps-cord-formatter)))
         (grid (make-widget 'country-town-grid :name "country-town-gridxx"
                            :columns columns
                            :edit-inline nil
                            :title "Country/Town"
                            :row-object-class 'country-town)))
    (setf (sort-key-function grid)
          (lambda (doc)
            (format nil "~A ~A ~A"  
                    (get-val doc 'country)
                    (get-val doc 'province)
                    (get-val doc 'province))))
    (render (make-widget 'page :name "country-town-page")
            :body (render-to-string grid))))


