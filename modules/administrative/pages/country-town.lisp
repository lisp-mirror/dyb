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
                         :name 'municipalities
                         :header "Municipality")            
            (make-instance 'grid-column
                         :name 'town
                         :header "Town")          
          (make-instance 'grid-column
                         :name 'longitude  
                         :header "Longitude")          
          (make-instance 'grid-column
                         :name 'latitude
                         :header "Latitude")))
         (grid (make-widget 'country-town-grid :name "country-town-grid"
                                       :columns columns
                                       :edit-inline nil
                                       :title "Country/Town")))
    (render (make-widget 'page :name "country-town-page")
            :body (render-to-string grid))))


