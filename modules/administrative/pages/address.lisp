(in-package :dyb)

(define-easy-handler (addresses-pagexx :uri "/dyb/bio-addresses") ()
  (let* ((columns
          (list
           (make-instance 'grid-column
                          :name 'key
                          :header "Entity")
           (make-instance 'grid-column
                          :name 'user
                          :header "Employee Number")
           (make-instance 'grid-column
                          :name 'address-type
                          :header "Address Type")
           (make-instance 'grid-column
                          :name 'country-town
                          :header "Country"
                          :printer 'print-country)
           (make-instance 'grid-column
                          :name 'country-town
                          :header "Province"
                          :printer 'print-province)
           (make-instance 'grid-column
                          :name 'country-town
                          :header "Town"
                          :printer 'print-town)))

         (grid (make-widget 'address-grid :name "bio-addresses-gridx"
                            :columns columns
                            :title "Addresses"
                            :row-object-class 'address)))

    (setf (sort-key-function grid)
          (lambda (doc)
            (format nil "~A ~A"  
                    (get-val doc 'key)
                    (get-val doc 'user))))
    (render (make-widget 'page :name "bio-addresses-page")
            :body (render-to-string grid))))

