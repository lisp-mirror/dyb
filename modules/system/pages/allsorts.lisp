(in-package :ems)

(define-easy-handler (all-sorts :uri "/ems/all-sorts") ()
  (let* ((columns
           (list
            (make-instance 'grid-column
                           :name 'sort
                           :header "Sort" )
            (make-instance 'grid-column
                           :name 'sort-order
                           :header "Sort Order" )
            (make-instance 'grid-column
                           :name 'sort-value
                           :header "Sort Value")
            (make-instance 'grid-column
                           :name 'alternate-sort-order
                           :header "Alternate Sort Order")
            (make-instance 'grid-column
                           :name 'aa-sort-order
                           :header "AA Sort Order")
            (make-instance 'grid-column
                           :name 'description
                           :header "Description")
            (make-instance 'grid-column
                           :name 'extended-description
                           :header "Extended Description")))

         (grid (make-widget 'all-sorts-grid :name "all-sorts-grid"
                                       :columns columns
                                       :edit-inline nil
                                       :title "All-Sorts"
                                       :row-object-class 'allsort)))
    (setf (sort-key-function grid)
          (lambda (doc)
                      (format nil "~A ~A"  
                              (get-val doc 'sort)
                              (get-val doc 'sort-value))
                      ))
    (render (make-widget 'page :name "all-sorts-page")
            :body (render-to-string grid))))