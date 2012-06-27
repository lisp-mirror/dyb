(in-package :ems)

(defclass country-town-grid (grid)
  ()
  (:default-initargs :edit-inline nil))

(defmethod list-grid-filters ((grid country-town-grid))
  '( with-audit-data))

(defun get-country-town-data (grid &key filter search)
  (declare (ignore grid search))
  (xdb2::sort-docs
   (find-docs 'vector
              (lambda (doc)
                (cond ((equal filter 'with-audit-data)
                       doc)
                      
                      ((equal filter 'multi-nationals)
                       (if (and (not (string-equal (get-val doc 'doc-status) "superseded"))
                                (string-equal (get-val doc 'multinational) "yes"))
                           doc))
                      (t 
                       (if (not (string-equal (get-val doc 'doc-status) "superseded"))
                           doc))))
              (country-towns-collection))
   :sort-value-func (lambda (doc)
                      (get-val doc 'country))
   :sort-test-func #'string<))

(defmethod get-rows ((grid country-town-grid))
  (setf (rows grid)
	(loop for country-town across (country-towns)
              collect country-town)))

(defmethod render-row-editor ((grid country-town-grid) row)
  (let ((form (make-widget 'peach-form :name "country-town-form"
                           :grid-size 12
                           :header "Country Town"
                           :form-id "country-town-form"
                           :grid-name (name grid)))
        (form-section (make-widget 'form-section
                                   :name "form-section")))

(setf (get-val form 'header)
          (format nil "Country Town (~A)" 
                  (get-val row 'country)))
    
    (render form
            :content
            (with-html-to-string ()
              (render form-section 
                      :label "Country"
                      :input (with-html-to-string ()
                               (render-edit-field "country" 
                                                  (get-val row 'country))))
                                     
              (render form-section 
                      :label "Province"
                      :input (with-html-to-string ()
                               (render-edit-field "province" 
                                                  (get-val row 'province)
                                                  )))
             

              (render form-section 
                      :label "Town"
                      :input (with-html-to-string ()
                               (render-edit-field "town" 
                                                  (get-val row 'town))))

              (render form-section 
                      :label "Longitude"
                      :input (with-html-to-string ()
                               (render-edit-field "longitude" 
                                                  (get-val row 'longitude))))

              (render form-section 
                      :label "Latitude"
                      :input (with-html-to-string ()
                               (render-edit-field "latitude" 
                                                  (get-val row 'latitude)))) 

              (render form-section 
                      :label "Guess"
                      :input (with-html-to-string ()
                               (render-edit-field "guess-p" 
                                                  (get-val row 'guess-p))))))))

(defmethod handle-action ((grid country-town-grid) (action (eql 'save)))
  (when (string-equal (parameter "form-id") "country-town-form")  
    (persist-doc-from-grid-edit (editing-row grid) 
                                (list (parameter "country") 
                                      (parameter "province")
                                      (parameter "town")))))