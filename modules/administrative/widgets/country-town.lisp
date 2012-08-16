(in-package :ems)

(defclass municipalities-grid (grid)
  ((parent-grid :initarg :parent-grid)
   (country-town-doc :initarg nil))
  (:default-initargs :edit-inline nil))


(defmethod get-rows ((grid municipalities-grid))  
  (when (get-val (get-val grid 'country-town-doc) 'municipalities)
    (setf (rows grid)
          (loop for municipality across (coerce (get-val (get-val grid 'country-town-doc) 
                                                         'municipalities) 'vector)
             collect municipality))))

(defmethod render-row-editor ((grid municipalities-grid) row)
  (let ((form (make-widget 'peach-form :name "municipalities-formxx"
                           :grid-size 12
                           :header "Municipalities"
                           :form-id "municipalities-edit-form"
                           :grid-name (name grid)))
        (form-section (make-widget 'form-section
                                   :name "form-section")))

    (render form
            :grid grid
            :content
            (with-html-to-string ()                            
              (render form-section 
                      :label "District Municipality"
                      :input 
                      (with-html-to-string ()
                        (render-edit-field "district-municipality" 
                                           (get-val row 'district-municipality)
                                           :required t)))
              (render form-section 
                      :label "Local Municipality"
                      :input 
                      (with-html-to-string ()
                        (render-edit-field "local-municipality" 
                                           (get-val row 'local-municipality)
                                           :required t)))))))


(defmethod handle-action ((grid municipalities-grid) (action (eql 'save))) 
  (when (string-equal (parameter "form-id") "municipalities-edit-form")
    (let ((old-country-town (copy (get-val grid 'country-town-doc)))
          (new-country-town (get-val grid 'country-town-doc))
          (old-municipality (editing-row grid))
          (new-municipality (copy (editing-row grid))))
      
      (synq-edit-data new-municipality)
      (setf (error-message grid) nil)
      (if (get-val new-country-town 'municipalities)
          (if (find old-municipality (get-val old-country-town 'municipalities))
              (setf (get-val new-country-town 'municipalities) 
                    (substitute new-municipality old-municipality 
                                (get-val new-country-town 'municipalities)))
              (setf (get-val new-country-town 'municipalities) 
                    (append (get-val new-country-town 'municipalities) (list new-municipality))))
          (setf (get-val new-country-town 'municipalities) 
                (list new-municipality)))
      
      (persist new-country-town :old-object (if (get-val new-country-town 'xdb2::id)
                                        old-country-town)))
    (finish-editing grid)))

(defclass country-town-grid (grid)
  ()
  (:default-initargs :edit-inline nil))

(defmethod list-grid-filters ((grid country-town-grid))
  '(with-audit-data))

(defun get-country-town-data (grid &key filter search)
  (declare (ignore grid search))
  (if (equal filter 'with-audit-data)
      (let ((docs))
        (dolist (doc (coerce (periods) 'list))
          (setf docs (append docs (list doc)))
          (when (old-versions doc)
            (setf docs (append docs (old-versions doc)))))
        (coerce docs 'vector))
      (find-docs 'vector
                 (lambda (doc)
                   (if (not (string-equal (get-val doc 'doc-status) "superseded"))
                       doc))
                 (country-towns-collection))))

(defmethod get-rows ((grid country-town-grid))
  (setf (rows grid)
        (get-country-town-data grid 
                               :filter (grid-filter grid)  
                               :search (search-term grid))))

(defmethod render-row-editor ((grid country-town-grid) row)
  (let ((form (make-widget 'peach-form :name "country-town-formxxx"
                           :grid-size 12
                           :header "Country Town"
                           :form-id "country-town-form"
                           :grid-name (name grid)))
        (form-section (make-widget 'form-section
                                   :name "form-section"))
        (tab-box (make-widget 'peach-tab-box
                              :name "country-town-tab-box"
                              :header "Country/Town"
                              :icon "card--pencil")))

    (setf (get-val form 'header)
          (format nil "Country Town (~A)" 
                  (get-val row 'country)))
    
    
    (setf (tabs tab-box)
          (list
           (list 
            "Country/Town"
            (with-html-to-string ()
              (:div :class "section _100" 
                    (render form
                            :grid grid
                            :content
                            (with-html-to-string ()
                              (render form-section 
                                      :label "Country"
                                      :input (with-html-to-string ()
                                               (render-edit-field 
                                                "country" 
                                                (get-val row 'country))))
                              
                              (render form-section 
                                      :label "Province"
                                      :input (with-html-to-string ()
                                               (render-edit-field 
                                                "province" 
                                                (get-val row 'province))))
                              

                              (render form-section 
                                      :label "Town"
                                      :input (with-html-to-string ()
                                               (render-edit-field 
                                                "town" 
                                                (get-val row 'town))))

                              (render form-section 
                                      :label "Longitude"
                                      :input (with-html-to-string ()
                                               (render-edit-field 
                                                "longitude" 
                                                (gps-cord-formatter (get-val row 'longitude))
                                                :type "number"
                                                :required t)))

                              (render form-section 
                                      :label "Latitude"
                                      :input (with-html-to-string ()
                                               (render-edit-field 
                                                "latitude" 
                                                (gps-cord-formatter (get-val row 'latitude))
                                                :type "number"
                                                :required t))) 

                              (render form-section 
                                      :label "Guess"
                                      :input (with-html-to-string ()
                                               (render-edit-field 
                                                "Guess" 
                                                (get-val row 'guess-p)
                                                :data (find-allsorts-for-select
                                                       "Guess")
                                                :required t
                                                :type :select))))))))
           (list 
            "Municipalities"
            (with-html-to-string ()
              (:div :class "section _100" 
                    (let* ((columns
                            (list
                             (make-instance 'grid-column
                                            :name 'district-municipality
                                            :header "District Municiplality")
                             (make-instance 'grid-column
                                            :name 'local-municipality
                                            :header "Local Municiplality")))
                           (municipalities-grid (make-widget 'municipalities-grid 
                                                             :name "mine-municipalities-gridxxx"
                                                             :columns columns
                                                             :edit-inline nil
                                                             :title "Municipalities"
                                                             :row-object-class 'municipality )))

                      (setf (get-val municipalities-grid 'parent-grid) grid)

                      (setf (get-val municipalities-grid 'country-town-doc) 
                            (editing-row grid))
                      
                      (render municipalities-grid)))))))
    (render tab-box)))

(defmethod handle-action ((grid country-town-grid) (action (eql 'save)))
  (when (string-equal (parameter "form-id") "country-town-form")  
    (let ((old-doc (copy (editing-row grid)))
          (new-doc (editing-row grid)))

      (setf (error-message grid) nil)
      
      (cond ((or (string-equal (parameter "country") "")
                 (equal (parameter "country") nil))
             (setf (error-message grid)
                   "Country is required"))
            
            ((or (string-equal (parameter "province") "")
                 (equal (parameter "province") nil))
             (setf (error-message grid)
                   "Province is required"))

            ((or (string-equal (parameter "town") "")
                 (equal (parameter "town") nil))
             (setf (error-message grid)
                   "Town is required"))            

            ((and (not (get-val new-doc 'xid))
                  (get-country-town 
                   (get-val new-doc 'country)
                   (get-val new-doc 'province)
                   (get-val new-doc 'town)))
             (setf (error-message grid)
                   "Record already exists"))    
            
            (t
             (setf (key new-doc)
                   (list (parameter "country") 
                         (parameter "province")
                         (parameter "town")))

             (synq-edit-data new-doc)
             
             (if (not (get-val new-doc 'xid))
                 (persist new-doc)
                 (persist new-doc :old-object old-doc))
             (finish-editing grid))))))