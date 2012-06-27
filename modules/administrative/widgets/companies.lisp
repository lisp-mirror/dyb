(in-package :ems)

(defclass company-contact-grid (grid)
  ((current-company :initarg nil))
  (:default-initargs :edit-inline nil))

(defmethod get-rows ((grid company-contact-grid))
  (when (and (get-val grid 'current-company) (get-val (get-val grid 'current-company)
                                                     'contacts))
    (setf (rows grid)
          (loop for contact across (coerce (get-val (get-val grid 'current-company)
                                                    'contacts) 'vector)
             collect contact))))

(defmethod render-row-editor ((grid company-contact-grid) row)
  (let ((form (make-widget 'peach-form :name "company-contact-formx"
                           :grid-size 12
                           :header "Contact"
                           :form-id "company-contact-edit-form"
                           :grid-name (name grid)))
        (form-section (make-widget 'form-section
                                   :name "form-section")))

    (setf (get-val form 'header)
          (format nil "Contact (~A)" 
                  (get-val row 'contact-type)))
    

    (render form
            :content
            (with-html-to-string ()
              
              (render form-section
                      :label "Contact Type"
                      :input
                      (with-html-to-string ()
                        (render-edit-field "contact-type"
                                           (get-val row 'contact-type))))

              (render form-section
                      :label "Name"
                      :input
                      (with-html-to-string ()
                        (render-edit-field "contact-name"
                                           (get-val row 'contact-name))))
              (render form-section 
                      :label "Telephone Number"
                      :input 
                      (with-html-to-string ()
                        (render-edit-field "telephone-number"
                                           (get-val row 'telephone-number))))
              (render form-section 
                      :label "Facsimile Number"
                      :input 
                      (with-html-to-string ()
                        (render-edit-field
                         "facsimile-number"
                         (get-val row 'facsimile-number))))
              (render form-section
                      :label "Email"
                      :input
                      (with-html-to-string ()
                        (render-edit-field
                         "email-address"
                         (get-val row 'email-address))))))))

(defmethod handle-action ((grid company-contact-grid) (action (eql 'save)))
  (when (string-equal (parameter "form-id") "company-contact-edit-form")
    (persist-doc (update-master-child-doc (get-val grid 'current-company)
                                          (editing-row grid)
                                          'contacts
                                          :element 'contact-type))
    ))



(defclass company-address-grid (grid)
  ((current-company :initarg nil))
  (:default-initargs :edit-inline nil))

(defmethod get-rows ((grid company-address-grid))
  (when (and (get-val grid 'current-company)
             (get-val (get-val grid 'current-company) 'addresses))
    (setf (rows grid)
          (loop for address across (coerce (get-val (get-val grid 'current-company)
                                                    'addresses) 'vector)
             collect address))))

(defmethod render-row-editor ((grid company-address-grid) row)
  (let* ((form (make-widget 'peach-form :name "company-address-form"
                            :grid-size 12
                            :header "Address"
                            :form-id "company-address-edit-form"
                            :grid-name (name grid)))
         (form-section (make-widget 'form-section
                                    :name "form-section"))
         (country-town (make-widget 'country-town-select :name "country-townx"))
         )

    (setf (get-val form 'header)
          (format nil "Address (~A)" 
                  (get-val row 'address-type)))

    (render form
            :content
            (with-html-to-string ()
              

              (render form-section
                      :label "Address Type"
                      :input
                      (with-html-to-string ()
                        (render-edit-field
                         "address-type"
                         (get-val row 'address-type)
                         :data (find-allsorts-for-select "Address Type")
                         :required t
                         :type :select)))
              (destructuring-bind (country province town)
                  (selects country-town)
                (render country-town)
                (render form-section
                        :label "Country"
                        :input
                        (with-html-to-string ()
                          (setf (value country) (get-val row 'country))
                          (render country)))
                (render form-section
                        :label "Province"
                        :input
                        (with-html-to-string ()
                          (setf (value province) (get-val row 'province))
                          (render province)))
                (render form-section
                        :label "Town"
                        :input
                        (with-html-to-string ()
                          (setf (value town) (get-val row 'town))
                          (render town))))))))

(defmethod handle-action ((grid company-address-grid) (action (eql 'save)))

  (when (string-equal (parameter "form-id") "company-address-edit-form")
    (persist-doc (update-master-child-doc (get-val grid 'current-company)
                                          (editing-row grid)
                                          'addresses
                                          :element 'address-type))
    ))


(defclass company-grid (grid)
  ()
  (:default-initargs :edit-inline nil))


(defun get-company-data (grid &key filter search)
  (declare (ignore grid search))
  (xdb2::sort-docs
   (find-docs 'vector
              (lambda (doc)
                (if (match-context-entities doc)
                    (cond ((equal filter 'with-audit-data)
                           doc)
                          (t 
                           (if (not (string-equal (get-val doc 'doc-status) "superseded"))
                               doc)))))
              (companies-collection))
   :sort-value-func (lambda (doc)
                      (get-val (get-val doc 'entity) 'entity-name))
   :sort-test-func #'string<))

(defmethod get-rows ((grid company-grid))
  (setf (rows grid)
	(get-company-data grid 
                          :filter (grid-filter grid)  
                          :search (search-term grid))))

(defmethod render-row-editor ((grid company-grid) row)
  (let ((form (make-widget 'peach-form :name "companies-formx"
                                       :grid-size 12
                                       :header "Companies"
                                       :form-id "company-edit-form"
                                       :grid-name (name grid)))
        (form-section (make-widget 'form-section
                                   :name "form-section"))
        (tab-box (make-widget 'peach-tab-box
                              :name "company-tab-box"
                              :header "Company"
                              :icon "card--pencil")))

    (setf (get-val form 'header)
          (format nil "Company (~A)" 
                  (get-val (get-val (editing-row grid) 'entity) 'entity-name)))
    (setf (get-val tab-box 'header)
          (format nil "Company (~A)" 
                  (get-val (get-val (editing-row grid) 'entity) 'entity-name)))
    
    (setf 
     (tabs tab-box)
     (list
      (list
       "Company"
       (with-html-to-string ()
         (:div :class "section _100"

               (render form
                       :content
                       (with-html-to-string ()
                         (render 
                          form-section
                          :label "Entity"
                          :input
                          (with-html-to-string ()
                            (if (get-val row 'entity)
                                (htm
                                 (:input :type "hidden" :name "entity" 
                                         :value (get-val (get-val row 'entity) 'xid))
                                 (render-edit-field 
                                  "entity-name" 
                                  (get-val (get-val row 'entity) 'entity-name)
                                                    :data (entity-list)
                                                    :type :span))
                                (render-edit-field "entity" 
                                                   (get-val (get-val row 'entity) 'xid)
                                                   :data (entity-list)
                                                   :type :select))))
                         (render 
                          form-section
                          :label "Registration No"
                          :input (with-html-to-string ()
                                   (render-edit-field 
                                    "registration-no"
                                    (get-val row 'registration-no))))
                         (render 
                          form-section
                          :label "Vat No"
                          :input (with-html-to-string ()
                                   (render-edit-field 
                                    "vat-no"
                                    (get-val row 'vat-no)))))))))
      (list
       "Addresses"
       (with-html-to-string ()
         (:div :class "section _100"
               (let* ((columns
                       (list
                        (make-instance 'grid-column
                                       :name 'address-type
                                       :header "Address Type")
                        (make-instance 'grid-column
                                       :name 'country
                                       :header "Country")
                        (make-instance 'grid-column
                                       :name 'province
                                       :header "Province")
                        (make-instance 'grid-column
                                       :name 'town
                                       :header "Town")))
                      (address-grid (make-widget 'company-address-grid
                                                 :name "company-address-gridx"
                                                 :columns columns
                                                 :edit-inline nil
                                                 :title "Addresses"
                                                 :row-object-class 'address)))

                 (setf (get-val address-grid 'title)
                       (format nil "Addresses (~A)" 
                               (get-val (get-val (editing-row grid) 'entity) 'entity-name)))
                 (setf (get-val address-grid 'current-company) (editing-row grid))
                 (render address-grid)))))
      (list
       "Contacts"
       (with-html-to-string ()
         (:div :class "section _100"
               (let* ((columns
                       (list
                        (make-instance 'grid-column
                                       :name 'contact-type
                                       :header "Contact Type")
                               
                        (make-instance 'grid-column
                                       :name 'contact-name
                                       :header "Contact Name")
                        (make-instance 'grid-column
                                       :name 'email-address
                                       :header "Email")
                        ))
                      (contact-grid (make-widget 'company-contact-grid
                                                 :name "company-contact-grid"
                                                 :columns columns
                                                 :edit-inline nil
                                                 :title "Contacts"
                                                 :row-object-class 'contact)))

                 (setf (get-val contact-grid 'title)
                       (format nil "Contacts (~A)" 
                               (get-val (get-val (editing-row grid) 'entity) 'entity-name)))
                 (setf (get-val contact-grid 'current-company) (editing-row grid))
                 (render contact-grid)))))))
    (render tab-box)))

(defmethod handle-action ((grid company-grid) (action (eql 'save)))
  (when (string-equal (parameter "form-id") "company-edit-form")

    (let ((new-doc (copy (editing-row grid))))
      (synq-edit-data new-doc)
      (if (and (get-val new-doc 'entity) (stringp (get-val new-doc 'entity)))
          (setf (get-val new-doc 'entity) (get-entity-by-id 
                                           (parse-integer
                                             (get-val new-doc 'entity)))))
 
     (setf (key new-doc) (xid (get-val new-doc 'entity)))
    
      (persist-doc new-doc))

    ))