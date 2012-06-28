(in-package #:ems)

;;(populate-post-db-from-json (rest (first (json:decode-json-from-string *jsstr*))))

(defclass post-grid (grid)
  ()
  (:default-initargs :edit-inline nil))

(defun get-post-data (grid &key filter search)
  (declare (ignore grid search))
  (xdb2::sort-docs 
   
   (find-docs 'vector
              (lambda (doc)
                ;;(if (match-context-entities doc)
                ;;    )

                (cond ((equal filter 'with-audit-data)
                           doc)
                          (t 
                           (if (not (string-equal (get-val doc 'doc-status) "superseded"))
                               doc))))
              (posts-collection))
   :sort-value-func (lambda (doc)
                      (get-val doc 'post-id))
   :sort-test-func #'string<))

(defmethod get-rows ((grid post-grid))
  (setf (rows grid)
	(get-post-data grid 
                          :filter (grid-filter grid)  
                          :search (search-term grid))))


(defmethod render-row-editor ((grid post-grid) row)
  (let ((form (make-widget 'peach-form :name "p-formx"
                                       :grid-size 12
                                       :header "Posts"
                                       :form-id "post-edit-form"
                                       :grid-name (name grid)))
        (form-section (make-widget 'form-section
                                   :name "form-section"))
        (tab-box (make-widget 'peach-tab-box
                              :name "post-tab-box"
                              :header "post"
                              :icon "card--pencil")))

    (setf (get-val form 'header)
          (format nil "Post (~A)" 
                  (get-val (editing-row grid) 'post-id)))
    (setf (get-val tab-box 'header)
          (format nil "Post (~A)" 
                  (get-val (editing-row grid) 'post-id)))
    
    (setf 
     (tabs tab-box)
     (list
      (list
       "post"
       (with-html-to-string ()
         (:div :class "section _100"
               (render form
                       :content
                       (with-html-to-string ()
                         (render 
                          form-section
                          :label "Post ID"
                          :input (with-html-to-string ()
                                   (render-edit-field 
                                    "post-id"
                                    (get-val row 'post-id))))
                         (render 
                          form-section
                          :label "Message"
                          :input (with-html-to-string ()
                                   (render-edit-field 
                                    "message"
                                    (get-val row 'message)
                                    :type :textarea)))
                         (render 
                          form-section
                          :label "Story"
                          :input (with-html-to-string ()
                                   (render-edit-field 
                                    "Story"
                                    (get-val row 'story)
                                    :type :textarea))))))))
      (list
       "Addresses"
       (with-html-to-string ()
         (:div :class "section _100"
               #|
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
                      (address-grid (make-widget 'post-address-grid
                                                 :name "post-address-gridx"
                                                 :columns columns
                                                 :edit-inline nil
                                                 :title "Addresses"
                                                 :row-object-class 'address)))

                 (setf (get-val address-grid 'title)
                       (format nil "Addresses (~A)" 
                               (get-val (get-val (editing-row grid) 'entity) 'entity-name)))
                 (setf (get-val address-grid 'current-post) (editing-row grid))
                 (render address-grid))
               |#
               )))
      (list
       "Contacts"
       (with-html-to-string ()
         (:div :class "section _100"
               #|
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
                      (contact-grid (make-widget 'post-contact-grid
                                                 :name "post-contact-grid"
                                                 :columns columns
                                                 :edit-inline nil
                                                 :title "Contacts"
                                                 :row-object-class 'contact)))

                 (setf (get-val contact-grid 'title)
                       (format nil "Contacts (~A)" 
                               (get-val (get-val (editing-row grid) 'entity) 'entity-name)))
                 (setf (get-val contact-grid 'current-post) (editing-row grid))
                 (render contact-grid))|#
               )))))
    (render tab-box)))
