(in-package :ems)

(defclass clients-grid (grid)
  ()
  (:default-initargs :edit-inline nil))

(defmethod get-rows ((grid clients-grid))
  (setf (rows grid)
	(loop for clients across 
             (find-docs 'vector
                        (lambda (doc)
                          (and
                           (not (string-equal (get-val doc 'doc-status) "Superseded"))
                           (string-equal 
                            (get-val (get-val doc 'entity-type) 'entity-type-name)
                            "Client")))
                        (entities-collection))
              collect clients)))

(defmethod render-row-editor ((grid clients-grid) row)
  (let ((form (make-widget 'peach-form :name "clients-form"
                           :grid-size 12
                           :header "Clients"
                           :form-id "clients-form"
                           :grid-name (name grid)))
        (form-section (make-widget 'form-section
                                   :name "form-section")))
    (render form
            :grid grid
            :content
            (with-html-to-string ()
              (:input :type "hidden" :name "entity-type" 
                                           :value "Client")
              (render form-section 
                      :label "Client"
                      :input (if (get-val row 'entity-name)
                                 (with-html-to-string ()
                                   (render-edit-field "entity-name" 
                                                      (get-val row 'entity-name)
                                                      :type :span)
                                   (:input :type "hidden" :name "entity-name" 
                                           :value (get-val row 'entity-name)))
                                 (with-html-to-string ()
                                   (render-edit-field "entity-name" 
                                                      (get-val row 'entity-name)
                                                      :required t))))))))

(defmethod handle-action ((grid clients-grid) (action (eql 'cancel)))
  (finish-editing grid))

(defmethod handle-action ((grid clients-grid) (action (eql 'save)))
  (when (string-equal (parameter "form-id") "clients-form")

   (let ((new-doc (editing-row grid))
         (old-doc (copy (editing-row grid))))
     (synq-edit-data new-doc)
     (setf (key new-doc) (parameter "entity-name"))
     (setf (get-val new-doc 'entity-type) (get-entity-type (parameter "entity-type")) )
     (setf (get-val new-doc 'doc-type) "Entity")
    
     (if (xid old-doc) 
         (persist new-doc :old-object old-doc)
         (persist new-doc))

     (let* ((entity (get-entity (parameter "entity-name")))
           (relation (get-root (xid entity))))

       (unless relation
         (setf relation (make-entity-relationship nil nil entity nil)))
       (persist relation))
     (finish-editing grid))))