(in-package :dyb)

(defclass root-entities-grid (grid)
  ()
  (:default-initargs :edit-inline nil))

(defmethod get-rows ((grid root-entities-grid))
  (setf (rows grid)
	(loop for root-entities across 
             (find-docs 'vector
                        (lambda (doc)
                          (not (string-equal (get-val doc 'doc-status) "Superseded")))
                        (entities-collection))
              collect root-entities)))

(defmethod render-row-editor ((grid root-entities-grid) row)
  (let ((form (make-widget 'html-framework-form :name "root-entities-form"
                           :grid-size 12
                           :header "Root-Entities"
                           :form-id "root-entities-form"
                           :grid-name (name grid)))
        (form-section (make-widget 'form-section
                                   :name "form-section"))
        (entity-type-select (make-widget 'select :name "entity-type-select")))

    (setf (items entity-type-select) (entity-type-list))
    (render form
            :content
            (with-html-to-string ()
              
              (render form-section 
                      :label "Entity"
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
                                                      :required t))))
              (render form-section 
                      :label "Entity Type"
                      :input (if (get-val row 'entity-name)
                                 (with-html-to-string ()
                                   (render-edit-field "entity-type" 
                                                      (get-val row 'entity-type)
                                                      :type :span)
                                   (:input :type "hidden" :name "entity-type" 
                                           :value (get-val row 'entity-type)))
                                 (with-html-to-string ()
                                   (render entity-type-select))))))))


(defmethod handle-action ((grid root-entities-grid) (action (eql 'cancel)))
  (finish-editing grid))

(defmethod handle-action ((grid root-entities-grid) (action (eql 'save)))
  (when (string-equal (parameter "form-id") "root-entities-form")

   (let ((new-doc (copy (editing-row grid))))
     (synq-edit-data new-doc)
     (setf (key new-doc) (parameter "entity-name"))
     (setf (get-val new-doc 'entity-type) (get-entity-type (parameter "entity-type")) )
     (setf (get-val new-doc 'doc-type) "Entity")
     (persist new-doc)

     (let* ((entity (get-entity (parameter "entity-name")))
           (relation (get-root (xid entity))))

       (unless relation
         (setf relation (make-entity-relationship nil nil entity nil)))
       (persist relation)))))