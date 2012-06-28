(in-package :ems)

(defclass all-sorts-grid (grid)
  ()
  (:default-initargs :edit-inline nil))

(defmethod list-grid-filters ((grid all-sorts-grid))
  '(with-audit-data))

(defun get-all-sorts-data (grid &key filter search)
  (declare (ignore grid search))
  (xdb2::sort-docs
   (find-docs 'vector
              (lambda (doc)
                ;;TODO: Implement context
               #| (if (and (string-equal (get-val (get-val doc 'verification-set) 'doc-type)
                                       "entity")
                         (find (xid (get-val doc 'verification-set)) (list (xid (get-val doc 'verification-set))))) 
                    )
                |#
                (cond ((equal filter 'with-audit-data)
                       doc)
                      (t 
                       (if (not (string-equal (get-val doc 'doc-status) "superseded"))
                           doc))))
              (allsorts-collection))
   :sort-value-func (lambda (doc)
                      (get-val doc 'sort-value))
   :sort-test-func #'string<))

(defmethod get-rows ((grid all-sorts-grid))
  (setf (rows grid) (get-all-sorts-data grid 
                                        :filter (grid-filter grid)  
                                        :search (search-term grid))))

(defmethod render-row-editor ((grid all-sorts-grid) row)
  (let ((form (make-widget 'peach-form :name "allsorts-form"
                           :grid-size 12
                           :header "All-Sorts"
                           :form-id "allsorts-form"
                           :grid-name (name grid)))
        (form-section (make-widget 'form-section
                                   :name "form-section")))

    

    (render form
            :content
            (with-html-to-string ()
              
              
              (render form-section 
                      :label "Sort"
                      :input (if (get-val row 'sort)
                                 (with-html-to-string ()
                                   (render-edit-field "sort" 
                                                      (get-val row 'sort)
                                                      :type :span)
                                   (:input :type "hidden" :name "sort" 
                                           :value (get-val row 'sort)))
                                 (with-html-to-string ()
                                   (render-edit-field "sort" 
                                                      (get-val row 'sort)
                                                      :required t))))

              (render form-section 
                      :label "Sort Order"
                      :input (with-html-to-string ()
                               (render-edit-field "sort-order" 
                                                  (get-val row 'sort-order))))

              (render form-section 
                      :label "Sort Value"
                      :input (if (get-val row 'sort-value)
                                 (with-html-to-string ()
                                   (render-edit-field "sort-value" 
                                                      (get-val row 'sort-value)
                                                      :type :span)
                                   (:input :type "hidden" :name "sort-value" 
                                           :value (get-val row 'sort-value)))
                                 (with-html-to-string ()
                                   (render-edit-field "sort-value" 
                                                      (get-val row 'sort-value)
                                                      :required t))) )

              (render form-section 
                      :label "Alternate Sort Order"
                      :input (with-html-to-string ()
                               (render-edit-field "alternate-sort-order" 
                                                  (get-val row 'alternate-sort-order))))

              (render form-section 
                      :label "AA Sort Order"
                      :input (with-html-to-string ()
                               (render-edit-field "aa-sort-order" 
                                                  (get-val row 'aa-sort-order))))

              (render form-section 
                      :label "Description"
                      :input (if (get-val row 'description)
                                 (with-html-to-string ()
                                   (render-edit-field "description" 
                                                      (get-val row 'description)
                                                      :type :span))
                                 (with-html-to-string ()
                                   (render-edit-field "description" 
                                                      (get-val row 'description)
                                                      :required t))) )
              (render form-section 
                      :label "Extended Description"
                      :input (with-html-to-string ()
                               (render-edit-field "extended-description" 
                                                  (get-val row 'extended-description))))
              ))))


(defmethod handle-action ((grid all-sorts-grid) (action (eql 'cancel)))
  (finish-editing grid))

(defmethod handle-action ((grid all-sorts-grid) (action (eql 'save)))
  (when (string-equal (parameter "form-id") "allsorts-form")
   (let ((new-doc (copy (editing-row grid))))
     (synq-edit-data new-doc)
     (setf (key new-doc) (list (parameter "sort") 
                               (parameter "sort-value")))
     
     (persist-doc new-doc))))

(defmethod export-csv ((grid all-sorts-grid))
  (let ((data (grid-filtered-rows grid)))
    (format nil "~{~{~a~^| ~}~%~}" data)))