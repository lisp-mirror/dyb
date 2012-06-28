(in-package :ems)

(defclass period-grid (grid)
  ()
  (:default-initargs :edit-inline nil))

#|
(loop for period across (periods)
              collect period)
|#


(defmethod list-grid-filters ((grid period-grid))
  '(status with-audit-data))

(defun get-periods-data (grid &key filter search)
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
                ;(break "~A" (match-context-entities doc))
                (cond ((equal filter 'with-audit-data)
                       doc)
                      ((equal filter 'status)
                       (if (and (not (string-equal (get-val doc 'doc-status) "superseded"))
                                (string-equal (get-val doc 'status) "Closed"))
                           doc))
                      (t 
                       (if (not (string-equal (get-val doc 'doc-status) "superseded"))
                           doc))))
              (periods-collection))
   :sort-value-func (lambda (doc)
                      (get-val doc 'status))
   :sort-test-func #'string<))

(defmethod get-rows ((grid period-grid))
  (setf (rows grid) (get-periods-data grid 
                                       :filter (grid-filter grid)  
                                       :search (search-term grid))))


(defmethod render-row-editor ((grid period-grid) row)
  (let ((form (make-widget 'peach-form :name "period-formxxxx"
                           :grid-size 12
                           :header "Reporting Periods"
                           :form-id "period-form"
                           :grid-name (name grid)))
        (form-section (make-widget 'form-section
                                   :name "form-section")))
    
    (render form
            :content
            (with-html-to-string ()   
              
              
              (render form-section 
                      :label "Entity"
                      :input 
                      (if (get-val (get-val row 'entity-relationship) 'entity)
                          (with-html-to-string ()
                            (render-edit-field
                             "entityx" 
                             (get-val (get-val (get-val row 'entity-relationship) 'entity) 'entity-name)
                             :type :span)
                            (:input :type "hidden" :name "entity" 
                                    :value (get-val 
                                            (get-val 
                                             (get-val row 'entity-relationship) 
                                             'entity) 'entity-name)))
                          (with-html-to-string ()
                            (render-edit-field
                             "entity" 
                             (get-val (get-val row 'entity-relationship) 'xid)
                             :data (entity-list)
                             :required t
                             :type :select))))

              (render form-section 
                      :label "Period Name"
                      :input (with-html-to-string ()
                               (render-edit-field 
                                "period-name" 
                                (get-val row 'period-name)
                                :required t)))

              (render form-section 
                      :label "Period Type"
                      :input (with-html-to-string ()
                               (render-edit-field 
                                "period-type" 
                                (get-val row 'period-type)
                                :required t)))

              (render form-section 
                      :label "Description"
                      :input (with-html-to-string ()
                               (render-edit-field
                                "description" 
                                (get-val row 'description)
                                :required t)))

              (render form-section 
                      :label "Start Date"
                      :input (with-html-to-string ()
                               (render-edit-field 
                                "start-date"
                                (format-universal-date 
                                 (get-val row 'start-date))
                                :required t
                                :type "date")))

              (render form-section 
                      :label "End Date"
                      :input (with-html-to-string ()
                               (render-edit-field 
                                "end-date"
                                (format-universal-date 
                                 (get-val row 'end-date))
                                :required t
                                :type "date")))

              (render form-section 
                      :label "Status"
                      :input (with-html-to-string ()
                               (render-edit-field 
                                "Status" 
                                (get-val row 'status)
                                :data (find-allsorts-for-select
                                       "Reporting Period Status")
                                :required t
                                :type :select)))))))

(defmethod handle-action ((grid period-grid) (action (eql 'save)))
  (let ((eid (if (numeric-string-p (parameter "entity"))
                 (parse-integer (parameter "entity"))
                 (xid (get-entity (parameter "entity")))))) 
    
    (when (get-root eid)
      (let* ((root (get-root eid))
             (entity-rel (get-relationship-tree-item 
                          root
                          (xid (get-entity-by-id eid))))
             (erid entity-rel))  
        (persist-doc-from-grid-edit
         (editing-row grid) 
         (list (xid erid)  
               (parameter "period-name")))))))