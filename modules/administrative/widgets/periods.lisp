(in-package :dyb)

(defclass period-grid (grid)
  ()
  (:default-initargs :edit-inline nil))

(defmethod list-grid-filters ((grid period-grid))
  '(status with-audit-data))

(defun get-periods-data (grid &key filter search)
  (declare (ignore grid search))

  (if (equal filter 'with-audit-data)
       (let ((docs))
         (dolist (doc (coerce (periods) 'list))
           (when (match-context-entities doc)
             (setf docs (append docs (list doc)))
             (when (old-versions doc)
               (setf docs (append docs (old-versions doc))))))
         (coerce docs 'vector))

       (find-docs 'vector
                  (lambda (doc)
                    (if (match-context-entities doc)
                        (cond ((equal filter 'status)
                               (if (and (not (string-equal (get-val doc 'doc-status) "superseded"))
                                        (string-equal (get-val doc 'status) "Closed"))
                                   doc))
                              (t 
                               (if (not (string-equal (get-val doc 'doc-status) "superseded"))
                                   doc)))))
                  (periods-collection))))

(defmethod get-rows ((grid period-grid))
  (setf (rows grid) (get-periods-data grid 
                                       :filter (grid-filter grid)  
                                       :search (search-term grid))))

(defun entity-mine-list ()
  (let ((e-list))
    (dolist (doc (coerce (entities) 'list))
      (if (and (not (string-equal (get-val doc 'doc-status) "superseded"))
            (string-equal (get-val (get-val doc 'entity-type) 'entity-type-name) "Mine"))
          (setf e-list (append e-list (list (list (get-val doc 'xid) 
                                                  (get-val doc 'entity-name)))))))
    e-list))


(defmethod render-row-editor ((grid period-grid) row)
  (let ((form (make-widget 'html-framework-form :name "period-formxx"
                           :grid-size 12
                           :header "Reporting Periods"
                           :form-id "reporting-period-form"
                           :grid-name (name grid)))
        (form-section (make-widget 'form-section
                                   :name "form-section")))
    (render form
             :grid grid
             :content
             (with-html-to-string ()   
               
               
               (render form-section 
                       :label "Entity"
                       :input 
                       (if (get-val row 'entity)
                           (with-html-to-string ()
                             (render-edit-field
                              "mine-entity-name"  
                              (get-val (get-val row 'entity) 'entity-name)
                              :type :span)
                             (:input :type "hidden" :name "mine-entity-xid" 
                                     :value (get-val (get-val row 'entity) 'xid)))
                           (with-html-to-string ()
                             (render-edit-field
                              "mine-entity" 
                              (get-val (get-val row 'entity) 'xid)
                              :data (entity-mine-list)
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
                                 (get-val row 'description))))

               (render form-section 
                       :label "Start Date"
                       :input (with-html-to-string ()
                                (render-edit-field 
                                 "start-date"
                                 (format-universal-date 
                                  (get-val row 'start-date))
                                 :required t
                                 :type :date)))

               (render form-section 
                       :label "End Date"
                       :input (with-html-to-string ()
                                (render-edit-field 
                                 "end-date"
                                 (format-universal-date 
                                  (get-val row 'end-date))
                                 :required t
                                 :type :date)))

               (render form-section 
                       :label "Status"
                       :input (with-html-to-string ()
                                (render-edit-field 
                                 "status" 
                                 (get-val row 'status)
                                 :data (find-allsorts-for-select
                                        "Reporting Period Status")
                                 :required t
                                 :type :select)))))))

(defun get-period-exist (entity period-name)
  (find-doc (periods-collection)
            :test
            (lambda (doc)
              (and       
               (not (string-equal (get-val doc 'doc-status) "superseded"))
               (equal (get-val doc 'entity) entity)
               (string-equal (get-val doc 'period-name) period-name)))))


(defmethod handle-action ((grid period-grid) (action (eql 'save)))  
  (when (string-equal (parameter "form-id") "reporting-period-form")
    (if (and
         (not (get-val (editing-row grid) 'xid))
         (or (string-equal (parameter "mine-entity") "")
                 (equal (parameter "mine-entity") nil)))
             (setf (error-message grid)
                   "Entity is required")
             (let ((new-doc (editing-row grid)))
                                             
               (unless (parameter "mine-entity-xid") 
                 (setf (get-val new-doc 'entity) 
                       (get-entity-by-id 
                        (if (stringp (parameter "mine-entity"))
                            (parse-integer 
                             (parameter "mine-entity"))
                            (parameter "mine-entity")))))

               (setf (get-val new-doc 'doc-type) "period")
               (setf (get-val new-doc 'period-name) (parameter "period-name"))
               (setf (key new-doc)
                     (list (xid (get-val new-doc 'entity)) 
                           (parameter "period-name")))

               (setf (error-message grid) nil)
               (cond ((and
                       (not (parameter "mine-entity-xid"))
                       (or (string-equal (parameter "mine-entity") "")
                           (equal (parameter "mine-entity") nil)))
                      (setf (error-message grid)
                            "Entity is required"))

                     ((and (not (get-val new-doc 'xid))
                           (get-period-exist 
                            (get-val new-doc 'entity)
                            (get-val new-doc 'period-name)))
                      (setf (error-message grid)
                            "Record already exists"))
                     
                     ((> (string-to-date (parameter "start-date")) 
                         (string-to-date (parameter "end-date")))
                      (setf (error-message grid)
                            "Start date must be earlier than end date"))

                     ((or (string-equal (parameter "status") "")
                          (equal (parameter "status") nil))
                      (setf (error-message grid)
                            "Status is required"))
                     
                     (t
                      (synq-edit-data new-doc)
                      (persist new-doc)
                      (finish-editing grid)))))))

(defmethod export-csv ((grid period-grid))
  (let* ((data (grid-filtered-rows grid)))
    (when data
      (with-output-to-string (stream)
       (format stream "Reporting Period Export Filter :~A    Search :~A    Date:~A ~%Context:~A~%~%"  
                (or (get-val grid 'grid-filter ) "") (or  (get-val grid 'search-term ) "")  (current-date-time) (print-context) ) 
        (format stream "~A~%" "entity|period-name|period-type|description|start-date|end-date|status")
        (loop 
           for doc across data 
           do (format stream "~A|~A|~A|~A|~A|~A|~A|~%"
                          (get-val (get-val doc 'entity) 'entity-name)
                          (get-val doc 'period-name)
                          (get-val doc 'period-type)
                          (get-val doc 'description)
                          (format-universal-date (get-val doc 'start-date))
                          (format-universal-date (get-val doc 'end-date))
                          (get-val doc 'status)))))))

(defmethod handle-action ((grid period-grid) (action (eql 'cancel)))
  (finish-editing grid))

