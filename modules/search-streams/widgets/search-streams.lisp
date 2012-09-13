(in-package #:ems)


(defclass search-streams-grid (grid)
  ((parent-grid :initarg :parent-grid)
   (current-doc :initarg nil))
  (:default-initargs :edit-inline nil))


(defun get-search-streams-data (grid &key filter search)
  (declare (ignore grid search))
  (find-docs 'vector
              (lambda (doc)
                (if (match-context-entities doc )
                    (cond ((equal filter 'with-audit-data)
                           doc)
                          (t 
                           (if (not (string-equal (get-val doc 'doc-status) "superseded"))
                               doc)))))
              (search-streams-collection)))

(defmethod get-rows ((grid search-streams-grid))
  (setf (rows grid)
	(get-search-streams-data grid 
                          :filter (grid-filter grid)  
                          :search (search-term grid))))

(defmethod render-row-editor ((grid search-streams-grid) row)
  (let ((form (make-widget 'peach-form :name "search-stream-form"
                           :grid-size 12
                           :header "Search Streams"
                           :form-id "search-stream-form"
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
                              "entity-name"  
                              (get-val (get-val row 'entity) 'entity-name)
                              :type :span)
                             (:input :type "hidden" :name "entity-xid" 
                                     :value (get-val (get-val row 'entity) 'xid)))
                           (with-html-to-string ()
                             (render-edit-field
                              "entity" 
                              (get-val (get-val row 'entity) 'xid)
                              :data (entity-list)
                              :required t
                              :blank-allowed t
                              :type :select))))
               (render form-section 
                       :label "Description"
                       :input (with-html-to-string ()
                                (render-edit-field 
                                 "description" 
                                 (get-val row 'description)
                                 :required t)))

               (render form-section 
                       :label "Type"
                       :input (with-html-to-string ()
                                (render-edit-field 
                                 "search-stream-type" 
                                 (get-val row 'search-stream-type)
                                 :data (list (list "Social Mention" "Social Mention")
                                             (list "Twitter" "Twitter"))
                                 :required t
                                 :blank-allowed t
                                 :type :select)))
               (render form-section 
                       :label "Search"
                       :input (with-html-to-string ()
                                (render-edit-field 
                                 "search-stream" 
                                 (get-val row 'search-stream)
                                 :required t)))

               (render form-section 
                       :label "Status"
                       :input (with-html-to-string ()
                                (render-edit-field 
                                 "search-stream-status" 
                                 (get-val row 'search-stream-status)
                                 :data (list (list "Open" "Open")
                                             (list "Closed" "Closed"))
                                 :required t
                                 :blank-allowed t
                                 :type :select)))
               
))))

(defmethod handle-action ((grid search-streams-grid) (action (eql 'save)))
  (when (string-equal (parameter "entity") "")
    (setf (error-message grid) "Select an Entity."))

  (when (string-equal (parameter "search-stream-type") "")
    (setf (error-message grid) "Select a Service."))

  (when (and (not (string-equal (parameter "entity") ""))
             (not (string-equal (parameter "search-stream-type") "")))
 
    (when (and (parameter "description") (not (string-equal (parameter "description") "")))
      (let ((new-doc (editing-row grid))
            (old-doc (copy (editing-row grid)))
            (entity
             (if (parameter "entity-xid")
                 (get-entity-by-id (parse-integer (parameter "entity-xid")))
                 (get-entity (parameter "entity")))
             ))
        (synq-edit-data new-doc)
        (setf (get-val new-doc 'entity) entity)
        (if (xid old-doc)
            (persist new-doc :old-object old-doc)
            (persist new-doc))

        (finish-editing grid)))))