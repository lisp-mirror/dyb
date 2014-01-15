(in-package :dyb)

(defclass all-sorts-grid (grid)
  ())

(defmethod list-grid-filters ((grid all-sorts-grid))
  '(with-audit-data))

(defun get-all-sorts-data (grid &key filter search)
  (declare (ignore grid search))

  (if (equal filter 'with-audit-data)
       (let ((docs))
         (dolist (doc (coerce (allsorts) 'list))
           (setf docs (append docs (list doc)))
           (when (old-versions doc)
               (setf docs (append docs (old-versions doc)))))
         (coerce docs 'vector))

       (find-docs 'vector
                  (lambda (doc)
                    (cond (t 
                           (if (not (string-equal (get-val doc 'doc-status) "superseded"))
                               doc))))
                  (allsorts-collection))))

(defmethod get-rows ((grid all-sorts-grid))
  (setf (rows grid) (get-all-sorts-data grid 
                                        :filter (grid-filter grid)  
                                        :search (search-term grid))))

(defmethod render-row-editor ((grid all-sorts-grid) row)
  (let ((form (make-widget 'html-framework-form :name "allsorts-form"
                           :grid-size 12
                           :header "All-Sorts"
                           :form-id "allsorts-form"
                           :grid-name (name grid)))
        (form-section (make-widget 'form-section
                                   :name "form-section")))

    

    (render form
            :grid grid
            :content
            (with-html-string
                (render form-section 
                      :label "Sort"
                      :input (if (get-val row 'sort)
                                 (with-html-string
                                   (render-edit-field "sort" 
                                                      (get-val row 'sort)
                                                      :type :span)
                                   (:input :type "hidden" :name "sort" 
                                           :value (get-val row 'sort)))
                                 (with-html-string
                                   (render-edit-field "sort" 
                                                      (get-val row 'sort)
                                                      :required t))))

              (render form-section 
                      :label "Sort Order"
                      :input (with-html-string
                               (render-edit-field "sort-order" 
                                                  (get-val row 'sort-order)
                                                  :type "number")))

              (render form-section 
                      :label "Sort Value"
                      :input (if (get-val row 'sort-value)
                                 (with-html-string
                                   (render-edit-field "sort-value" 
                                                      (get-val row 'sort-value)
                                                      :type :span)
                                   (:input :type "hidden" :name "sort-value" 
                                           :value (get-val row 'sort-value)))
                                 (with-html-string
                                   (render-edit-field "sort-value" 
                                                      (get-val row 'sort-value)
                                                      :required t))) )

              (render form-section 
                      :label "Alternate Sort Order"
                      :input (with-html-string
                               (render-edit-field "alternate-sort-order" 
                                                  (get-val row 'alternate-sort-order))))

              (render form-section 
                      :label "AA Sort Order"
                      :input (with-html-string
                               (render-edit-field "aa-sort-order" 
                                                  (get-val row 'aa-sort-order)
                                                  :type "number")))

              (render form-section 
                      :label "Description"
                      :input (if (get-val row 'description)
                                 (with-html-string
                                   (render-edit-field "description" 
                                                      (get-val row 'description)
                                                      :type :span))
                                 (with-html-string
                                   (render-edit-field "description" 
                                                      (get-val row 'description)
                                                      :required t))))
              (render form-section 
                      :label "Extended Description"
                      :input (with-html-string
                               (render-edit-field "extended-description" 
                                                  (get-val row 'extended-description))))
              ))))

(defun get-allsorts-exist (sort sort-value)
  (find-doc (allsorts-collection)
            :test
            (lambda (doc)
              (and       
               (not (string-equal (get-val doc 'doc-status) "superseded"))
               (string-equal (get-val doc 'sort) sort)
               (string-equal (get-val doc 'sort-value) sort-value)))))

(defmethod handle-action ((grid all-sorts-grid) (action (eql :save)))
  (when (string-equal (parameter "form-id") "allsorts-form")
    (let ((doc (editing-row grid)))
      (setf (get-val doc 'sort) (parameter "sort"))
      (setf (get-val doc 'sort-value) (parameter "sort-value"))
      (synq-edit-data doc)
      (setf (error-message grid) nil)
      (cond ((and (not (get-val doc 'xid))
                  (get-allsorts-exist 
                   (get-val doc 'sort)
                   (get-val doc 'sort-value)))
             (setf (error-message grid)
                   "Record already exists")
             (finish-editing grid))
            (t
             (persist doc))
            (finish-editing grid)) )))

(defmethod export-csv ((grid all-sorts-grid))
  (let* ((data (grid-filtered-rows grid)))
    (when data
      (with-output-to-string (stream)
        (format stream "~A~A~A Filtered by :~A ~%Context:~A~%~%" "Allsorts Export" 
                " Date:" (current-date-time) (or (get-val grid 'grid-filter ) "All Records") (print-context))
        (format stream "~A~%" "sort|sort-order|sort-value|multinational|alternate-sort-order|aa-sort-order|description|extended-description")
        (loop 
           for doc across data 
           do (format stream "~A|~A|~A|~A|~A|~A|~A|~%"
                          (get-val doc 'sort)
                          (get-val doc 'sort-order)
                          (get-val doc 'sort-value)
                          (get-val doc 'alternate-sort-order)
                          (get-val doc 'aa-sort-order)
                          (get-val doc 'description)
                          (get-val doc 'extended-description))
             )))))

(defmethod handle-action ((grid all-sorts-grid) (action (eql :cancel)))
  (finish-editing grid))
