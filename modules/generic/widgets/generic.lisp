(in-package #:ems)

;;(populate-post-db-from-json (rest (first (json:decode-json-from-string *jsstr*))))

(defclass generic-grid (grid)
  ()
  (:default-initargs :edit-inline nil))

(defun get-generic-data (grid &key filter search)
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
              (generic-entry-collection))
   :sort-value-func (lambda (doc)
                      (get-val doc 'pid))
   :sort-test-func #'string<))

(defmethod get-rows ((grid generic-grid))
  (setf (rows grid)
	(get-generic-data grid 
                          :filter (grid-filter grid)  
                          :search (search-term grid))))

; as it says 
(defmethod render-row-editor ((grid generic-grid) row)

  (let ((form (make-widget 'peach-form :name "p-formx"
                                       :grid-size 12
                                       :header "Posts"
                                       :form-id "generic-edit-form"
                                       :grid-name (name grid)))
        (form-section (make-widget 'form-section
                                   :name "form-section"))
        )


    
    (render form
                            :content
                            (with-html-to-string ()
                              (render 
                               form-section
                               :label "Post ID"
                               :input (with-html-to-string ()
                                        (render-edit-field 
                                         "pid"
                                         (get-val row 'pid))))
                              (render 
                               form-section
                               :label "Title"
                               :input (with-html-to-string ()
                                        (render-edit-field 
                                         "title"
                                         (get-val row 'title)
                                         :type :textarea)))
                              (if (string= (get-val row 'type) "facebook")
                                  (if (get-val (get-val (get-val row 'payload) 'likes) 'count)
                                      (render 
                                       form-section
                                       :label "Likes"
                                       :input (with-html-to-string ()
                                                (render-edit-field 
                                                 "Count"
                                                 (write-to-string (get-val (get-val (get-val row 'payload) 'likes) 'count))
                                        ; (write-to-string (get-val (get-val row 'comments) 'count))
                                        ; (get-val row 'story)
                                        ;(get-val (first (get-val (get-val row 'comments) 'data)) 'message)
                                                 :type :textarea))))
                                  (if (get-val (get-val row 'payload) 'retweet-count)
                                      (render 
                                       form-section
                                       :label "Retweets"
                                       :input (with-html-to-string ()
                                                (render-edit-field 
                                                 "Count"
                                                 (write-to-string (get-val (get-val row 'payload) 'retweet-count))
                                        ; (write-to-string (get-val (get-val row 'comments) 'count))
                                        ; (get-val row 'story)
                                        ;(get-val (first (get-val (get-val row 'comments) 'data)) 'message)
                                                 :type :textarea))))
                                  )
                              (if (string= (get-val row 'type) "facebook")
                                  (dolist (com (get-val (get-val (get-val row 'payload) 'comments) 'data))
                                    (render 
                                     form-section
                                     :label "Comment"
                                     :input (with-html-to-string ()
                                              (render-edit-field 
                                               "Story"
                                               (get-val com 'message)
                                        ; (write-to-string (get-val (get-val row 'comments) 'count))
                                        ; (get-val row 'story)
                                        ;(get-val (first (get-val (get-val row 'comments) 'data)) 'message)
                                               :type :textarea)))))
                              #|			 (dolist (com (get-val (get-val row 'comments) 'data))
                              (render 
                              form-section
                              :label "Story"
                              :input (with-html-to-string ()
                              (render-edit-field 
                              "Story"
                              (get-val com 'message)
				   ; (write-to-string (get-val (get-val row 'comments) 'count)) ; ;
				   ; (get-val row 'story) ; ;
                                  ;(get-val (first (get-val (get-val row 'comments) 'data)) 'message) ; ;
                              :type :textarea))))|#
			 (render 
                          form-section
                          :label "Created"
                          :input (with-html-to-string ()
                                   (render-edit-field 
                                    "Created"
                                    (get-val row 'created)
                                    :type :textarea)))
                         ))
    ))
