(in-package #:ems)

;;(populate-post-db-from-json (rest (first (json:decode-json-from-string *jsstr*))))


(defclass generic-comments-grid (grid)
  ((parent-grid :initarg :parent-grid)
   (current-doc :initarg nil))
  (:default-initargs :edit-inline nil))

(defmethod get-rows ((grid generic-comments-grid))
  
  (when (and (get-val grid 'current-doc)
             (get-val (get-val grid 'current-doc) 
                      'payload)
             (get-val (get-val (get-val grid 'current-doc) 
                      'payload) 
                      'comments)
             (get-val (get-val (get-val (get-val grid 'current-doc) 
                      'payload) 
                               'comments)
                      'data))
    

    (setf (rows grid)
          (loop for comment across (coerce (get-val (get-val (get-val (get-val grid 'current-doc) 
                                                                      'payload) 
                                                             'comments)
                                                    'data) 'vector)
             collect comment))))

(defclass generic-grid (grid)
  ()
  (:default-initargs :edit-inline nil))

(defun get-generic-data (grid &key filter search)
  (declare (ignore grid search))
  (find-docs 'vector
              (lambda (doc)
             ;;   (if (match-context-entities (get-val doc 'payload) ))
                (cond ((equal filter 'with-audit-data)
                           doc)
                          (t 
                           (if (not (string-equal (get-val doc 'doc-status) "superseded"))
                               doc))))
              (generic-entry-collection)))

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
        (comment-form (make-widget 'peach-form :name "post-comment-form"
                                       :grid-size 12
                                       :header "Comment Post"
                                       :form-id "post-comment-edit-form"
                                       ))
        (form-section (make-widget 'form-section
                                   :name "form-section"))
        (tab-box (make-widget 'peach-tab-box
                              :name "post-tab-box"
                              :header "Posts"
                              :icon "card--pencil")))

    (setf (get-val tab-box 'header)
          (format nil "Post (~A)" 
                  (get-val row 'pid)))
    (setf (tabs tab-box)
          (list 
           (list
            "Post"
            (with-html-to-string ()
              (:div :class "section _100"
                    (render form  
                            :grid grid
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
                              
                              (if (string-equal (get-val row 'type) "facebook")
                                  (if (get-val (get-val (get-val row 'payload) 'likes) 'count)
                                      (render 
                                       form-section
                                       :label "Likes"
                                       :input (with-html-to-string ()
                                                (render-edit-field 
                                                 "Count"
                                                 (if (get-val (get-val row 'payload) 'likes)
                                                  (get-val (get-val (get-val row 'payload) 'likes) 'count))
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
				   ; (write-to-string (get-val (get-val row 'comments) 'count)) ; ; ; ; ; ; ;
				   ; (get-val row 'story) ; ; ; ; ; ; ;
                                  ;(get-val (first (get-val (get-val row 'comments) 'data)) 'message) ; ; ; ; ; ; ;
                              :type :textarea))))|#
			 (render 
                          form-section
                          :label "Created"
                          :input (with-html-to-string ()
                                   (render-edit-field 
                                    "Created"
                                    (get-val row 'created)
                                    :type :textarea)))
                         )))))
           (list 
            "Comments"
            (with-html-to-string ()
              (:div :class "section _100" 
                   (let* ((columns
                             (list
                              (make-instance 'grid-column
                                             :name 'from
                                             :header "From"
                                             :printer (lambda (from)
                                                        (get-val from 'name)))
                              (make-instance 'grid-column
                                             :name 'message
                                             :header "Comment")
                              (make-instance 'grid-column
                                             :name 'likes
                                             :header "Likes"
                                             )))
                           (comment-grid (make-widget 'generic-comments-grid 
                                                      :name "generic-comment-gridxx"
                                                      :columns columns
                                                      :edit-inline nil
                                                      :title "Comments"
                                                      :row-object-class 'comment)))

                      
                     (setf (get-val comment-grid 'parent-grid) grid)
                     (setf (get-val comment-grid 'current-doc) (editing-row grid))
                     
                      (render comment-grid)))))
           (list 
            "Comment Post"
            (with-html-to-string ()
              ;;(break "~A" row)
              (:div :class "section _100"
                   (render comment-form
                           :grid grid
                           :content
                           (with-html-to-string ()
                             (:input :type "hidden" :name "from-user-id" 
                                     :value (get-val (get-val (get-val row 'payload) 'from) 'id))
                             (:input :type "hidden" :name "to-user-id" 
                                     :value (if (get-val (get-val row 'payload) 'to)
                                                (if (listp (get-val (get-val row 'payload) 'to))
                                                    (get-val (first (get-val (get-val row 'payload) 'to)) 'id)
                                                    (get-val (get-val (get-val row 'payload) 'to) 'id))))
                             (render 
                               form-section
                               :label "Post ID"
                               :input (with-html-to-string ()
                                        (render-edit-field 
                                         "pid"
                                         (get-val row 'pid))))
                             (render form-section 
                                     :label "Entity"
                                     :input 
                                     (with-html-to-string ()
                                       (render-edit-field
                                        "comment" 
                                        ""
                                            
                                        :required t
                                        :type :input))))) ))
            )))
    (render tab-box)
    ))


(defun get-facebook-error (json-string)
  (assoc ':error  (json::decode-json-from-string json-string)))

(defun get-facebook-access-token (fb-id)
  (find-doc (service-users-collection)
            :test (lambda (doc)
                    (if (string-equal (get-val doc 'user-id) fb-id)
                        (return-from get-facebook-access-token doc)))))



(defmethod handle-action ((grid generic-grid) (action (eql 'save)))
  (setf (error-message grid) nil)
  (when (and (string-equal (parameter "form-id") "post-comment-edit-form"))
    (let ((from-user (get-facebook-access-token (parameter "from-user-id")))
          (to-user (get-facebook-access-token (parameter "to-user-id"))))
      (if (or from-user to-user)
          (multiple-value-bind (body)
              (drakma:http-request (format nil "https://graph.facebook.com/~A/comments&access_token=~A"
                                           (parameter "pid")
                                           (get-val (or from-user to-user) 'last-access-token))
                                   :method :post
                                   :parameters (list (cons "message"  (parameter "comment"))))
            (let ((error-message (get-facebook-error body) ))           
              (if error-message
                (setf (error-message grid) (cdr (car (rest error-message))))
                (setf (error-message grid) "Posted comment successfully."))))
          (setf (error-message grid) "User does not exist.")))))