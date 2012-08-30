(in-package #:ems)

(defun update-facebook-posts-for-users (grid)
  
  (dolist (user (find-docs 'list 
                           (lambda (doc)
                             (match-context-entities doc))
                            (service-users-collection)))
        
        (when (and user (string-equal (get-val user 'doc-status) "Active"))
          ;;TODO: How to get error messages in for users without access tokens.
          (when (get-val user 'last-access-token)
            (multiple-value-bind (bodyx)
                (drakma:http-request 
                 (format nil "https://graph.facebook.com/~A/feed?access_token=~A" 
                         (url-encode (get-val user 'user-id))
                         (get-val user 'last-access-token)))

              (let ((post-list (rest (first (json::decode-json-from-string bodyx)))))

                (if (populate-generic-db-from-post post-list )
                    (if (string-equal (car (car post-list)) "MESSAGE")
                  
                        (when grid 
                    
                          (setf (error-message grid)
                                (format nil (error-message grid) "~A~%~A%"
                                        (error-message grid)
                                        (car (cdr post-list))) ) ))))
              )))))

(define-easy-handler (generic-page :uri "/ems/generic") ()
  (let* ((columns
           (list
            (make-instance 'grid-column
                           :name 'payload
                           :header "From"
                           :printer (lambda (doc)
                                     (if doc
                                         (if (get-val doc 'from)
                                             (if (get-val (get-val doc 'from) 'name)
                                                 (get-val (get-val doc 'from) 'name)
                                                 0)
                                             0)
                                         0)))
            (make-instance 'grid-column
                           :name 'title
                           :header "Title")
            (make-instance 'grid-column
                           :name 'type
                           :header "Type")
           (make-instance 'grid-column
                           :name 'interaction
                           :header "Response")
           (make-instance 'grid-column
                          :name 'payload
                          :header "Likes"
                          :printer (lambda (doc)
                                      ;;TODO: See why action ended up in likes slot
                                     (if doc
                                         (if (get-val doc 'likes)
                                             (if (string-equal 
                                                  (type-of (make-instance 'comments))
                                                  "LIKES")
                                                 (if (get-val (get-val doc 'likes) 'count)
                                                     (get-val (get-val doc 'likes) 'count)
                                                     0))
                                             0)
                                         0)))
           (make-instance 'grid-column
                          :name 'payload
                          :header "Comments"
                          :printer (lambda (doc)
                                     ;;TODO: See why actions ended up in comments slot
                                     (if doc
                                         (if (get-val doc 'comments)
                                             (if (string-equal 
                                                  (type-of (make-instance 'comments))
                                                  "COMMENTS")
                                           
                                                 (if (get-val (get-val doc 'comments) 'count)
                                                     (get-val (get-val doc 'comments) 'count)
                                                     0))
                                               0)
                                         
                                         0)))
           (make-instance 'grid-column
                          :name 'created
                          :header "Created")
           ))
         (grid (make-widget 'generic-grid :name "generic-post-gridXx"
                                       :columns columns
                                       :edit-inline nil
                                       :title "Facebook Inbox"
                                       :row-object-class 'generic-entry)))
    
    (when (parameter "get-facebook-data")
      (update-facebook-posts-for-users grid))
    
    (render (make-widget 'page :name "generic-page")
            :body (with-html-to-string ()
                    (:form :name "fetch-data"
                           :method :post
                           (:input :type "submit" :name "get-facebook-data" 
                                   :value "Get Facebook Data"))
                    (str (render grid)))))


  
  )
