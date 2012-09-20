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
                 (format nil "https://graph.facebook.com/~A/feed?limit=2000&access_token=~A" 
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
					 (typecase doc 
					   (tweet
					    (if (get-val doc 'user)
						(get-val (get-val doc 'user) 'name)))
					   (post
					    (if (get-val doc 'from)
						(if (get-val (get-val doc 'from) 'name)
						    (get-val (get-val doc 'from) 'name)
						    "Unknown Source")
						"Unknown Source")
					    )
					   (t "Unknown Source")))))
            (make-instance 'grid-column
                           :name 'title
                           :header "Title"
                            :width "250px;")
            (make-instance 'grid-column
                           :name 'type
                           :header "Type"
                           :width "70px;"
                           :printer (lambda (post-type)
                                      (cond ((string-equal post-type "Facebook")
                                             (with-html-to-string ()
                                               (:img :style "width:16px;height:16px;" :src "/appimg/facebook.png")))
                                            ((string-equal post-type "Twitter")
                                             (with-html-to-string ()
                                               (:img :style "width:16px;height:16px;" :src "/appimg/facebook.png"))))))
           (make-instance 'grid-column
                           :name 'interaction
                           :header "Response"
                           :width "80px;")
           (make-instance 'grid-column
                          :name 'payload
                          :header "Likes"
                          :width "70px;"
                          :printer (lambda (doc)
                                      ;;TODO: See why action ended up in likes slot
                                     
                                     (if doc
					 (typecase doc 
					   
					   (post
                                            
					    (if (get-val doc 'likes)
                                             (if (string-equal 
                                                  (type-of (make-instance 'comments))
                                                  "LIKES")
                                                 (if (get-val (get-val doc 'likes) 'count)
                                                     (with-html-to-string ()
                                                       (:img :style "width:16px;height:16px;" :src "/appimg/fb-like.jpeg")
                                                       (str (get-val (get-val doc 'likes) 'count))
)
                                                     (with-html-to-string ()
                                                       (:img :style "width:16px;height:16px;" :src "/appimg/fb-like.jpeg")
                                                       (str 0)
                                                       ))
                                                 (with-html-to-string ()
                                                       (:img :style "width:16px;height:16px;" :src "/appimg/fb-like.jpeg")
                                                       (str 0)
                                                       ))
                                             (with-html-to-string ()
                                                       (:img :style "width:16px;height:16px;" :src "/appimg/fb-like.jpeg")
                                                       (str 0)
                                                       ))
					    )
					   (t "??"))
                                         
                                         "?-?")))
           (make-instance 'grid-column
                          :name 'payload
                          :header "Comments"
                          :width "80px;"
                          :printer (lambda (doc)
                                     ;;TODO: See why actions ended up in comments slot
                                     (format nil "~A" (if doc
                                                     (typecase doc
                                                       (post
                                                        (if (get-val doc 'comments)
                                                            (if (string-equal 
                                                                 (type-of (make-instance 'comments))
                                                                 "COMMENTS")
                                           
                                                                (if (get-val (get-val doc 'comments) 'count)
                                                                    (get-val (get-val doc 'comments) 'count)
                                                                    0))
                                                            0)
					    
                                                        )
                                                       (t 0))))))
           (make-instance 'grid-column
                          :name 'created
                          :header "Created")
           ))
         (grid (make-widget 'generic-grid :name "generic-post-gridzzssrzrssssssppsstsxs0sss"
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


(define-easy-handler (scheduled-page :uri "/ems/scheduled") ()
  (let* ((columns
                             (list
                              (make-instance 'grid-column
                                             :name 'pid
                                             :header "Post Id"
                                             )
                              (make-instance 'grid-column
                                             :name 'action
                                             :header "Action")
                              (make-instance 'grid-column
                                             :name 'scheduled-date
                                             :header "Scheduled Date"
                                             )))
                           (action-grid (make-widget 'generic-actions-grid 
                                                      :name "generic-actions-grid"
                                                      :columns columns
                                                      :edit-inline nil
                                                      :title "Actions"
                                                      :row-object-class 'generic-action)))

                      
            
            
                     (render (make-widget 'page :name "scheduled-page")
                             :body (with-html-to-string ()
                                     
                                     (str (render action-grid))))
                     ))