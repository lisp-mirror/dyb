(in-package #:ems)

(define-easy-handler (generic-page :uri "/ems/generic") ()
  (let* ((columns
           (list
            (make-instance 
             'grid-column
             :name 'payload
             :header "Post"
             :width "100%"
             :printer (lambda (doc)
                        (if doc
                            (typecase doc 
                              (tweet
                               (if (get-val doc 'user)
                                   (get-val (get-val doc 'user) 'name)))
                              (post
                               (with-html-to-string ()
                                 (:div :class "nonboxy-widget"
                                       ;; (:div :class "widget-head")
                                       (:div :class "widget-content" :style "background:white;"
                                             (if (get-val doc 'from)
                                                 (htm
                                                  (:table 
                                                      :style "width:100%;border-bottom: 1px;border-spacing: 0px;border-bottom-style:solid;border-collapse: collapse;border-color:lightgray;"
                                                      (:tr 
                                                       (:td :rowspan 1 :style "width:32px;border-width: 0px;padding: 0px;border-style: none;border-color:-moz-border-radius: ;"
                                                            (if (get-val (get-val doc 'from) 'picture)
                                                                (htm
                                                      
                                                                 (:img :style "padding: 3px;height:50%;" :src (get-val (get-val doc 'from) 'picture)))
                                                                ""))
                                                       (:td :style "border-width: 0px;padding: 0px;border-style: none;border-color: gray;-moz-border-radius: ;"
                                                            (:strong (str (get-val (get-val doc 'from) 'name)))
                                                            (:br)
                                                            (str (get-val doc 'created-time)))
                                                       
                                                       )
                                                          
                                                    
                                                      )
                                                  
                                                  (str (if (get-val doc 'message)
                                                           (get-val doc 'message)
                                                           (get-val doc 'story)))
                                                  (if (get-val doc 'picture)
                                                      (htm
                                                       (:img :style "padding: 3px;height:100%;" :src (get-val doc 'picture))))
                                                  (:br)
                                                  (:div :style "width:100%;background-color:green;"
                                                   (:span :style "float:right;"
                                                          (str (if (get-val doc 'comments)
                                                                   (if (string-equal 
                                                                        (type-of (make-instance 'comments))
                                                                        "COMMENTS")
                                           
                                                                       (if (get-val (get-val doc 'comments) 'count)
                                                                           (get-val (get-val doc 'comments) 'count)
                                                                           0))
                                                                   0)))
                                                   (:img :style "float:right; " :src "/appimg/fb-comment.png"))
                                                  )
                                                 
                                                 ""))
                                       )))))))
#|            (make-instance 
             'grid-column
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
            (make-instance 
             'grid-column
             :name 'payload
             :header "Picture"
             :printer (lambda (doc)
                        (if doc
                            (typecase doc 
                              (tweet
                               (if (get-val doc 'user)
                                   (get-val (get-val doc 'user) 'name)))
                              (post
                               (if (get-val doc 'from)
                                     (if (get-val (get-val doc 'from) 'picture)
                                         (with-html-to-string ()
                                               (:img :style "width:32px;height:32px;" :src (get-val (get-val doc 'from) 'picture))) ;
                                               "")
                                                  "")
                                            
                                                 )
                                             (t "")))))
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
|#
           (make-instance 'grid-column
                          :name 'created
                          :header "Created")))
         (grid (make-widget 'generic-grid :name "generic-post-grid"
                                       ;;:columns columns
                                       :edit-inline nil
                                       :title "Facebook Inbox"
                                       :row-object-class 'generic-entry)))
    (setf (get-val grid 'columns) columns)
    
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