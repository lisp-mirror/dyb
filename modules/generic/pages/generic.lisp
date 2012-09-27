(in-package #:ems)


(defun comment-facebook (post from-user-id comment &key scheduled-date)
  (let* (
         (to-user-id (get-val (get-val post 'from) 'id))
         (action (make-generic-action (get-val post 'post-id) 
                                          "Facebook Comment" 
                                          from-user-id 
                                          to-user-id
                                          "Comment"
                                          comment
                                          scheduled-date)))
    
    (unless scheduled-date
      (multiple-value-bind (body)
          (drakma:http-request (format nil "https://graph.facebook.com/~A/comments&access_token=~A"
                                       (get-val post 'post-id)
                                       (get-val (get-service-user-by-user-id from-user-id) 'last-access-token))
                               :method :post
                               :parameters (list (cons "message"  (parameter "comment"))))

        (let ((error-message (get-facebook-error body) ))           
          (when error-message
            (setf (get-val action 'action-status) "Error")
            (setf (get-val action 'action-log) (cdr (car (rest error-message)))))

          (unless error-message 
            (setf (get-val action 'action-status) "Completed")
            (setf (get-val action 'action-log) "Posted comment successfully.")))))
    (persist action)))


(defclass fb-post-comment-form (ajax-widget)
  ((current-post :initarg :current-post)))

(defmethod action-handler ((widget fb-post-comment-form))
  ;;
  ;;(break "?~a" (parameter "action"))
  (cond ((string-equal (parameter "action") "save")
         (if (string-equal 
              (format nil "comments-~A-dialog-form" 
                      (parameter "post-id"))
              (name widget))
             (comment-facebook (get-val widget 'current-post)
                               (if (get-val (get-val widget 'current-post) 'to)
                                   (get-val (first (get-val (get-val widget 'current-post) 'to)) 'id)
                                   (get-val (get-val (get-val widget 'current-post) 'from) 'id))
                               (parameter "comment"))
             ))
        (t
         nil))
  )

(defmethod render ((widget fb-post-comment-form) &key form-id)
  (let ((comment-form (make-widget 'html-simple-framework-form 
                                   :name form-id
                                   :grid-size 12
                                   ;;:header "Comment"
                                   :form-id form-id
                                   ))
        (form-section (make-widget 'form-section
                                   :name "form-section"))
        (current-post (if (parameter "post-id")
                          (get-post-by-post-id (parameter "post-id")))))
    (with-html 
      
      (if (parameter "action")
          (render comment-form
                  ;;:grid grid
                  :content
                  (with-html-to-string ()
                    (:div 
                           
                     (:input :type "hidden" :name "post-id" 
                             :value (get-val current-post 'post-id))
                     (:input :type "hidden" :name "from-user-id" 
                             :value (get-val (get-val current-post 'from) 'id))
                     (:input :type "hidden" :name "to-user-id" 
                             :value (if (get-val current-post 'to)
                                        (if (listp (get-val 
                                                    current-post 'to))
                                            (get-val (first (get-val current-post 'to)) 'id)
                                            (get-val (get-val current-post 'to) 'id))))
                     (:input :type "hidden" :name "action-type" 
                             :value "Facebook Comment")

                     (render form-section 
                             :label "Comment"
                             :input 
                             (with-html-to-string ()
                               (render-edit-field
                                "comment" 
                                (parameter "comment")
                                :required t
                                :type :textarea)))
                     
                     ))))
      

      (defer-js (format nil 
                        "$('#comments~a-dialog-form').dialog('open')"
                        (get-val current-post 'post-id)))
      (defer-js (format nil "$('#comments~a-dialog-form').dialog({autoOpen: false, width: 900, height: 590})"
                        (get-val current-post 'post-id)))

)))



(defun social-mention-display (doc)
 ;; (break "~A" doc)
  (with-html-to-string ()
    (:div :class "nonboxy-widget"
          ;; (:div :class "widget-head")
          (:div :class "widget-content" :style "background:white;"
                (:div :class "post"
                      (:div :class "post_image_thumb"
                            (:a :href= "#" :title "Click to view the full size image"
                                (:img :src (get-val doc 'image))))
                      (:div :class "profile_pic"
                            (:a :href (get-val doc'user-link) :title "View ~A\'s Profile"
                                (:img  :src (if (get-val doc 'user-image)
                                                (get-val doc 'user-image)
                                                "/appimg/social-mention.jpg"
                                                ))))
                      (:div :class "social_icon"
                            (:img :src (if (get-val doc 'favicon)
                                                    (get-val doc 'favicon)
                                                    "/appimg/social-mention.jpg"))
                            (cond ((string-equal (get-val doc 'source) "facebook")
                                   (htm (:img :src "/appimg/facebook.png")))
                                  ((string-equal (get-val doc 'source) "twitter")
                                   (htm (:img :src "/appimg/twitter.png")))
                                  )
                            )
                      (:div :class "post_title"
                            ;;(:h3 (get-val doc 'title))
                            (str (format-universal-date (get-val doc 'time-stamp)))
                            
                            )
                      (:div :class "post_content"
                            (str (get-val doc 'title))
                            (:br)
                            (str (get-val doc 'description)))
                      (:div :class "actions"
                            (:div :class "reply" :title "Go to Source"
                                  (:a :href (get-val doc 'link) (:img :src "/appimg/twitter-reply.png")))
                            
                            
                            )))

          )))





(defun twitter-post-display (doc &key mention)

  (with-html-to-string ()
    (:div :class "nonboxy-widget"
          ;; (:div :class "widget-head")
          (:div :class "widget-content" :style "background:white;"
                (:div :class "post"
                      (:div :class "post_image_thumb"
                            (:a :href= "#" :title "Click to view the full size image"
                                (:img :src "")))
                      (:div :class "profile_pic"
                            (:a :href "#" :title "View ~A\'s Profile"
                                (:img  :src (if (get-val doc 'user)
                                                (get-val (get-val doc 'user) 'profile-image-url-https)))))
                      (:div :class "social_icon"
                            (if mention
                                (htm (:img :src "/appimg/social-mention.jpg")))
                            (:img :src "/appimg/twitter.png"))
                      (:div :class "post_title"
                            (:h3 (str (format nil "~A..." (subseq (get-val doc 'text) 0 10)) )))
                      (:div :class "post_content"
                            (str (get-val doc 'text)))
                      (:div :class "actions"
                            (:div :class "reply" :title "Reply"
                                  (:a :href "#" (:img :src "/appimg/twitter-reply.png")))
                            (:div :class "retweet" :title "Retweet"
                                  (:a :href "#" (:img :src "/appimg/twitter-retweet.png")))
                            (:div :class "favourites" :title "Add to favourites"
                                  (:a :href "#" (:img :src "/appimg/twitter-favourite.png")))
                            )))

          )))




(defun facebook-post-display (doc &key mention)
  ;;(break "~A" doc)
  (with-html-to-string ()
    (:div :class "nonboxy-widget"
          ;; (:div :class "widget-head")
          (:div :class "widget-content"
                (:div :class "post"
                      (:div :class "post_image_thumb"
                            (:a :href= "#" :title "Click to view the full size image"
                                (:img :src (get-val doc 'picture))))
                      (:div :class "profile_pic"
                            (:a :href "#" :title (format nil "View ~A\'s Profile" 
                                                         (if (get-val doc 'from)
                                                             (get-val (get-val doc 'from) 'name)))
                                (:img :src (if (get-val doc 'from)
                                          (get-val (get-val doc 'from) 'picture)))))
                      (:div :class "social_icon"
                            (if mention
                                (htm (:img :src "/appimg/social-mention.jpg")))
                            (:img :src "/appimg/facebook.png"))
                      (:div :class "post_title"
                            (:h3 (str (get-val doc 'caption)))
                            (str (get-val doc 'created-time)))
                      (:div :class "post_content"
                            (str (if (get-val doc 'message)
                              (get-val doc 'message)
                              (get-val doc 'story))))
                      (:div :class "actions"
                            (:div :class "like" :title "Like"
                                  (:a :href "#" (:img :src "/appimg/facebook-like.png")))
                            (:div :class "like_count"
                                  (let ((likes 0))
                                    (if (get-val doc 'likes)
                                        (if (string-equal 
                                             (type-of (make-instance 'likes))
                                             "LIKES")
                                                 
                                            (if (get-val (get-val doc 'likes) 'count)
                                                (setf likes (get-val (get-val doc 'likes) 'count))))
                                        )
                                    (htm (str likes))))
                            (:div :class "comment" :title "Comment"
                                  (:a :href "#" :onclick (format nil "$(\"#comments-~A\").toggle();" (get-val doc 'post-id))
                                      (:img :src "/appimg/facebook-comment.png")))
                            (:div :class "comment_count"
                                  (str (if (get-val doc 'comments)
                                           (if (string-equal 
                                                (type-of (make-instance 'comments))
                                                "COMMENTS")
                                           
                                               (if (get-val (get-val doc 'comments) 'count)
                                                   (get-val (get-val doc 'comments) 'count)
                                                   0))
                                           0)))
                            (:div :class "share" :title "Share"
                                  (:a :href "#" (:img :src "/appimg/facebook-share.png")))
                            )
                      (:div :id (format nil "comments-~A" (get-val doc 'post-id)) 
                           :style "background-color:#F2F2F2;display:none;"
                           (:br)
                           (if (get-val doc 'comments)
                               (if (string-equal 
                                    (type-of (make-instance 'comments))
                                    "COMMENTS")
                                       
                                   (dolist (comment (get-val (get-val doc 'comments) 'data))
                                     (htm (:div (str (get-val comment 'message)))))))
                           (:br)

                           (let ((comment-form 
                                   (make-widget 'fb-post-comment-form
                                                :name (format nil 
                                                              "comments-~A-dialog-form" 
                                                              (get-val doc 'post-id)))))
                             
                             
                             (setf (get-val comment-form 'current-post) doc)
                             (htm (:a :href
                                      (js-link 
                                       (js-render comment-form
                                                  (js-pair "post-id" 
                                                           (get-val doc 'post-id))
                                                  (js-pair "action" "comment")))
                                      (make-icon "card--pencil"
                                                 :title "Post Comment"))
                                  (render comment-form :form-id (format nil 
                                                                        "comments-~A" 
                                                                        (get-val doc 'post-id))))))))

          )))

(defun generic-grid-item-display (doc)
  (if doc
      (typecase doc 
        (tweet
         (twitter-post-display doc)
         )
        (post
         (facebook-post-display doc)
         )
        (social-mention
         (social-mention-display doc)
         ))))



(define-easy-handler (generic-page :uri "/ems/generic") ()
  (let* ((columns
           (list
            (make-instance 
             'grid-column
             :name 'payload
             :header "Post"
             :width "100%"
             :printer #'generic-grid-item-display)))

           
         (grid (make-widget 'generic-grid :name "generic-post-gridxx"
                                       ;;:columns columns
                                       :edit-inline nil
                                       :title "Inbox"
                                       :row-object-class 'generic-entry)))
    (setf (get-val grid 'columns) columns)
    (setf (sort-direction grid) :descending)
    (setf (sort-key-function grid)
          (lambda (doc)
            (format nil "~A"  
                    
                    (get-val doc 'created))))
    
    (when (parameter "get-facebook-data")
      (update-facebook-posts-for-users grid))
    (when (parameter "get-search-stream-data")
      (update-social-mention-for-searches))
    
    (render (make-widget 'page :name "generic-page")
            :body (with-html-to-string ()
                    (:form :name "fetch-data"
                           :method :post
                           (:input :type "submit" :name "get-facebook-data" 
                                   :value "Get Facebook Data"))
                    (:form :name "fetch-data"
                           :method :post
                           (:input :type "submit" :name "get-search-stream-data" 
                                   :value "Get Search Stream Data"))
                    (str (render grid))))))


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


(defclass some-widget (ajax-widget)())

(defmethod render ((widget some-widget) &key)
(if (parameter "some-param1")
    (with-html :href
               (fmt "Hello ~a" (parameter "some-param")))))

(define-easy-handler (ajax-widget-page :uri "/ems/test-ajax") ()
  (let ((page (make-widget 'html-framework-page
                           :name "ajax-test"))
        (widget (make-widget 'some-widget :name "eish")))
    (render page
            :body
            (with-html-string
              (:a :href
                  (js-link 
                   (js-render widget
                              (js-pair "some-param" "testing")
                              (js-pair "some-param1" "fuck")))
                  (make-icon "card--pencil"
                             :title "Hello World Example."))
              (render widget)))))


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
