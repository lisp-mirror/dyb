(in-package #:ems)

(defun comment-facebook (post from-user-id comment &key scheduled-date)
  (let* ((to-user-id (get-val (get-val post 'from) 'id))
         )
    
    (unless scheduled-date
      (multiple-value-bind (body)
          (drakma:http-request (format nil "https://graph.facebook.com/~A/comments&access_token=~A"
                                       (get-val post 'post-id)
                                       (get-val (get-service-user-by-user-id from-user-id) 'last-access-token))
                               :method :post
                               :parameters (list (cons "message"  (parameter "comment"))))

        #|(let ((error-message (get-facebook-error body) ))           
          (when error-message
            (setf (get-val action 'action-status) "Error")
            (setf (get-val action 'action-log) (cdr (car (rest error-message)))))

          (unless error-message 
            (setf (get-val action 'action-status) "Completed")
            (setf (get-val action 'action-log) "Posted comment successfully.")))|#
        ))
    ))


(defclass fb-post-comment-form (ajax-widget)
  ((current-post :initarg :current-post)
   (inner-id :initarg :inner-id)))

(defmethod action-handler ((widget fb-post-comment-form))
  ;;
  (when (and (string-equal (parameter "action") "post")
             (string-equal 
              (format nil "comments-~A-dialog-form" 
                      (parameter "post-id"))
              (name widget)))
    ;; (break "?~A ~a" (parameter "action") (parameter "post-id"))
    (comment-facebook (get-val widget 'current-post)
                      (if (get-val (get-val widget 'current-post) 'to)
                          (get-val (first (get-val (get-val widget 'current-post) 'to)) 'id)
                          (get-val (get-val (get-val widget 'current-post) 'from) 'id))
                      (parameter "comment"))
    (defer-js (format nil "$('#~a').dialog('close')" (name widget)))))

(defmethod render ((widget fb-post-comment-form) &key )

  (let ((comment-form (make-widget 'html-simple-framework-form 
                                   :name (get-val widget 'inner-id)
                                   :grid-size 12
                                   ;;:header "Comment"
                                  ; :form-id form-id
                                   ))
        (form-section (make-widget 'form-section
                                   :name "form-section"))
        (current-post (if (parameter "post-id")
                          (get-post-by-post-id (parameter "post-id")))))
    (setf (get-val comment-form 'form-id) (get-val widget 'inner-id))
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
                        "$('#comments-~a-dialog-form').dialog('open')"
                        (get-val current-post 'post-id)))
      (defer-js (format nil "$('#comments-~a-dialog-form').dialog({autoOpen: false, width: 900, height: 590})"
                        (get-val current-post 'post-id))))))



(defun social-mention-display (doc)
 ;; (break "~A" doc)
  (with-html-to-string ()
    (:div :class "nonboxy-widget"
          ;; (:div :class "widget-head")
          (:div :class "widget-content" :style "background:white;"
                
                (:div :class "post"

                      (:table :cellpadding "10" 
                              (:tr
                               (:td :width "82"
                                (:div :class "profile_pic"
                                      (:a :href (get-val doc 'user-link) :title "Go to Content"
                                          (:img  :src "/appimg/social-mention.jpg")))
                                (:div :class "social_icon"
                                      (htm (:img :src (get-val doc 'favicon)))
                                      ))
                               (:td 
                                (:div :class "post_title"
                                      (:h3 (get-val doc 'user))
                                      
                            
                                      )
                                (:div :class "date" (:p (str (format-universal-date (get-val doc 'time-stamp)))))
                                (:div :class "post_content"
                                      (str (get-val doc 'title))
                                      (:br)
                                      (str (get-val doc 'description)))
                                (:div :class "actions"
                                      (:div :class "go-to-content" :title "Go to Source"
                                            (:a :href (get-val doc 'link) (:img :src "/appimg/social-source.png")))
                            
                            
                                      )
                                )
                               (:td :width "150"
                                    (:div :class "post_image_thumb"
                                          (:a :href= "#" :title "Click to view the full size image"
                                              (:img :src (get-val doc 'image))))))))))))





(defun twitter-post-display (doc &key mention)

  (with-html-to-string ()
    (:div :class "nonboxy-widget"
          ;; (:div :class "widget-head")
          (:div :class "widget-content" :style "background:white;"


                (:div :class "post"
                      (:table :cellpadding "10"
                              (:tr
                               (:td :width "82"
                                (:div :class "profile_pic"
                                      (:a :href "#" :title "View ~A\'s Profile"
                                          (:img  :src (if (get-val doc 'user)
                                                          (get-val (get-val doc 'user) 'profile-image-url-https)))))
                                (:div :class "social_icon"
                                      (:img :src "/appimg/twitter.png")
                                      )
                                (:td
                                 (:div :class "post_title"

                                       (:h3 (str (get-val (get-val doc 'user) 'name))))
                                 (:div :class "date"
                                       (:p (str (get-val doc 'created-at))))
                                 (:div :class "post_content"
                                       (str (get-val doc 'text)))
                                 (:div :class "actions"
                                       (:div :class "reply" :title "Reply"
                                             (:a :href "#" (:img :src "/appimg/twitter-reply.png")))
                                       (:div :class "retweet" :title "Retweet"
                                             (:a :href "#" (:img :src "/appimg/twitter-retweet.png")))
                                       (:div :class "favourites" :title "Add to favourites"
                                             (:a :href "#" (:img :src "/appimg/twitter-favourite.png")))
                                       )
                                 )
                                (:td :width "150"
                                     (:div :class "post_image_thumb"
                                           (:a :href= "#" :title "Click to view the full size image"
                                               (:img :src ""))))))
                      
                      
                      
                      
                      
                      ))

          ))))




(defun facebook-post-display (doc &key mention)
  ;;(break "~A" doc)
  (with-html-to-string ()
    (:div :class "nonboxy-widget"
          ;; (:div :class "widget-head")
          (:div :class "widget-content"
                (:div :class "post"

                      (:table :cellpadding "10" 
                              (:tr
                               (:td :width "82"
                                (:div :class "profile_pic"
                                      (:a :href "#" :title (format nil "View ~A\'s Profile" 
                                                                   (if (get-val doc 'from)
                                                                       (get-val (get-val doc 'from) 'name)))
                                          ;;TODO: image parsing is broken????
                                          (:img :src (if (get-val doc 'from)
                                                         (if (get-val (get-val doc 'from) 'picture)
                                                             (rest (assoc "url" (rest (assoc "data" (get-val (get-val doc 'from) 'picture)  :test #'string-equal))  :test #'string-equal)))))))
                                (:div :class "social_icon" (:img :src "/appimg/facebook.png")))
                               (:td
                                (:div :class "post_title"
                                      (:h3 (str (get-val doc 'caption))))
                                (:div :class "date" (str (get-val doc 'created-time)))
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
                                                          (setf likes (get-val (get-val doc 'likes) 'count)))))

                                              (htm (str likes))))
                                      (:div :class "comment" :title "Comment"
                                            (:a :href "#" :onclick (format nil "$(\"#comments-~A\").toggle();" (get-val doc 'post-id))
                                                (:img :src "/appimg/facebook-comment.png")))
                                      (:div :class "comment_count"
                                            (str (if (get-val doc 'comments)
                                                     (if (typep
                                                          (get-val doc 'comments)
                                                          'comments)
                                           
                                                         (if (get-val (get-val doc 'comments) 'count)
                                                             (get-val (get-val doc 'comments) 'count)
                                                             0))
                                                     0)))
                                      (:div :class "share" :title "Share"
                                            (:a :href "#" (:img :src "/appimg/facebook-share.png")))
                                      )
                                )
                               (:td  :width "150"
                                (:div :class "post_image_thumb"
                                      (:a :href= "#" :title "Click to view the full size image"
                                          (:img :src (get-val doc 'picture)))))))


                      (:div :id (format nil "comments-~A" (get-val doc 'post-id)) 
                           :style "background-color:#F2F2F2;display:none;"
                           (:br)
                           (if (get-val doc 'comments)
                               (if (typep 
                                    (make-instance 'comments)
                                    'comments)
                                   ;;TODO: WTF how did the crap get into the comments?
                                   (if (listp (get-val (get-val doc 'comments) 'data))
                                       (dolist (comment (get-val (get-val doc 'comments) 'data))
                                         (typecase comment
                                           (comment
                                            (htm (:div (str (get-val comment 'message)))))
                                           )))))
                           (:br)

                           (let ((comment-form 
                                   (make-widget 'fb-post-comment-form
                                                :name (format nil 
                                                              "comments-~A-dialog-form" 
                                                              (get-val doc 'post-id)))))
                             
                             
                             (setf (get-val comment-form 'current-post) doc)

                             (setf (get-val comment-form 'inner-id) (format nil 
                                                                        "form-comments-~A" 
                                                                        (get-val doc 'post-id)))

                             (htm (:a :href
                                      (js-link 
                                       (js-render comment-form
                                                  (js-pair "post-id" 
                                                           (get-val doc 'post-id))
                                                  (js-pair "action" "comment")))
                                      "Post Comment")
                                  (render comment-form ))))))

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
    

    
    (render (make-widget 'page :name "generic-page")
            :body (with-html-to-string ()
                   
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


(define-easy-handler (manual-updates-page :uri "/ems/manual-updates") ()
  (let ((page (make-widget 'html-framework-page
                           :name "ajax-test")))
    (when (parameter "get-facebook-data")
      (update-facebook-posts-for-users))
    (when (parameter "get-search-stream-data")
      (update-social-mention-for-searches))
    (when (parameter "schedule-actions")
      (post-facebook-scheduled-actions))
    (when (parameter "get-twitter-old")
      (fetch-twitter-users-old))

    

    (render page :body
            (with-html-to-string ()
              (:form :name "fetch-data"
                     :method :post
                     (:input :type "submit" :name "get-facebook-data" 
                             :value "Get Facebook Data"))
              (:form :name "fetch-data"
                     :method :post
                     (:input :type "submit" :name "get-search-stream-data" 
                             :value "Get Search Stream Data"))
              (:form :name "fetch-data"
                     :method :post
                     (:input :type "submit" :name "schedule-actions" 
                             :value "Schedule Actions"))
              (:form :name "fetch-data"
                     :method :post
                     (:input :type "submit" :name "get-twitter-old" 
                             :value "Get Tweets"))))))

