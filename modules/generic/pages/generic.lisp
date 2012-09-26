(in-package #:ems)


(defun comment-facebook (post from-user-id comment scheduled-date)
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
                                       (get-val post 'id)
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
  ;;(comment-facebook (get-val widget 'current-post))
  )

(defmethod render ((widget fb-post-comment-form) &key)
  (let ((comment-form (make-widget 'html-simple-framework-form 
                                   :name "fb-post-comment-formsxx"
                                   :grid-size 12
                                   ;;:header "Comment"
                                   :form-id "fb-post-comment-form"
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
                     (render 
                      form-section
                      :label "Scheduled Date"
                      :input (with-html-to-string ()
                               (render-edit-field 
                                "scheduled-date"
                                (parameter "scheduled-date")
                                :type :datetime-local)
                               ))
                     ))))
      

      (defer-js (format nil 
                        "$('#comments~a-dialog-form').dialog('open')"
                        (get-val current-post 'post-id)))
      (defer-js (format nil "$('#comments~a-dialog-form').dialog({autoOpen: false, width: 900, height: 590})"
                        (get-val current-post 'post-id)))

)))

#|
(make-instance 'social-mention
                                                         :id (assoc ':id mention)
                                                         :title (assoc ':title mention)
                                                         :link (assoc ':link mention)
                                                         :time-stamp (assoc ':timestamp mention)
                                                         :language (assoc ':language mention)
                                                         :image (assoc ':image mention)
                                                         :embeded (assoc ':embeded mention)
                                                         :user (assoc ':user mention)
                                                         :user-image (assoc ':user_image mention)
                                                         :user-link (assoc ':user_link mention)
                                                         :domain (assoc ':domain mention)
                                                         :source (assoc ':source mention)
                                                         :favicon (assoc ':favicon mention)
                                                         :type (assoc ':type mention))
|#

(defun social-mention-display (doc)
  (with-html-to-string ()
    (:div :class "nonboxy-widget"
          ;; (:div :class "widget-head")
          (:div :class "widget-content" :style "background:white;"
                (:table 
                      :style "width:100%;border-bottom: 1px;border-spacing: 0px;border-bottom-style:solid;border-collapse: collapse;border-color:lightgray;"
                      (:tr 
                       (:td :rowspan 1 :style "width:32px;border-width: 0px;padding: 0px;border-style: none;border-color:-moz-border-radius: ;"
                            (if (get-val doc 'user-image)
                                (htm
                                                      
                                 (:img :style "padding: 3px;height:50%;" :src (get-val doc 'user-image)))
                                ""))
                       (:td :style "border-width: 0px;padding: 0px;border-style: none;border-color: gray;-moz-border-radius: ;"
                            (:a :href (get-val doc 'user-link) (:strong (str (get-val doc 'user))))
                            (:br)
                            (str (get-val doc 'time-stamp)))))
                (cond ((string-equal (get-val doc 'source) "facebook")
                       (htm (:img :style "float:right;width:16px;height:16px;" 
                                  :src "/appimg/facebook.png"))))
                (:span (:string "Title : "))
                (str (get-val doc 'title))
                (:br)
                
                (str (get-val doc 'description))
                (if (get-val doc 'image)
                    (htm
                     (:img :style "padding: 3px;height:100%;" :src (get-val doc 'image))))
                (:br)
                (:a :href (get-val doc 'link) (str "Go to Source")))
          )))

(defun generic-grid-item-display (doc)
  (if doc
      (typecase doc 
        (tweet
         (if (get-val doc 'user)
             (get-val (get-val doc 'user) 'name)))
        (post
         (facebook-post-display doc)
         )
        (social-mention
         (social-mention-display doc)
         ))))

(defun facebook-post-display (doc)
  
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
                            (str (get-val doc 'created-time)))))
                     (:img :style "float:right;width:16px;height:16px;" 
                                 :src "/appimg/facebook.png")                             
                     (str (if (get-val doc 'message)
                              (get-val doc 'message)
                              (get-val doc 'story)))
                     (if (get-val doc 'picture)
                         (htm
                          (:img :style "padding: 3px;height:100%;" :src (get-val doc 'picture))))
                     (:br)
                     (:div :style "width:100%;background-color:white;"
                           (:span :style "float:right;"
                                  (str (if (get-val doc 'comments)
                                           (if (string-equal 
                                                (type-of (make-instance 'comments))
                                                "COMMENTS")
                                           
                                               (if (get-val (get-val doc 'comments) 'count)
                                                   (get-val (get-val doc 'comments) 'count)
                                                   0))
                                           0)))
                           (:img :style "float:right; " :src "/appimg/fb-comment.png" 
                                 :onclick (format nil "$(\"#comments-~A\").toggle();" (get-val doc 'post-id)) )
                           (:span :style "float:right;"
                                  (let ((likes 0))
                                    (if (get-val doc 'likes)
                                        (if (string-equal 
                                             (type-of (make-instance 'likes))
                                             "LIKES")
                                                 
                                            (if (get-val (get-val doc 'likes) 'count)
                                                (setf likes (get-val (get-val doc 'likes) 'count))))
                                        )
                                    (htm (str likes))))
                           (:img :style "float:right;width:16px;height:16px;" 
                                 :src "/appimg/fb-like.jpeg"))

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
                             ;; (setf (get-val comment-form 'current-post) doc)
                             (htm (:a :href
                                      (js-link 
                                       (js-render comment-form
                                                  (js-pair "post-id" 
                                                           (get-val doc 'post-id))
                                                  (js-pair "action" "comment")))
                                      (make-icon "card--pencil"
                                                 :title "Post Comment"))
                                  (render comment-form)))))
                "")))))

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
