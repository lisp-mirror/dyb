(in-package :dyb)

(defun comment-facebook (post from-user-id comment &key scheduled-date)
  (let* ((to-user-id (get-val (get-val post 'from) 'id))
         )
    
    (unless scheduled-date
      (multiple-value-bind (body)
          (drakma:http-request (format nil "https://graph.facebook.com/~A/comments&access_token=~A"
                                       (get-val post 'post-id)
                                       (get-val (get-channel-user-by-user-id from-user-id) 'last-access-token))
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
    (defer-js (format nil "$('#~a').dialog('close')" (name widget)))
    (finish-editing  widget)))

(defmethod render ((widget fb-post-comment-form) &key )

  (let* ((comment-form (make-widget 'html-simple-framework-form 
                                    :name (get-val widget 'inner-id)
                                    :grid-size 12
                                    ;;:header "Comment"
                                        ; :form-id form-id
                                    ))
         (form-section (make-widget 'form-section
                                    :name "form-section"))
         (current-post (if (parameter "post-id")
                           (get-generic-post-by-post-id (parameter "post-id"))))
         (post-id (gpv current-post :id)))
    (setf (get-val comment-form 'form-id) (get-val widget 'inner-id))
    (with-html 
      
      (if (parameter "action")
          (render comment-form
                  ;;:grid grid
                  :content
                  (with-html-to-string ()
                    (:div 
                           
                     (:input :type "hidden" :name "post-id" 
                             :value post-id)

                     (:input :type "hidden" :name "from-user-id" 
                             :value (gpv current-post :from :id))
                     (:input :type "hidden" :name "to-user-id" 
                             :value (gpv current-post :to :id))
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
                                :type :textarea)))))))
      (defer-js
          (format nil 
                  "$('#comments-~a-dialog-form').dialog('open')"
                  post-id))
      (defer-js
          (format nil
                  "$('#comments-~a-dialog-form').dialog({autoOpen: false, width: 900, height: 500})"
                  post-id)))))



(defun social-mention-display (doc)
 ;; (break "~A" doc)
  (with-html-to-string ()
    (:div :class "nonboxy-widget"
          ;; (:div :class "widget-head")
          (:div :class "widget-content" :style "background:white;"
                
                (:div :class "post"

                      (:a
                       :class "post-title"
                       :href (or (gpv doc :user--link)
                                 (gpv doc :link))
                       (str (gpv doc :title)))
                      (if (gpv doc :user)
                          (htm
                           (:span :class "twitter-user" (:a :href (gpv doc :user--link) (gpv doc :user)))))
                      (:span :class "timestamp"
                             (str (format-universal-date-time  
                                   (unix-time-to-universal 
                                    (gpv doc :timestamp)))))
                      (:span :class "post-content"
                             (str (gpv doc :description)))
                      (:span :class "twitter-actions"
                             (:span :class "action-icon" :title "Go to Content"
                                    (:a :href (gpv doc :link) (:img :src "/appimg/go-to-content.png"))))

                      )))))





(defun twitter-post-display (doc &key mention)
  (with-html-to-string ()
    (:div 
     :class "nonboxy-widget"
     (:div 
      :class "widget-content" :style "background:white;"
      (:div 
       :class "post"
       (:a
        :class "post-title"
        :href (format nil "http://www.twitter.com/~A" 
                      (gpv doc :user :id))
        (str (gpv doc :user :name)))
       (:span :class "twitter-user" 
              (:a :href (format nil "http://www.twitter.com/~A" 
                                (gpv doc :user :screen-name))))
       (:span :class "timestamp"
              (str (gpv doc :created-at)))
       (:span :class "post-content"
              (str (gpv doc :text)))
       (:span :class "twitter-actions"
              (:span :class "action-icon" :title "Reply"
                     (:a :href "#" (:img :src "/appimg/twitter-reply.png")))
                                       
              (:span :class "action-icon" :title "Retweet"
                     (:a :href "#" (:img :src "/appimg/twitter-retweet.png")))
                                       
              (:span :class "action-icon" :title "Add to favourites"
                     (:a :href "#" (:img :src "/appimg/twitter-favourite.png")))
              )

       )

      ))))

(defun linkedin-post-display (doc &key mention)
  (with-html-to-string ()
    (:div 
     :class "nonboxy-widget"
     (:div 
      :class "widget-content" :style "background:white;"
      (:div 
       :class "post"
       (:a
        :class "post-title"
        :href (format nil "http://www.linkedin.com/~A" 
                      (cond ((string-equal (gpv doc :update-type) "SHAR")
                             (gpv doc :update-content :person :id))
                            ((string-equal (gpv doc :update-type) "CMPY")
                             (gpv doc :update-content :company :id))))
        (str (cond ((string-equal (gpv doc :update-type) "SHAR")
                    (gpv doc :update-content :person :first-name))
                   ((string-equal (gpv doc :update-type) "CMPY")
                    (gpv doc :update-content :company :name)))))
       (:span :class "twitter-user" 
              (:a :href (format nil "http://www.linkedin.com/~A" 
                                (cond ((string-equal (gpv doc :update-type) "SHAR")
                                       (gpv doc :update-content :person :id))
                                      ((string-equal (gpv doc :update-type) "CMPY")
                                       (gpv doc :update-content :company :id))))))
       (:span :class "timestamp"
              (str (format-universal-date-time (unix-time-to-universal (truncate (/ (gpv doc :timestamp) 1000))))))
       (:span :class "post-content"
              (str (cond ((string-equal (gpv doc :update-type) "SHAR")
                          (or (gpv doc :update-content :person :current-share :content :description)
                              (gpv doc :update-content :person :current-share :comment)))
                         ((string-equal (gpv doc :update-type) "CMPY")
                          (gpv doc :update-content :company-status-update :share :comment)))))
       (:span :class "twitter-actions"
              (:span :class "action-icon" :title "Like"
                     (:a :href "#" "Like"))
                                       
              (:span :class "action-icon" :title "Comment"
                     (:a :href "#" "Comment"))
                                       
              (:span :class "action-icon" :title "Share"
                     (:a :href "#" "Share"))
              )

       )

      ))))




(defun facebook-post-display (doc &key mention)
;;(break "~A" doc)
  (with-html-to-string ()
    (:div 
     :class "nonboxy-widget"
     (:div 
      :class "widget-content"
      (:div 
       :class "post"
       (:a
        :class "post-title"
        :href (format nil "http://www.facebook.com/~A" (gpv doc :from :id))
        (str (gpv doc :from :name)))
       (:span :class "timestamp"
              (str (gpv doc 'created--time)))
       (:span :class "post-content"
              (str (or (gpv doc :message) (gpv doc :story))))
       (:span :class "twitter-actions"
              (:span :class "action-icon" :title "Like"
                     (:a :href (js-link 
                                (js-render nil (js-pair "action" "like"))) 
                         (:img :src "/appimg/fb-like.png")))
              (:span :class "action-icon" (str (if (gpv doc :likes :count)
                                                   (gpv doc :likes :count)
                                                   0)))
              (:span :class "action-icon" :title "Comment"
                     (:a :href "#" 
                         :onclick (format nil "$(\"#comments-~A\").toggle(); return false;"
                                          (gpv doc :id))
                         (:img :src "/appimg/fb-comment.png")))
              (:span :class "action-icon" (str (gpv doc :comments :count)))
              (:span :class "action-icon" :title "Share"
                     (:a :href "#" (:img :src "/appimg/fb-share.png")))
              )


       (:div :id (format nil "comments-~A" (gpv doc :id)) 
             :style "background-color:#F2F2F2;display:none;"
             (:br)
                           
             (dolist (comment (gpv doc :comments :data))
               (htm (:div :style "border-bottom:thin solid #ffffff;" 
                          (str (gpv comment :message)))))
             (:br)

             (let ((comment-form 
                    (make-widget 'fb-post-comment-form
                                 :name (format nil 
                                               "comments-~A-dialog-form" 
                                               (gpv doc :id)))))
                             
                             
               (setf (get-val comment-form 'current-post) doc)

               (setf (get-val comment-form 'inner-id) (format nil 
                                                              "form-comments-~A" 
                                                              (gpv doc :id)))

               (htm (:a :href
                        (js-link 
                         (js-render comment-form
                                    (js-pair "post-id" 
                                             (gpv doc :id))
                                    (js-pair "action" "comment")))
                        "Post Comment")
                    (render comment-form ))))))

     )))

(defun generic-grid-item-display (payload)
  (if payload
      (cond ((gpv payload :id--str)
             (twitter-post-display payload))
            ((gpv payload :favicon)
             (social-mention-display payload))
            ((gpv payload :update-type)
             (linkedin-post-display payload))
            (t
             (facebook-post-display payload)))))



(define-easy-handler (generic-page :uri "/dyb/generic") ()
  (let* ((columns
          (list
           (make-instance 
            'grid-column
            :name 'post-type
            :header "Source"
            :printer 
            (lambda (source)
              (cond ((equal source 'twitter)
                     (with-html-to-string ()
                       (:span :class "post-source" 
                              (:img :src "/appimg/twitter-bird-white-on-blue.png"))
                       ))
                    ((equal source 'facebook)
                     (with-html-to-string ()
                       (:span :class "post-source" 
                              (:img :src "/appimg/Facebook_Light_Logo.png"))
                       ))
                    ((equal source 'social-mention)
                     (with-html-to-string ()
                       (:span :class "post-source" 
                              (:img :src "/appimg/social-mention.png"))
                       ))
                    ((equal source 'linkedin)
                     (with-html-to-string ()
                       (:span :class "post-source" 
                              (:img :src "/appimg/linkedin-icon.png"))
                       ))))
            )
           (make-instance 
            'grid-column
            :name 'payload
            :header "Pic"
            :printer 
            (lambda (payload)
              (cond ((gpv payload :id--str)
                     (with-html-to-string ()
                       (:span :class "post-source" 
                              (:img :src (if (gpv payload :user :profile--image--url--https)
                                             (gpv payload :user :profile--image--url--https)
                                             "/appimg/user-thumb.png")))
                       ))
                    ((gpv payload :favicon)
                     (with-html-to-string ()
                       (:span :class "user-thumb" 
                              (:img :src (if (gpv payload :user--image)
                                             (gpv payload :user--image)
                                             "/appimg/user-thumb.png")))
                       ))
                    ((gpv payload :update-type)
                     (with-html-to-string ()
                       (:span :class "user-thumb" 
                              (:img :src (if (gpv payload :update-content :person :picture-url)
                                             (gpv payload :update-content :person :picture-url)
                                             "/appimg/user-thumb.png")))
                       )
                     )
                    (t
                     (with-html-to-string ()
                       (:span :class "post-source" 
                              (:img :src "/appimg/user-thumb.png"))
                       ))))

            )
            
           (make-instance 
            'grid-column
            :name 'payload
            :header "Post"
            :width "100%"
            :printer #'generic-grid-item-display)
           (make-instance 'grid-column
                          :name 'payload
                          :header "Image"
                          :printer (lambda (payload)
                                     (cond ((gpv payload :id--str)
                                            (with-html-to-string ()
                                              (:div :class "post-image-thumb"
                                                    (:a :href "#" :title "Click to view the full size image"
                                                        ;;(:img :src "")
                                                        ))
                                              ))
                                           ((gpv payload :favicon)
                                            (with-html-to-string ()
                                              (if (gpv payload :image)
                                                  (htm
                                                   (:div :class "post-image-thumb"
                                                         (:a :href (gpv payload :image) :title "Click to view the full size image"
                                                             (:img :src (gpv payload :image))
                                                             ))))
                                              ))
                                           ((gpv payload :update-type)
                                            (with-html-to-string ()
                                              (let ((image (or
                                                            (gpv payload :update-content :person :current-share :content :thumbnail-url)
                                                            (gpv payload :update-content :company-status-update :share :content :thumbnail-url))))
                                                (if image
                                                    (htm
                                                     (:div :class "post-image-thumb"
                                                           (:a :href image
                                                               :title "Click to view the full size image"
                                                               (:img :src (gpv payload :update-content :person :current-share :content :thumbnail-url))
                                                               )))))
                                              )
                                            )
                                           (t
                                            (with-html-to-string ()
                                              (if (gpv payload :picture)
                                                  (htm
                                                   (:div :class "post-image-thumb"
                                                         (:a :href "#" :title "Click to view the full size image"
                                                              
                                                             (:img :src (gpv payload :picture))))))
                                              ))
                                           ))

                          )))

           
         (grid (make-widget 'generic-grid :name "generic-post-gridxx"
                            ;;:columns columns
                            :edit-inline nil
                            :title "Inbox"
                            :row-object-class 'generic-post)))
    (setf (get-val grid 'columns) columns)
    (setf (get-val grid 'grid-links) (list (list 'twitter "delete" "Delete")
                                           (list 'twitter "unfollow" "Unfollow")
                                           (list 'twitter "direct-messaeg" "Direct Message")
                                           (list 'twitter "assign-task" "Assign Task")
                                           (list 'twitter "highlight" "Highlight")
                                           (list 'facebook "delete" "Delete")
                                           (list 'facebook "block-user" "Block User")
                                           (list 'facebook "direct-messaeg" "Direct Message")
                                           (list 'facebook "assign-task" "Assign Task")
                                           (list 'facebook "highlight" "Highlight")
                                           (list 'social-mention "delete" "Delete")))
    (setf (sort-direction grid) :descending)
    (setf (sort-key-function grid)
          (lambda (doc)
            (format nil "~A"  
                    (get-val doc 'created-date))))
    
    (render (make-widget 'page :name "generic-page")
            :body (with-html-to-string ()
                   
                    (str (render grid))))))


(define-easy-handler (scheduled-page :uri "/dyb/scheduled") ()
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

(define-easy-handler (ajax-widget-page :uri "/dyb/test-ajax") ()
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


(define-easy-handler (manual-updates-page :uri "/dyb/manual-updates") ()
  (let ((page (make-widget 'html-framework-page
                           :name "ajax-test")))
    (when (parameter "get-facebook-data")
      (facebook-refresh-feeds))
    (when (parameter "get-search-stream-data")
      (social-mention-refresh-searches))
    (when (parameter "schedule-actions")
      (post-facebook-scheduled-actions))
    (when (parameter "get-twitter-old")
      (twitter-refresh-home-timelines))
    (when (parameter "get-linkedin-updates")
      (linkedin-refresh-updates))

    

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
                             :value "Get Tweets"))
              (:form :name "fetch-data"
                     :method :post
                     (:input :type "submit" :name "get-linkedin-updates" 
                             :value "Get LinkedIn Updates"))))))

