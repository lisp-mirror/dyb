(in-package :dyb)


(defun social-mention-display (grid row col-val row-id)
 ;; (break "~A" col-val)
  (with-html-to-string ()
    (:div :class "nonboxy-widget"
          ;; (:div :class "widget-head")
          (:div :class "widget-content" :style "background:white;"
                
                (:div :class "post"

                      (:a
                       :class "post-title"
                       :href (or (gpv col-val :user--link)
                                 (gpv col-val :link))
                       (str (gpv col-val :title)))
                      (if (gpv col-val :user)
                          (htm
                           (:span :class "twitter-user" (:a :href (gpv col-val :user--link) (gpv col-val :user)))))
                     ;; (:span :class "timestamp"
                     ;;        (str (format-universal-date-time  
                     ;;              (unix-time-to-universal 
                     ;;               (gpv col-val :timestamp)))))
                      (:span :class "post-content"
                             (str (gpv col-val :description)))
                      (:span :class "twitter-actions"
                             (:span :class "action-icon" :title "Go to Content"
                                    (:a :href (gpv col-val :link) (:img :src "/appimg/go-to-content.png"))))

                      )))))

(defun twitter-post-display (grid row col-val row-id)
;(break "~A~%~A" col-val (gpv col-val :created--at)) 
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
                      (gpv col-val :user :id))
        (str (gpv col-val :user :name)))
       (:span :class "twitter-user" 
              (:a :href (format nil "http://www.twitter.com/~A" 
                                (gpv col-val :user :screen--name))))
       ;;(:span :class "timestamp"
       ;;       (str (gpv col-val :created--at)))
       (:span :class "post-content"
              (str (gpv col-val :text)))
       (:span :class "twitter-actions"
              (:span :class "action-icon" :title "Reply"
                     (:a :href (js-link 
                                (js-render (editor grid)
                                           (js-pair "grid-name" (name grid))
                                           (js-pair "action" "reply-twitter-form")
                                           (js-pair "row_id" row-id))) 
                                  ;;(:img :src "/appimg/twitter-reply.png")
                                  (str "Reply")
                                  )

                     )
                                       
              (:span :class "action-icon" :title "Retweet"
                     (:a :href (js-link 
                                (js-render (editor grid)
                                           (js-pair "grid-name" (name grid))
                                           (js-pair "action" "retweet-twitter-form")
                                           (js-pair "row_id" row-id))) 
                                  ;;(:img :src "/appimg/twitter-retweet.png")
                                  (str "Retweet")
                                  )
                     )
                                       
              (:span :class "action-icon" :title "Add to favourites"
                     (:a :href (js-link 
                                (js-render (editor grid)
                                           (js-pair "grid-name" (name grid))
                                           (js-pair "action" "favourite-twitter")
                                           (js-pair "row_id" row-id))) 
                                  ;;(:img :src "/appimg/twitter-favourite.png")
                                  (str "Add to Favourites")
                                  )
                     )
              )

       )

      ))))

(defun facebook-post-display (grid row col-val row-id)
  (declare (ignore row))
;;(break "~A" col-val)
  (with-html-to-string ()
    (:div 
     :class "nonboxy-widget"
     (:div 
      :class "widget-content"
      (:div 
       :class "post"
       (:a
        :class "post-title"
        :href (format nil "http://www.facebook.com/~A" (gpv col-val :from :id))
        (str (gpv col-val :from :name)))
;;       (:span :class "timestamp"
;;              (str (gpv col-val :created--time)))
       (:span :class "post-content"
              (str (or (gpv col-val :message) (gpv col-val :story))))
       (if (gpv col-val :actions)
           (htm (:span :class "twitter-actions"
                       (:span :class "action-icon" :title "Like"
                              (:a :href (js-link 
                                         (js-render (editor grid)
                                                    (js-pair "grid-name" (name grid))
                                                    (js-pair "action" "post-facebook-like")
                                                    (js-pair "row_id" row-id))) 
                                  ;;(:img :src "/appimg/fb-like.png")
                                  (str "Like")
                                  ))
                       (:span :class "action-icon" (str (if (gpv col-val :likes :count)
                                                            (gpv col-val :likes :count)
                                                            0)))
                       (:span :class "action-icon" :title "Comment"
                              (:a :href "#" 
                                  :onclick (format nil "$(\"#comments-~A\").toggle(); return false;"
                                                   (gpv col-val :id))
                                  ;;(:img :src "/appimg/fb-comment.png")
                                  (str "comment")))
                       (:span :class "action-icon" (str (gpv col-val :comments :count)))
                       ;;(:span :class "action-icon" :title "Share"
                       ;;       (:a :href "#" (:img :src "/appimg/fb-share.png")))
                       )))


       (:div :id (format nil "comments-~A" (gpv col-val :id)) 
             :style "background-color:#F2F2F2;display:none;"
             (:br)
                           
             (dolist (comment (gpv col-val :comments :data))
               (htm (:div :style "border-bottom:thin solid #ffffff;" 
                          (str (gpv comment :message)))))
             (:br)

             (htm (:a :href
                        (js-link 
                         (js-render (editor grid)
                                    (js-pair "grid-name" (name grid))
                                    (js-pair "action" "facebook-comment")
                                    (js-pair "row_id" row-id)))
                        
                        (str "Post Comment"))))))

     )))

(defun linkedin-post-display (grid row col-val row-id)
;;(break "~A~%~A" col-val (gpv col-val :timestamp))
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
                      (cond ((string-equal (gpv col-val :update-type) "SHAR")
                             (gpv col-val :update-content :person :id))
                            ((string-equal (gpv col-val :update-type) "CMPY")
                             (gpv col-val :update-content :company :id))))
        (str (cond ((string-equal (gpv col-val :update-type) "SHAR")
                    (gpv col-val :update-content :person :first-name))
                   ((string-equal (gpv col-val :update-type) "CMPY")
                    (gpv col-val :update-content :company :name)))))
       (:span :class "twitter-user" 
              (:a :href (format nil "http://www.linkedin.com/~A" 
                                (cond ((string-equal (gpv col-val :update-type) "SHAR")
                                       (gpv col-val :update-content :person :id))
                                      ((string-equal (gpv col-val :update-type) "CMPY")
                                       (gpv col-val :update-content :company :id))))))
      ;; (:span :class "timestamp"
      ;;        (str (format-universal-date-time (unix-time-to-universal (truncate (/ (gpv col-val :timestamp) 1000))))))
       (:span :class "post-content"
              (str (cond ((string-equal (gpv col-val :update-type) "SHAR")
                          (or (gpv col-val :update-content :person :current-share :content :description)
                              (gpv col-val :update-content :person :current-share :comment)))
                         ((string-equal (gpv col-val :update-type) "CMPY")
                          (gpv col-val :update-content :company-status-update :share :comment)))))
       
       (:span :class "twitter-actions"
              (if (gpv col-val :is-likeable)
                  (htm (:span :class "action-icon" :title "Like"
                              (:a :href (js-link 
                                         (js-render (editor grid)
                                                    (js-pair "grid-name" (name grid))
                                                    (js-pair "action" "like-linkedin-form")
                                                    (js-pair "row_id" row-id))) 
                                  ;;(:img :src "/appimg/fb-like.png")
                                  (str "Like")
                                  ))))
                                       
              (if (gpv col-val :is-commentable)
                  (htm (:span :class "action-icon" :title "Comment"
                              (:a :href (js-link 
                                         (js-render (editor grid)
                                                    (js-pair "grid-name" (name grid))
                                                    (js-pair "action" "comment-linkedin-form")
                                                    (js-pair "row_id" row-id))) 
                                  ;;(:img :src "/appimg/fb-like.png")
                                  (str "Comment")))))))))))


(defun generic-grid-item-display (grid row payload row-id)
  (if payload
      (cond ((string-equal (get-val row 'post-type) "Twitter")
             (twitter-post-display grid row payload row-id))
            ((string-equal (get-val row 'post-type) "Social Mention")
             (social-mention-display grid row payload row-id))
            ((string-equal (get-val row 'post-type) "LinkedIn")
             (linkedin-post-display grid row payload row-id))
            ((string-equal (get-val row 'post-type) "Facebook")
             (facebook-post-display grid row payload row-id)))))

(defun display-image (payload)
  (cond ((string-equal "t" "") 
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
                                                              
                          (:img :src (gpv payload :picture)))))
               ))
         ))

  )

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
            :width "60%"
            :special-printer #'generic-grid-item-display)
           (make-instance 
            'grid-column
            :name 'created-date
            :header "Created"
            
            :printer #'format-universal-date-time)
           (make-instance 'grid-column
                          :name 'payload
                          :header "Image"
                          :printer #'display-image)))

           
           (grid (make-widget 'generic-grid :name "generic-post-gridxx"
                              ;;:columns columns
                              :edit-inline nil
                              :title "Inbox"
                              :row-object-class 'generic-post)))
    (setf (get-val grid 'columns) columns)
    (setf (get-val grid 'grid-links) 
          (list (list 'twitter "delete" "Delete")
                (list 'twitter "unfollow" "Unfollow")
                (list 'twitter "direct-message" "Direct Message")
                (list 'twitter "assign-task-form" "Assign Task")
                (list 'twitter "highlight" "Highlight")

                (list 'facebook "delete" "Delete")
                (list 'facebook "block-user" "Block User")
                (list 'facebook "direct-messaeg" "Direct Message")
                (list 'facebook "assign-task-form" "Assign Task")
                (list 'facebook "highlight" "Highlight")

                (list 'linkedin "delete" "Delete")
                (list 'linkedin "assign-task-form" "Assign Task")
                (list 'linkedin "highlight" "Highlight")

                (list 'social-mention "delete" "Delete")
                (list 'social-mention "assign-task-form" "Assign Task")))
    (setf (sort-keys grid) '(3 created-date))
    (setf (initial-sort-column grid) '(3 :descending))
    
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




