(in-package :dyb)


(defun social-mention-display (grid row col-val row-id)
 ; (break "~A" col-val)
  (with-html-to-string ()
    (:div :class "nonboxy-widget"
          ;; (:div :class "widget-head")
          (:div :class "widget-content" :style "background:white;"
                
                (:div :class "post"

                      (:a
                       :class "post-title"
                       :target "_blank"
                       :href (or (gpv col-val :link)
                                 (gpv col-val :user--link))
                       (str (gpv col-val :title)))
                      (if (gpv col-val :user)
                          (htm
                           (:span :class "twitter-user" 
                                  (:a :href (gpv col-val :user--link) 
                                      (gpv col-val :user)))))
                      (:span :class "post-content"
                             (str (linkify (gpv col-val :description))))
                      (:span :class "twitter-actions"
                             (:span :class "action-icon" :title "Go to Content"
                                    (:a :href (gpv col-val :link) 
                                        (:img :src "/appimg/go-to-content.png"))))

                      )))))

(defun twitter-post-display (grid row col-val row-id)
;(break "~A~%~A" col-val (gpv col-val :entities :urls :expanded--url)) 
 (with-html-to-string ()
    (:div 
     :class "nonboxy-widget"
     (:div 
      :class "widget-content" :style "background:white;"
      (:div 
       :class "post"
       (:a
        :class "post-title"        
        :target "_blank"
        :href (format nil "http://www.twitter.com/~A" 
                      (gpv col-val :entities :urls :expanded--url))
        (str (gpv col-val :user :name)))
       (:span :class "twitter-user" 
              (:a :href (format nil "http://www.twitter.com/~A" 
                                (gpv col-val :user :screen--name))))
       (:span :class "post-content"
              (str (linkify (gpv col-val :text))))
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
        :target "_blank"
        :href (format nil "http://www.facebook.com/~A" (gpv col-val :id))
        (str (gpv col-val :from :name)))     
       (:span :class "post-content"
              (if (not (or (gpv col-val :message) (gpv col-val :story)))
                  (if (and (string-equal (gpv col-val :type) "photo")
                           (string-equal (gpv col-val :status--type) "added_photos"))
                      (htm  (str (gpv col-val :name )))
                      (htm (str "MISSING TEXT report to info@dyb.co.za")))
                  (htm (str (linkify (or (gpv col-val :message) (gpv col-val :story)))))))
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

       (:br)
       (:div :id (format nil "comments-~A" (gpv col-val :id)) 
             :style "background-color:#F2F2F2;display:none;"
             
             (:span :style "font-weight:bold;" (str "Latest Comments"))
             
             
             (:br)
             
             (dolist (comment (gpv col-val :comments :data))
               (htm 

                (:table :style "border:none,border-collapse: collapse;"
                 (:tr
                  (:td :style "border:none,border-collapse: collapse;" :width "20%" 
                       (:a :target "_blank"
                        :href (format nil "http://facebook.com/~A"
                                      (gpv comment :from :id)) 
                        (str (gpv comment :from :name))
                        (:img :src 
                                     (if (gpv comment :from :id)
                                         (format nil 
                                                 "https://graph.facebook.com/~A/picture" 
                                                 (gpv comment :from :id))
                                         "/appimg/user-thumb.png"))
                        ))
                        
                  (:td (str  (gpv comment :message)))))
                
                ))
             (:br)

             (htm (:a :href
                        (js-link 
                         (js-render (editor grid)
                                    (js-pair "grid-name" (name grid))
                                    (js-pair "action" "facebook-comment")
                                    (js-pair "row_id" row-id)))
                        
                        (str "Add Comment"))))))

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
        :target "_blank"
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

       (:span :class "post-content"
              (str (cond ((string-equal (gpv col-val :update-type) "SHAR")
                          (linkify 
                           (or (gpv col-val :update-content :person 
                                    :current-share :content :description)
                               (gpv col-val :update-content :person 
                                    :current-share :comment))))
                         ((string-equal (gpv col-val :update-type) "CMPY")
                          (linkify (gpv col-val :update-content :company-status-update 
                                        :share :comment)))))))))))


(defun generic-grid-item-display (grid row payload row-id)
  (if payload
      (cond ((string-equal (get-val row 'post-type) "Twitter")
             (twitter-post-display grid row payload row-id))
            ((string-equal (get-val row 'post-type) "Social-Mention")
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
                      (:a :href (gpv payload :image) 
                          :title "Click to view the full size image"
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


(defun get-profile-pic (grid row payload row-id)
  (cond ((string-equal (get-val row 'post-type) "Twitter")
             (with-html-to-string ()
                       (:span :class "post-source" 
                              (:a
                               :class "post-title"        
                               :target "_blank"
                               :href (format nil "http://www.twitter.com/~A" 
                                             (gpv payload :user :screen--name))
        
                               (:img :src (if (gpv payload :user :profile--image--url--https)
                                              (gpv payload :user :profile--image--url--https)
                                              "/appimg/user-thumb.png"))))
                       ))
           ; ((string-equal (get-val row 'post-type) "Social Mention")
           ;  (social-mention-display grid row payload row-id))
            ((string-equal (get-val row 'post-type) "LinkedIn")
             (with-html-to-string ()
                       (:span :class "user-thumb" 
                              (:a
                               :class "post-title"
                               :target "_blank"
                               :href (format nil "http://www.linkedin.com/~A" 
                                             (cond ((string-equal 
                                                     (gpv payload :update-type) "SHAR")
                                                    (gpv payload :update-content 
                                                         :person :id))
                                                   ((string-equal 
                                                     (gpv payload :update-type) "CMPY")
                                                    (gpv payload :update-content 
                                                         :company :id))))
        
                               (:img :src (if (gpv payload :update-content 
                                                   :person :picture-url)
                                              (gpv payload :update-content 
                                                   :person :picture-url)
                                              "/appimg/user-thumb.png"))))
                       ))
            ((string-equal (get-val row 'post-type) "Facebook")
             (with-html-to-string ()
                       (:span :class "post-source" 
                              (:a
                               :class "post-title"
                               :target "_blank"
                               :href (format nil "http://www.facebook.com/~A" 
                                             (gpv payload :from :id))
                               (:img :src 
                                     (if (gpv payload :from :id)
                                         (format nil 
                                                 "https://graph.facebook.com/~A/picture" 
                                                 (gpv payload :from :id))
                                         "/appimg/user-thumb.png"))))
                       ))
            ((string-equal (get-val row 'post-type) "Social-Mention")
             (with-html-to-string ()
                       (:span :class "user-thumb" 
                              (:a
                               :class "post-title"
                               :target "_blank"
                               :href (gpv payload :user--link)
                               (:img :src (if (gpv payload :user--image)
                                              (gpv payload :user--image)
                                              "/appimg/user-thumb.png"))))
                       ))
            (t
             (break "~A" row)
                     (with-html-to-string ()
                       (:span :class "post-source" 
                              (:img :src "/appimg/user-thumb.png"))
                       ))))

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
            :special-printer #'get-profile-pic 
            

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
             ;   (list 'twitter "unfollow" "Unfollow")
                (list 'twitter "direct-message-form" "Direct Message")
                (list 'twitter "assign-task-form" "Assign Task")
               ; (list 'twitter "highlight" "Highlight")
               ; (list 'twitter "go-to-content" "Go To Content")

                (list 'facebook "delete" "Delete")
               ; (list 'facebook "block-user" "Block User")
                (list 'facebook "direct-message-form" "Direct Message")
                (list 'facebook "assign-task-form" "Assign Task")
              ;  (list 'facebook "highlight" "Highlight")
              ;  (list 'facebook "go-to-content" "Go To Content")

                (list 'linkedin "delete" "Delete")
               ;; (list 'linkedin "direct-message-form" "Direct Message")
                (list 'linkedin "assign-task-form" "Assign Task")
                
               ; (list 'linkedin "highlight" "Highlight")
               ; (list 'linkedin "go-to-content" "Go To Content")

                (list 'social-mention "delete" "Delete")
                (list 'social-mention "assign-task-form" "Assign Task")
                ;(list 'social-mention "go-to-content" "Go To Content")
                ))
    (setf (not-sorting-columns grid) '(0 1 2 4 ))
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




