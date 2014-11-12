(in-package :dyb)

(defun social-mention-display (grid row col-val row-id)
 ; (break "~A" col-val)
  (with-html-string
    (:div :class "nonboxy-widget"
          ;; (:div :class "widget-head")
          (:div :class "widget-content" :style "background:white;"

                (:div :class "post"

                      (:a
                       :class "post-title"
                       :target "_blank"
                       :href (or (gpv col-val :link)
                                 (gpv col-val :user--link))
                       ;;TODO:Check why to long links with no spaces pushes display over box edges
                       (str (if (> (length (gpv col-val :title)) 100)
                                (subseq (gpv col-val :title) 0 100)
                                (gpv col-val :title))))
                      (if (gpv col-val :user)
                          (htm
                           (:span :class "twitter-user"
                                  (:a :href (gpv col-val :user--link)
                                      (gpv col-val :user)))))
                      (:span :class "post-content"
                             (str (linkify (if (not (empty-p (gpv col-val :description)))
                                               (gpv col-val :description)
                                               (gpv col-val :title)))))
                      (:span :class "twitter-actions"
                             (:span :class "action-icon" :title "Go to Content"
                                    (:a  :target "_blank"
                                         :href  (gpv col-val :link)

                                        (:img :src "/appimg/go-to-content.png"))))

                      )))))

(defun twitter-post-display (grid row col-val row-id)
;;  (if (search "t.co" (gpv col-val :text)  )
;;      (break "~A" col-val))
 (with-html-string
    (:div
     :class "nonboxy-widget"
     (:div
      :class "widget-content" :style "background:white;"
      (:div
       :class "post"
       (:a
        :class "post-title"
        :target "_blank"
        :href (format nil "http://www.twitter.com/~A/statuses/~A"
                      (gpv col-val :user :screen--name)
                      (gpv col-val :id--str))
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
                         "Reply"))

              (:span :class "action-icon" :title "Retweet"
                     (:a :href (js-link
                                (js-render (editor grid)
                                           (js-pair "grid-name" (name grid))
                                           (js-pair "action" "retweet-twitter-form")
                                           (js-pair "row_id" row-id)))
                         ;;(:img :src "/appimg/twitter-retweet.png")
                         "Retweet"))

              (:span :class "action-icon" :title "Add to favourites"
                     (:a :href (js-link
                                (js-render (editor grid)
                                           (js-pair "grid-name" (name grid))
                                           (js-pair "action" "favourite-twitter")
                                           (js-pair "row_id" row-id)))
                                  ;;(:img :src "/appimg/twitter-favourite.png")
                         "Add to Favourites"))))))))

(defun facebook-post-display (grid row col-val row-id)
  (with-html-string
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
              (str
               (cond ((or (gpv col-val :message) (gpv col-val :story))
                      (linkify (or (gpv col-val :message) (gpv col-val :story))))
                     ((or (string-equal (gpv col-val :type) "photo")
                          (string-equal (gpv col-val :status--type) "added_photos"))
                      (gpv col-val :name))
                     (t
                      "MISSING TEXT report to info@dyb.co.za"))))
       (when (gpv col-val :actions)
         (htm (:span :class "twitter-actions"
                     (:span :class "action-icon" :title "Like"
                            (:a :href (js-link
                                       (js-render (editor grid)
                                                  (js-pair "grid-name" (name grid))
                                                  (js-pair "action" "post-facebook-like")
                                                  (js-pair "row_id" row-id)))
                                ;;(:img :src "/appimg/fb-like.png")
                                "Like"
                                ))
                     (:span :class "action-icon"
                            (str
                             (if (gpv
                                  (gethash :post-likes
                                           (get-val row 'post-data))
                                  :summary :total--count)
                                 (gpv
                                  (gethash :post-likes
                                           (get-val row 'post-data))
                                  :summary :total--count)
                                 0
                                 )))
                     (:span :class "action-icon" :title "Comment"
                            (:a :href "#"
                                :onclick (format nil "$(\"#comments-~A\").toggle(); return false;"
                                                 (gpv col-val :id))
                                ;;(:img :src "/appimg/fb-comment.png")
                                "Comments"))
                     (:span :class "action-icon" (str
                                                  (if (gpv
                                                       (gethash
                                                        :post-comments
                                                        (get-val row 'post-data))
                                                       :summary :total--count)
                                                      (gpv
                                                       (gethash
                                                        :post-comments
                                                        (get-val row 'post-data))
                                                       :summary :total--count)
                                                      0
                                                      )))
                     ;;(:span :class "action-icon" :title "Share"
                     ;;       (:a :href "#" (:img :src "/appimg/fb-share.png")))
                     )))

       (:br)
       (:div :id (format nil "comments-~A" (gpv col-val :id))
             :style "background-color:#F2F2F2;display:none;"

             (:span :style "font-weight:bold;" "Latest Comments")


             (:br)
             ;;(when (> (hash-table-count (get-val row 'post-data) ) 0) (break "~A" row))
             (dolist (comment (gpv (gethash :post-comments (get-val row 'post-data)) :data))
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
                        (:br)
                        (:a :href
                            (js-link
                             (js-render (editor grid)
                                        (js-pair "grid-name" (name grid))
                                        (js-pair "comment-id" (gpv comment :id))
                                        (js-pair "action" "facebook-comment-reply")
                                        (js-pair "row_id" row-id)))

                            "Reply")
                        (:a :href (js-link
                                   (js-render (editor grid)
                                              (js-pair "grid-name" (name grid))
                                              (js-pair "comment-id" (gpv comment :id))
                                              (js-pair "action" "post-facebook-comment-like")
                                              (js-pair "row_id" row-id)))
                            ;;(:img :src "/appimg/fb-like.png")
                            "Like"
                            )
                        (:span :class "action-icon"
                              (str
                               (or (gpv comment :like-count)
                                   0)))))

                  (:td (str  (gpv comment :message)))))))
             (:br)
             (htm (:a :href
                        (js-link
                         (js-render (editor grid)
                                    (js-pair "grid-name" (name grid))
                                    (js-pair "action" "facebook-comment")
                                    (js-pair "row_id" row-id)))
                      "Add Comment"))))))))

(defun linkedin-post-display (grid row col-val row-id)
;;(break "~A~%~A" col-val (gpv col-val :timestamp))
  (with-html-string
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
      (cond  ((string-equal (get-val row 'post-type) "Twitter")
             (twitter-post-display grid row payload row-id))
            ((string-equal (get-val row 'post-type) "Social-Mention")
             (social-mention-display grid row payload row-id))
            ((string-equal (get-val row 'post-type) "LinkedIn")
             (linkedin-post-display grid row payload row-id))
            ((string-equal (get-val row 'post-type) "Facebook")
             (facebook-post-display grid row payload row-id)))))

(defun display-image (payload)
  (cond ((string-equal "t" "")
         (with-html-string
           (:div :class "post-image-thumb"
                 (:a :href "#" :title "Click to view the full size image"
                     ;;(:img :src "")
                     ))
           ))
        ((gpv payload :favicon)
         (with-html-string
           (if (gpv payload :image)
               (htm
                (:div :class "post-image-thumb"
                      (:a :href (gpv payload :image)
                          :title "Click to view the full size image"
                          (:img :src (gpv payload :image))
                          ))))
           ))
        ((gpv payload :update-type)
         (with-html-string
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
         (with-html-string
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
             (with-html-string
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
             (with-html-string
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
             (with-html-string
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
             (with-html-string
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
                     (with-html-string
                       (:span :class "post-source"
                              (:img :src "/appimg/user-thumb.png"))
                       ))))


(defun format-universal-date-time-special (date)
  (format-universal-date-time (+ date (* 60 60 2))))

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
                     (with-html-string
                       (:span :class "post-source"
                              (:img :src "/appimg/twitter-bird-white-on-blue.png"))))
                    ((equal source 'facebook)
                     (with-html-string
                       (:span :class "post-source"
                              (:img :src "/appimg/Facebook_Light_Logo.png"))))
                    ((equal source 'social-mention)
                     (with-html-string
                       (:span :class "post-source"
                              (:img :src "/appimg/social-mention.png"))))
                    ((equal source 'linkedin)
                     (with-html-string
                       (:span :class "post-source"
                              (:img :src "/appimg/linkedin-icon.png")))))))
           (make-instance
            'grid-column
            :name 'payload
            :header "Pic"
            :special-printer #'get-profile-pic)

           (make-instance
            'grid-column
            :name 'payload
            :header "Post"
            :width "60%"
            :special-printer #'generic-grid-item-display)
           (make-instance
            'grid-column
            :name 'created-date
            :header "Posted"
            :printer #'format-universal-date-time-special)
           (make-instance 'grid-column
                          :name 'payload
                          :header "Image"
                          :printer #'display-image)))


           (grid (make-widget 'generic-grid :name "generic-post-gridxx"
                              ;;:columns columns
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
    (setf (not-sorting-columns grid) '(0 1 2 4))
    (setf (sort-keys grid) '(3 created-date))
    (setf (initial-sort-column grid) '(3 :descending))

    (render (make-widget 'page :name "generic-page")
            :body (render-to-string grid))))


(define-easy-handler (scheduled-page :uri "/dyb/scheduled") ()
  (let* ((columns
           (list
            (make-instance 'grid-column
                           :name 'pid
                           :header "Post Id")
            (make-instance 'grid-column
                           :name 'action
                           :header "Action")
            (make-instance 'grid-column
                           :name 'scheduled-date
                           :header "Scheduled Date")))
         (action-grid (make-widget 'generic-actions-grid
                                   :name "generic-actions-grid"
                                   :columns columns
                                   :title "Actions"
                                   :row-object-class 'generic-action)))

    (render (make-widget 'page :name "scheduled-page")
            :body (render-to-string action-grid))))
