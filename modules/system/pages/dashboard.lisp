(in-package :ems)

(defclass dashboard-item (widget)
  (
   ))

(defmethod render ((widget dashboard-item) &key name header items)
  (with-html-to-string ()
    (let ((box (make-widget 'peach-box :name (format nil "~A-box" name))))
      (setf (header box) header)
      (setf (get-val box 'content )
            (with-html-to-string ()
              (:ul :class "stats-list"
                   (dolist (item items)
                     (htm (:li (:a :href "#" (str (first item)) 
                                   (:span (str (second item))))))))))
      (str (render box
                   ;;need to move this
                   :actions
                   (with-html-to-string ()
                     (:div :class "actions-left")
                     (:div :class "actions-right"
                                 (:a :class "button" :href "#" "Got to stats &raquo;")))))
      (htm (:div :class "clear"))
      )))


(defun get-facebook-user (fb-user-id)
  (find-doc (service-users)
            :test (lambda (doc)
                    (if (string-equal (get-val doc 'user-id) fb-user-id)
                        (return-from get-facebook-user doc)))))

(defun get-fb-post-from (generic-entry)
  (get-val (get-val (get-val generic-entry 'payload) 'from) 'id))

(defun get-fb-post-to (generic-entry)
  (if (get-val (get-val generic-entry 'payload) 'to)
      (get-val (get-val generic-entry 'payload) 'to)))

(defun get-fb-likes-count (generic-entry)
  (if (get-val (get-val generic-entry 'payload) 'likes)
      (if (get-val (get-val (get-val generic-entry 'payload) 'likes) 'count)
          (get-val (get-val (get-val generic-entry 'payload) 'likes) 'count)
          0)
      0))

(defun get-fb-comments-count (generic-entry)
  
  (if (get-val (get-val generic-entry 'payload) 'comments)
      (if (get-val (get-val (get-val generic-entry 'payload) 'comments) 'count)
          (get-val (get-val (get-val generic-entry 'payload) 'comments) 'count)
          0)
      0))

(defun fb-count-posts-from ()
  (let ((count 0)
        (likes 0)
        (comments 0))
    (loop 
       for doc across (generic-entries) 
        do (if (not (string-equal 
                    (get-val doc
                             'doc-status) "superseded"))
              (loop 
                 for user across (service-users)
                    do (when (match-context-entities user)                      
                         (when (string-equal (format nil "~A" (get-val user 'user-id)) (get-fb-post-from doc))
                                (incf count)
                                (incf likes (get-fb-likes-count doc)) 
                                (incf comments (get-fb-comments-count doc)))
                          (when (string-equal (format nil "~A" (get-val user 'user-id)) (get-fb-post-from doc))
                                (incf count)
                                (incf likes (get-fb-likes-count doc)) 
                                (incf comments (get-fb-comments-count doc)))))))
    (values count likes comments)))

(defun fb-count-posts-to ()
  (let ((count 0)
        (likes 0)
        (comments 0))
    (loop 
       for doc across (generic-entry-collection) 
        do (if (not (string-equal 
                    (get-val doc
                             'doc-status) "superseded"))
              (loop 
                 for user across (service-users)
                    do (if (match-context-entities user)
                          (let ((to-user-id (if (get-val (get-val doc 'payload) 'to)
                                                (if (listp (get-val (get-val doc 'payload) 'to))
                                                    (get-val (first (get-val (get-val doc 'payload) 'to)) 'id)
                                                    (get-val (get-val (get-val doc 'payload) 'to) 'id)))))                                    
                            (when (not (string-equal (get-val user 'user-id) to-user-id))
                                (incf count)
                                (if (get-val (get-val doc 'payload) 'likes)
                                    (incf likes (get-val (get-val (get-val doc 'payload) 'likes) 'count)))
                                (if (get-val (get-val doc 'payload) 'comments)
                                    (incf comments (get-val (get-val (get-val doc 'payload) 'comments) 'count)))))))))
    (values count likes comments)))

(define-easy-handler (dashboard-page :uri "/ems/dashboard") ()
  
  (let ((page (make-widget 'page :name "dashboard-page")))
    (with-html
      (render page
              :body 
              (with-html-to-string ()
                (multiple-value-bind (posts likes comments)
                    (fb-count-posts-from)
                    (let ((dash-item (make-widget 'dashboard-item :name "dash-item"))) 
                      (str (render dash-item :name "analisys" :header "Summary"
                                   :items
                                   (list
                                    (list "Posts" posts)
                                    (list "Likes" likes)
                                    (list "Comments" comments))))

                      (str (render dash-item :name "analisys" :header "Analysis"
                                   :items
                                   (list
                                    (list "Brand Awareness" "0")
                                    (list "Customer Services" "0"))))

                      (str (render dash-item :name "engagement" :header "Engagement"
                                   :items
                                   (list
                                    (list "Likes" likes)
                                    (list "+1's" "0")
                                    (list "Shares" "0")
                                    (list "Retweets" "0"))))

                      (str (render dash-item :name "reach" :header "Reach"
                                   :items
                                   (list
                                    (list "Followers" "0")
                                    (list "Links" "0")
                                    (list "Twitter @ Replies" "0"))))

                      (str (render dash-item :name "ave-engagement" :header "Ave Engagement By Publication"
                                   ;;TODO: Graph
                                   ))

                      (str (render dash-item :name "top-10-content" :header "Top 10 Content"
                                   :items
                                   (list
                                    (list "Facebook Posts by Piet Snot" posts)
                                    (list "WP Post by Piet Snot" "0"))))

                      (str (render dash-item :name "top-10-mentions" :header "Top 10 Mentions"
                                   :items
                                   (list
                                    (list "@pietsnot" "0")
                                    (list "@sannie koekemoer" "0"))))
                      (str (render dash-item :name "top-10-users" :header "Top 10 Users"
                                   :items
                                   (list
                                    (list "Piet Snot" "0")
                                    (list "Gert Gieter" "0"))))
                      (str (render dash-item :name "published-with-links" :header "Activities Published with Links"
                                   :items
                                   (list
                                    (list "Links in published material" "0")
                                    (list "Links to home WWW in published material" "0"))))
                  
                      ))))
      )))