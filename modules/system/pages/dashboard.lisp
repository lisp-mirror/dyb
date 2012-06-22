(in-package :ems)

(defclass dashboard-item (widget)
  (
   ))

(defmethod render ((widget dashboard-item) &key name header items)
  (with-html-output-to-string (*standard-output*)
    (let ((box (make-widget 'peach-box :name (format nil "~A-box" name))))
      (setf (header box) header)
      (str (render box
                   :content
                   (with-html-output-to-string (*standard-output*)

                     (:ul :class "stats-list"
                          (dolist (item items)
                            (htm (:li (:a :href "#" (str (first item)) 
                                          (:span (str (second item)))))))))
                   :actions
                   (with-html-output-to-string (*standard-output*)
                     (:div :class "actions-left")
                           (:div :class "actions-right"
                                 (:a :class "button" :href "#" "Got to stats &raquo;")))))
      )))

(define-easy-handler (dashboard-page :uri "/ems/dashboard") ()
  
  (let ((page (make-widget 'ems-page :name "dashboard-page")))
    (with-html-output(*standard-output*)
      (render page
              :body 
              (with-html-output-to-string (*standard-output*)

                (let ((dash-item (make-widget 'dashboard-item :name "dash-item")))

                  (str (render dash-item :name "analisys" :header "Analysis"
                               :items
                               (list
                                (list "Brand Awareness" "10")
                                (list "Customer Services" "10"))))

                  (str (render dash-item :name "engagement" :header "Engagement"
                               :items
                               (list
                                (list "Likes" "10")
                                (list "+1's" "10")
                                (list "Shares" "10")
                                (list "Retweets" "10"))))
                  (str (render dash-item :name "reach" :header "Reach"
                               :items
                               (list
                                (list "Followers" "10")
                                (list "Links" "10")
                                (list "Twitter @ Replies" "10"))))
                  (str (render dash-item :name "ave-engagement" :header "Ave Engagement By Publication"
                               ;;TODO: Graph
                               ))
                  (str (render dash-item :name "top-10-content" :header "Top 10 Content"
                               :items
                               (list
                                (list "Twitter Posts by Piet Snot" "10")
                                (list "WP Post by Piet Snot" "10"))))
                  (str (render dash-item :name "top-10-mentions" :header "Top 10 Mentions"
                               :items
                               (list
                                (list "@pietsnot" "10")
                                (list "@sannie koekemoer" "10"))))
                  (str (render dash-item :name "top-10-users" :header "Top 10 Users"
                               :items
                               (list
                                (list "Piet Snot" "10")
                                (list "Gert Gieter" "10"))))
                  (str (render dash-item :name "published-with-links" :header "Activities Published with Links"
                               :items
                               (list
                                (list "Links in published material" "10")
                                (list "Links to home WWW in published material" "10"))))
                  
                  )))
      )))

