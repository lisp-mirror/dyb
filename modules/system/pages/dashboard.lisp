(in-package :ems)

(defclass dashboard-item-full (widget)
  ())

(defmethod render ((widget dashboard-item-full) &key header content)
  (with-html-to-string ()
    (:div :class "widget-block" 
          (:div :class "widget-head" 
                (:h5 (esc header)))
          (:div :class "widget-content"
                (:div :style "width:95%")
                (str content)))))

(defclass dashboard-item (widget)
  ())

(defmethod render ((widget dashboard-item) &key header content)
  (with-html-to-string ()
    (:div :class "widget-block" 
          (:div :class "widget-head" 
                (:h5 (esc header)))
          (:div :class "widget-content"
                (:table (:tr (:td :style "vertical-align:top;"
                                  (:img  :style "padding :10px;" :src "/appimg/save.png"))
                             (:td 
                              (:div :style "padding :10px"
                                    (str content)))))))))


(defun interval-days (days)
  (* days 24 60 60))

(defun within-date-range (interval date)
  (and (>
        date
        (- (get-universal-time) (interval-days interval) ))
       (<
        date
        (+ (get-universal-time) (interval-days interval) ))))


(defun posts-scheduled (interval)
  (find-docs 'vector 
             (lambda (doc)
               (if (match-entities doc (context))
                   (within-date-range interval (get-val doc 'scheduled-date))))
             (generic-actions-collection)))


(defun fb-comment-dates-count (interval payload)
  (let ((count 0))
    
    (dolist (comment  (gpv payload :comments :data))
      (when (within-date-range 
             interval 
             (parse-facebook-created-at (gpv comment :created--time)))
        (incf count)))
    count))

(defun fb-comments-made (interval)
  (let ((count 0))
    (loop for post across (generic-posts) 
       ;;when (match-entities post (context))
       do (incf count (fb-comment-dates-count 
                       interval 
                       (get-val post 'payload))))
    count))

(defun fb-likes-dates-count (interval payload)
  (if (gpv payload :likes :count)
      (gpv payload :likes :count)
      0))

(defun fb-likes-made (interval)
  (let ((count 0))
    (loop for post across (generic-posts) 
       ;;when (match-entities post (context))
         when (equal (post-type post) 'facebook)
       do (incf count (fb-likes-dates-count 
                       interval 
                       (get-val post 'payload))))
    count))


(defun twitter-retweets-dates-count (interval payload)
  (if (gpv payload :retweet--count)
      (gpv payload :retweet--count)
      0))

(defun twitter-retweets (interval)
  (let ((count 0))
    (loop for post across (generic-posts) 
       ;;when (match-entities post (context))
       when (equal (post-type post) 'twitter)
       do (incf count (twitter-retweets-dates-count 
                       interval 
                       (get-val post 'payload))))
    count))

(define-easy-handler (dashboard-page :uri "/ems/dashboard") ()
  
  (let ((page (make-widget 'page :name "dashboard-page")))
    (with-html
      (render page
              :body 
              (with-html-to-string ()

                (:div :class "dashboard-widget"
                      (:div :class "row-fluid"
                            (:div :class "span2"
                                  (:div :class "dashboard-wid-wrap"
                                        (:div :class "dashboard-wid-content"
                                              (:a :href "/ems/generic"
                                                     (:i :class "dashboard-icons mail_blk")
                                                     (:span :class "dasboard-icon-title"
                                                            "Inbox")))))
                            (:div :class "span2"
                                  (:div :class "dashboard-wid-wrap"
                                        (:div :class "dashboard-wid-content"
                                              (:a :href "/ems/generic-scheduler"
                                                     (:i :class "dashboard-icons month_calendar_blk")
                                                     (:span :class "dasboard-icon-title"
                                                            "Scheduler")))))
                            (:div :class "span2"
                                  (:div :class "dashboard-wid-wrap"
                                        (:div :class "dashboard-wid-content"
                                              (:a :href "/ems/search-stream"
                                                     (:i :class "dashboard-icons magnifying_glass_blk")
                                                     (:span :class "dasboard-icon-title"
                                                            "Search Streams")))))
                            (:div :class "span2"
                                  (:div :class "dashboard-wid-wrap"
                                        (:div :class "dashboard-wid-content"
                                              (:a :href "/ems/search-stream"
                                                     (:i :class "dashboard-icons graph_blk")
                                                     (:span :class "dasboard-icon-title"
                                                            "Reporting (Coming Soon)")))))
                            (:div :class "span2"
                                  (:div :class "dashboard-wid-wrap"
                                        (:div :class "dashboard-wid-content"
                                              (:a :href "/ems/service-users"
                                                     (:i :class "dashboard-icons cog_2_blk")
                                                     (:span :class "dasboard-icon-title"
                                                            "Settings")))))
                            (:div :class "span2"
                                  (:div :class "dashboard-wid-wrap"
                                        (:div :class "dashboard-wid-content"
                                              (:a :href "#"
                                                     (:i :class "dashboard-icons help_blk")
                                                     (:span :class "dasboard-icon-title"
                                                            "Help"))))))

                      (let ((dash-item (make-widget 'dashboard-item :name "dash-item"))
                            (dash-item-full (make-widget 'dashboard-item-full 
                                                         :name "dash-item-full")))
                        (htm 
                         (str (render dash-item-full :name "ave-engagement" 
                                      :header "Current Network Size"
                                      :content (render-to-string (make-widget 'line-graph :name "chart6"))))
                         (str (render dash-item-full :name "report-summary" 
                                      :header "Report Summary"
                                 
                                      :content (with-html-to-string ()
                                                 (:div 
                                                  (:p "Community Growth")
                                                  (:p "Activity" 
                                                      (str (length (posts-scheduled 7))))
                                                  (:p "Engagement" 
                                                      (str (+ (fb-comments-made 7)
                                                              (fb-likes-made 7))))
                                                  (:p "Impressions"
                                                      (str (twitter-retweets 7)))
                                                  (:p "Clicks"
                                                      "0")))
                                      ))
                         (str (render dash-item-full :name "audience-demographics" 
                                      :header "Audience Demographics"
                                 
                                      :content "Eish"
                                      ))
                         )
                        )
                      (:div :class "row-fluid"
                            (:div :class "span2"
                                  (:div :class "stat-block"
                                        (:ul
                                         (:li
                                          (:i :class "dashboard-icons-colors current_work_sl"))
                                         (:li :class "stat-count"
                                              (:span "Total Reach")
                                              (:span 974)
                                              (:br)
                                              (:br))
                                         (:li :class "stat-percent"
                                              (:span (:img :scr "/appimg/green-arrow.png"
                                                           :height "20"
                                                           :width "20"
                                                           :alt "Increased"))
                                              (:span :class "label-green"
                                                     "12%")))))
                            (:div :class "span2"
                                  (:div :class "stat-block"
                                        (:ul
                                         (:li
                                          (:i :class "dashboard-icons-colors current_work_sl"))
                                         (:li :class "stat-count"
                                              (:span "Current Network")
                                              (:span 373)
                                              (:br))
                                         (:li :class "stat-percent"
                                              (:span (:img :scr "/appimg/green-arrow.png"
                                                           :height "20"
                                                           :width "20"
                                                           :alt "Increased"))
                                              (:span :class "label-green"
                                                     "5%")))))
                            (:div :class "span2"
                                  (:div :class "stat-block"
                                        (:ul
                                         (:li
                                          (:i :class "dashboard-icons-colors current_work_sl"))
                                         (:li :class "stat-count"
                                              (:span "Total Impressions")
                                              (:span 3873)
                                              (:br))
                                         (:li :class "stat-percent"
                                              (:span (:img :scr "/appimg/green-arrow.png"
                                                           :height "20"
                                                           :width "20"
                                                           :alt "Increased"))
                                              (:span :class "label-green"
                                                     "9%")))))
                            (:div :class "span2"
                                  (:div :class "stat-block"
                                        (:ul
                                         (:li
                                          (:i :class "dashboard-icons-colors current_work_sl"))
                                         (:li :class "stat-count"
                                              (:span "Planned Activities")
                                              (:span 32)
                                              (:br))
                                         (:li :class "stat-percent"
                                              (:span (:img :scr "/appimg/green-arrow.png"
                                                           :height "20"
                                                           :width "20"
                                                           :alt "Increased"))
                                              (:span :class "label-green"
                                                     "53%")))))
                            (:div :class "span2"
                                  (:div :class "stat-block"
                                        (:ul
                                         (:li
                                          (:i :class "dashboard-icons-colors current_work_sl"))
                                         (:li :class "stat-count"
                                              (:span "Published Activities")
                                              (:span 31)
                                              (:br))
                                         (:li :class "stat-percent"
                                              (:span (:img :scr "/appimg/green-arrow.png"
                                                           :height "20"
                                                           :width "20"
                                                           :alt "Increased"))
                                              (:span :class "label-green"
                                                     "52%")))))
                            (:div :class "span2"
                                  (:div :class "stat-block"
                                        (:ul
                                         (:li
                                          (:i :class "dashboard-icons-colors current_work_sl"))
                                         (:li :class "stat-count"
                                              (:span "Planned Activities Published")
                                              (:span 11)
                                              )
                                         (:li :class "stat-percent"
                                              (:span (:img :scr "/appimg/green-arrow.png"
                                                           :height "20"
                                                           :width "20"
                                                           :alt "Increased"))
                                              (:span :class "label-green"
                                                     "42%"))))))
                      

                      ))))))

