(in-package :dyb)

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
                                  (:img  :style "padding :10px;" 
                                         :src "/appimg/save.png"))
                             (:td 
                              (:div :style "padding :10px"
                                    (str content)))))))))

(defun interval-days (days)
  (* days 24 60 60))

(defun within-date-range (interval date)
  (>
   date
   (- (get-universal-time) (interval-days interval) )))

(defun posts-scheduled (interval)
  (find-docs 'vector 
             (lambda (doc)

               (when (match-context-entities (get-val doc 'channel-user))

                 (when (within-date-range interval 
                                          (get-val doc 'scheduled-date))
                   (string-equal (get-val doc 'action-status) "completed"))))
             (generic-actions-collection)))

(defun posts-scheduled-count (interval)
  (length (posts-scheduled interval)))

(defun fb-friends-count ()
  (let ((count 0))
    (dolist (user (coerce (channel-users) 'list ))
      (when (valid-channel-user user "Facebook")  
        (if (get-val user 'user-data)
                (if (gethash "friends" (get-val user 'user-data))
                    (incf count 
                          (length (assoc-path
                                   (gethash "friends"
                                            (get-val user 'user-data))
                                   :data)))))))
    count))



(defun fb-insight-count (insight-name)
  (let ((count 0)
        (now (get-universal-time)))
    (dolist (user (coerce (channel-users) 'list ))
      (when (valid-channel-user user "Facebook")  
        (let ((fans
               (get-facebook-insight-values 
                user 
                (get-facebook-insight-by-name insight-name)
                (- now  (* 60 60 24 5)) 
                now)))

          (when fans
            
            (when (get-val fans 'value)
              (incf count (get-val fans 'value)))))))
    count))

(defun fb-insight-interval-count (insight-name interval)
  (let ((count 0)
        (now (get-universal-time)))
    (dolist (user (coerce (channel-users) 'list ))
      (when (valid-channel-user user "Facebook")
        (dolist (insights (get-facebook-insight-values 
                           user 
                           (get-facebook-insight-by-name insight-name) 
                           (- now (* 60 60 24 interval))
                           now))
          (when insights
            (when (get-val insights 'value)
              (incf count (get-val insights 'value)))))))
    count))

(defun fb-fans-count ()
  (fb-insight-count "page_fans"))

(defun fb-fans-adds-count (interval)
  (fb-insight-interval-count "page_fan_adds" interval))

(defun fb-like-adds-count (interval)
  (fb-insight-interval-count "page_like_adds" interval))


(defun fb-insight-range-count (insight-name start-date end-date)
  (let ((count 0))
    (dolist (user (coerce (channel-users) 'list ))
      (when (valid-channel-user user "Facebook")  
        (let ((fans
                    (get-facebook-insight-values 
                     user 
                     (get-facebook-insight-by-name insight-name)
                     start-date 
                     end-date)))
               (when fans
                 (when (get-val fans 'value)
                   (incf count (get-val fans 'value)))))))
    count))

(defun fb-like-adds-range-count (start-date end-date)
  (fb-insight-range-count "page_like_adds" start-date end-date))

(defun twitter-followers-count ()
  (let ((count 0))
    (dolist (user (coerce (channel-users) 'list ))
      (when (valid-channel-user user "Twitter")  
        (if (get-val user 'user-data)
            (when (gethash "followers" (get-val user 'user-data))
              (incf count (length (assoc-path
                                   (gethash "followers"
                                            (get-val user 'user-data))
                                   :ids)))))))
    (if (> count 0)
        (- count 1)
        count)))

(defun linkedin-connections-count ()
  (let ((count 0))
    (dolist (user (coerce (channel-users) 'list ))
      (when (valid-channel-user user "LinkedIn")  
        (when (get-val user 'user-data)
            (when (gethash "connections" (get-val user 'user-data))
              (if (gpv
                           (gethash "connections"
                                    (get-val user 'user-data))
                           :--total)
               (incf count (gpv
                            (gethash "connections"
                                     (get-val user 'user-data))
                            :--total)))))))
    count))

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
       when (match-entities (channel-user post) (context))
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
       when (match-entities (channel-user post) (context))
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
       when (match-entities (channel-user post) (context))
       when (equal (post-type post) 'twitter)
       do (incf count (twitter-retweets-dates-count 
                       interval 
                       (get-val post 'payload))))
    count))


(defun dash-menu-item (title icon href) 
  (with-html-string
    (:div :class "span2"
          (:div :class "dashboard-wid-wrap"
                (:div :class "dashboard-wid-content"
                      (:a :href href
                          (:i :class (format nil "dashboard-icons ~A" icon))
                          (:span :class "dasboard-icon-title"
                                 (str title))))))))

(defun dash-small-stat-graph (title graph-id range total-count total-percent)
  (with-html-string
    (:div :class "span3"
          (:div :class "stat-block"
                (:ul 
                 (:li :class "stat-graph"
                      :id graph-id
                      (str range)
                      )
                 (:li :class "stat-count"
                      (:span (str title))
                      (:span (str total-count)))
                 (:li :class "stat-percent"
                      (:span (:img :src "/appimg/green-arrow.png"
                                   :width "20"
                                   :height "20"
                                   :alt "Increase")
                             (:span :class "label-green" 
                                    (str (format nil "~A%" total-percent ))))))))))

(defun community-summary-item (title count icon alt style-p)
  (with-html-string
    (:span :class (if (not style-p) "summary-icon")
           :style "width: 36px;height: 36px;float: left;margin-right: 10px;padding: 6px;"
           (:img :src icon
                 :width "36" :height "36" :alt alt)
           )
    (:span :class "count" 
                  (str count))
           (:span :class "summary-title" (str title))))
;;TODO: Add date range

(defun board-stats (range title icons chart span)
  (with-html-string
    (:div :class span
          (:div :class "board-stats"
                (:div :class "statistics-wrap"
                      (:div :class "statistics-block"
                            (:div :class (format nil "stat-chart ~A" chart)
                                  (str range))
                            (:div :class "stat-info"
                                  (dolist (icon icons)
                                    (htm (:span :class (format nil "black-icons ~A" icon))))
                                  (str title))
                            ))))))

(defun network-size-graph (data)
  (with-html-string
    (:div :class "span4"
          (:div :class "graph-wrap"
                (:div :class "chart-block"
                      (let ((network-size 
                             (make-widget
                              'line-graph :name "currentnetworksize"
                              :data data)))
                        (setf (get-val network-size 'data) data)
                        (setf (title network-size) "Current Network Size")
                        (setf (x network-size)
                              '(:type :date
                                :tick-options (:format-string "%b&nbsp;%#d")))
                        (setf (y network-size)
                              '(:type :log
                                :tick-options (:format-string "")))
                        (setf (grid network-size)
                              '(:background "#fff"
                                :draw-border nil
                                :shadow nil
                                :grid-line-color "#ccc"
                                :grid-line-width 1))
                        (setf (highlighter network-size)
                              '(:show t :size-adjust 7.5))
                        (setf (legend network-size)
                              '(:show t :placement :inside))
                        (setf (series network-size)
                              '((:color "#00FFFF"
                                 :label "TW")
                                (:color "#0000FF"
                                 :label "FB")
                                (:color "#04B45F"
                                 :label "LNK")
                                (:color "#d44703"
                                 :label "Total"
                                 :style "x")))
                        (setf (series-defaults network-size) 
                              '(:show "true"
                                :xaxis "xaxis"
                                :yaxis "yaxis"
                                :line-width 3
                                :shadow "false"))
                        (render network-size)))))))

(defun engagement-graph (data)
  (with-html-string
    (:div :class "span4"
          (:div :class "graph-wrap"
                (:div :class "chart-block"
                      (let ((engagement 
                             (make-widget
                              'line-graph :name "engagementgraph"
                              :data data)))
                        (setf (get-val engagement 'data) data)
                        (setf (title engagement) "Engagement by Type")
                        
                        
                        (setf (grid engagement)
                              '(:background "#fff"
                                :draw-border nil
                                :shadow nil
                                :grid-line-color "#ccc"
                                :grid-line-width 1))
                        
                        (setf (legend engagement)
                              '(:show t :placement "w"))
                        
                        (setf (series-defaults engagement) 
                              '(:shadow "false"
                                :renderer :pie
                                :renderer-options
                                (:start-angle 180
                                 :slice-margin 4
                                 :show-data-labels "true")
                                ))
                        (render engagement)))))))

(define-easy-handler (dashboard-page :uri "/dyb/dashboard") ()
  
  (let* ((interval 7)
         (now (get-universal-time))
         (interval-start-date (- now (* 60 60 24 interval)))
         (interval-end-date now)
         (previous-interval-start-date (- now (* 60 60 24 (* interval 2))))
         (previous-interval-end-date (- now (* 60 60 24 interval)) )
         (page (make-widget 'page :name "dashboard-page"))
         (posts-scheduled-count (posts-scheduled-count interval))
         (fb-comments-made (fb-comments-made interval))
         (fb-likes-made (fb-likes-made interval))
         (twitter-retweets (twitter-retweets interval))
         (fb-friends-count (fb-friends-count))
         (fb-fans-count (fb-fans-count))
         (fb-fans-adds-count (fb-fans-adds-count interval))
         (fb-like-adds-count (fb-like-adds-count interval))
         (twitter-followers-count (twitter-followers-count))
         (linkedin-connections-count (linkedin-connections-count)))



    #|(break "~A ~A ~A ~A ~A ~A ~A"
           posts-scheduled-count
           fb-comments-made
           fb-likes-made
           twitter-retweets
           fb-friends-count
           fb-fans-count
           fb-fans-adds-count
           twitter-followers-count 
           linkedin-connections-count)|#
    
    (with-html
      
      (render page
              :body 
              (with-html-to-string ()
                "We are testing dashboard calculations. We apologize for any inconvenience."
                

                (:div :class "container-fluid"
                      (:div :class "page-header"
                            (:h1 "Dashboard"))
                      (:ul :class "breadcrumb"
                           (:li (:a :href "#" "Home")
                                (:span :class "divider" "&raquo;"))
                           (:li :class "active" "Dashboard"))
                      (:div :class "dashboard-widget"
                            (:div :class "row-fluid"
                                  (str (dash-menu-item "Inbox" 
                                                       "mail_blk" 
                                                       "/dyb/generix"))
                                  (str (dash-menu-item "Scheduler" 
                                                       "month_calendar_blk" 
                                                       "/dyb/generic-scheduler"))
                                  (str (dash-menu-item "Search Streams" 
                                                       "magnifying_glass_blk" 
                                                       "/dyb/search-stream"))
                                  (str (dash-menu-item "Reporting" 
                                                       "graph_blk" 
                                                       "#"))
                                  (str (dash-menu-item "Settings" 
                                                       "cog_2_blk" 
                                                       "#"))
                                  (str (dash-menu-item "Help" 
                                                       "help_blk" 
                                                       "#"))
                                  
                                  ))
                      (:div :class "page-header"
                            (:h3 "OVERVIEW") )
                      (:div :class "row-fluid"
                            (:form :name "dash-date-form"
                             (str "From" )
                             (:input :type "text" 
                                     :name "start-date" 
                                     :value (parameter "start-date"))
                             (str "To")
                             (:input :type "text" 
                                     :name "to-date" 
                                     :value (parameter "to-date"))))
                      (:div :class "row-fluid"
                            (str (dash-small-stat-graph 
                                  "Reach"
                                  "new-visits"
                                  (format nil "0,~A" 
                                          (+ fb-friends-count 
                                             fb-fans-count
                                             twitter-followers-count
                                             linkedin-connections-count))
                                  (+ fb-friends-count 
                                     fb-fans-count
                                     twitter-followers-count
                                     linkedin-connections-count)
                                  100)
                                   )
                            (str (dash-small-stat-graph 
                                  "Activity"
                                  "unique-visits"
                                  (format nil "0,~A" 
                                          posts-scheduled-count)
                                  posts-scheduled-count
                                  100)
                                   )
                            (str (dash-small-stat-graph 
                                  "Engagement"
                                  "weekly-sales"
                                  (format nil "0,~A" 
                                          (+ fb-fans-adds-count 
                                             fb-comments-made 
                                             fb-likes-made
                                             twitter-retweets))
                                  (+ fb-fans-adds-count 
                                     fb-comments-made 
                                     fb-likes-made
                                     twitter-retweets)
                                  100)
                                   )
                            )
                       
                   (:div :class "row-fluid"
                            (str (network-size-graph `(
                                                       ,(if (> (or (+  fb-friends-count
                                                                       fb-fans-count)
                                                                   0))
                                                           `(("2012-11-21" 
                                                             ,(or (+  fb-friends-count
                                                                       fb-fans-count) 
                                                                  0) 
                                                             )
                                                            ("2012-11-22" 
                                                             ,(or fb-friends-count 0))))
                                                       ,(if (> (or twitter-followers-count 0) 0)
                                                           `(("2012-11-21" 
                                                             ,(or twitter-followers-count 0))
                                                            ("2012-11-22" 
                                                             ,(or twitter-followers-count 0))))
                                                        (if (> (or linkedin-connections-count 0) 0)
                                                            `(("2012-11-21" 
                                                               ,(or linkedin-connections-count 0))
                                                              ("2012-11-22" 
                                                               ,(or linkedin-connections-count 0)))))))
                            (str (engagement-graph `((("Likes" ,fb-likes-made)
                                                      ("Clicks" 0)
                                                      ("Comments" ,fb-comments-made)
                                                      ("Retweets" ,twitter-retweets)
                                                      
                                                      ("Posts" ,posts-scheduled-count)
                                                      ("Mentions" 0)
                                                      ("Direct Messages" 0)))))
                            
                            (:div :class "span2"
                                  (:div :class "summary"
                                        (:h4 "CURRENT COMMUNITY SIZE")
                                        (:br)
                                        (:ul
                                         (:li
                                          (str (community-summary-item  
                                                "All Accounts"
                                                (+ fb-friends-count
                                                   fb-fans-count
                                                   twitter-followers-count
                                                   linkedin-connections-count)
                                                "/appimg/user-accounts.png"
                                                "All Accounts"
                                                nil
                                                ))
                                          
                                          )
                                         (:li
                                          (str (community-summary-item  
                                                " Facebook"
                                                fb-friends-count
                                                "/appimg/Facebook_Light_Logo.png"
                                                "Facebook Friends"
                                                t
                                                ))
                                          
                                          )
                                         (:li
                                          (str (community-summary-item  
                                                " Twitter"
                                                twitter-followers-count
                                                "/appimg/twitter-bird-white-on-blue.png"
                                                "Twitter Followers"
                                                t
                                                ))
                                          
                                          )
                                         (:li
                                          (str (community-summary-item  
                                                " LinkedIn"
                                                linkedin-connections-count
                                                "/appimg/linkedin-icon.png"
                                                "LinkedIn Connections"
                                                t
                                                )))))))
                      (:div :class "page-header"
                            (:h3 (:span :class "black-icons facebook" 
                                        :style "margin-top:1px;" )
                                 "FACEBOOK") )
                      (:div :class "row-fluid"
                            
                            (str (board-stats (format nil "~A,~A" 
                                                      (fb-like-adds-range-count 
                                                       (- now (* 60 60 24 (* interval 2))) 
                                                       (- now (* 60 60 24 interval)))
                                                      (fb-like-adds-range-count 
                                                       (- now (* 60 60 24 interval)) 
                                                       now)) 
                                              "New Likes" 
                                              (list "facebook_like") 
                                              "bar-chart" "span3"))
                            (str (board-stats (format nil "~A,~A" 
                                                      (fb-insight-range-count 
                                                       "page_views" 
                                                       previous-interval-start-date 
                                                       previous-interval-end-date)
                                                      (fb-insight-range-count 
                                                       "page_views" 
                                                       interval-start-date 
                                                       interval-end-date))
                                              "Page Impressions" 
                                              (list "documents")
                                              "bar-chart" "span3"))
                            (str (board-stats (format nil "~A,~A" 
                                                      (fb-insight-range-count 
                                                       "page_fans" 
                                                       previous-interval-start-date 
                                                       previous-interval-end-date)
                                                      (fb-insight-range-count 
                                                       "page_fans" 
                                                       interval-start-date 
                                                       interval-end-date))
                                              "Total Fans" 
                                              (list "users")
                                              "bar-chart" "span3"))
                            #|(str (board-stats "0,0"
                                              "Demographics" 
                                              (list "male_contour" "female_contour")
                                              "pie-chart" "span2"))|#
                            )
                      
                      
                      (:div :class "page-header"
                            (:h3 (:span :class "black-icons twitter" 
                                        :style "margin-top:1px;" )
                                 "TWITTER") )
                      (:div :class "row-fluid"
                            
                            (str (board-stats (format nil "0,~A" twitter-followers-count) 
                                              "New Followers" 
                                              (list "facebook_like") 
                                              "bar-chart" "span3"))
                            (str (board-stats (format nil "0,~A" 0)
                                              "Page Impressions" 
                                              (list "documents")
                                              "bar-chart" "span3"))
                            (str (board-stats (format nil "0,~A" twitter-followers-count)
                                              "Total Fans" 
                                              (list "users")
                                              "bar-chart" "span3"))
                            #|(str (board-stats "0,0"
                                              "Demographics" 
                                              (list "male_contour" "female_contour")
                                              "pie-chart" "span2"))|#
                            )
                      #|
                      (:div :class "page-header"
                            (:h3 (:span :class "black-icons linkedin" 
                                        :style "margin-top:1px;" )
                                 "LINKEDIN") )
                      (:div :class "row-fluid"
                            
                            (str (board-stats (format nil "0,~A" linkedin-connections-count) 
                                              "New Connections" 
                                              (list "facebook_like") 
                                              "bar-chart" "span3"))
                            (str (board-stats (format nil "0,~A" 0)
                                              "Page Impressions" 
                                              (list "documents")
                                              "bar-chart" "span3"))
                            (str (board-stats (format nil "0,~A" linkedin-connections-count)
                                              "Total Followers" 
                                              (list "users")
                                              "bar-chart" "span3"))
                            (str (board-stats "0,0"
                                              "Demographics" 
                                              (list "male_contour" "female_contour")
                                              "pie-chart" "span2"))
                            )
                |#
                      
                      ))
))))

