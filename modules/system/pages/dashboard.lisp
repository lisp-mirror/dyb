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


(defun fb-friends-count ()
  (let ((count 0))
    (dolist (user (coerce (channel-users) 'list ))
      (when (valid-channel-user user "Facebook")  
        (if (get-val user 'user-data)
                (if (gethash "friends" (get-val user 'user-data))
                    (incf count (length (assoc-path
                                         (gethash "friends"
                                                  (get-val user 'user-data))
                                         :data)))))))
    count))

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
              (incf count (gpv
                           (gethash "connections"
                                    (get-val user 'user-data))
                           :--total))))))
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
       when (match-entities post (context))
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
       when (match-entities post (context))
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


(defun dash-menu-item (title icon href) 
  (with-html-string
    (:div :class "span2"
          (:div :class "dashboard-wid-wrap"
                (:div :class "dashboard-wid-content"
                      (:a :href href
                          (:i :class (format nil "dashboard-icons ~A" icon))
                          (:span :class "dasboard-icon-title"
                                 (str title))))))))

(defun dash-small-stat-graph (graph-id range total-count total-percent)
  (with-html-string
    (:div :class "span3"
          (:div :class "stat-block"
                (:ul 
                 (:li :class "stat-graph"
                      :id graph-id
                      (str range)
                      )
                 (:li :class "stat-count"
                      (:span "Reach")
                      (:span (str total-count)))
                 (:li :class "stat-percent"
                      (:span (:img :src "/appimg/green-arrow.png"
                                   :width "20"
                                   :height "20"
                                   :alt "Increase")
                             (:span :class "label-green" 
                                    (str (format nil "~A%" total-percent ))))))))))

(define-easy-handler (dashboard-page :uri "/dyb/dashboard") ()
  
  (let ((page (make-widget 'page :name "dashboard-page"))
        (fb-friends-count (fb-friends-count))
        (twitter-followers-count (twitter-followers-count))
        (linkedin-connections-count (linkedin-connections-count)))
    
    (with-html
      (render page
              :body 
              (with-html-to-string ()
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
                                  "new-visits"
                                  (format nil "0,~A" 
                                          (+ fb-friends-count 
                                             twitter-followers-count
                                             linkedin-connections-count))
                                  (+ fb-friends-count 
                                     twitter-followers-count
                                     linkedin-connections-count)
                                  100)
                                   )
                            ))
                
                
#|

"<div class=\"container-fluid\">
	<div class=\"page-header\">
      <h1>Dashboard</h1>
	</div>
    <ul class=\"breadcrumb\">
      <li><a href=\"#\">Home</a><span class=\"divider\">&raquo;</span></li>
      <li class=\"active\">Dashboard</li>
    </ul>
   
    
	
	
    <div class=\"page-header\">
      <h3>OVERVIEW</h3>
	</div>
	<div class=\"row-fluid\">
		<div class=\"span3 \">
        <div class=\"stat-block\">
          <ul>
            <li class=\"stat-graph\" id=\"new-visits\">50,80,100,120,110,150,180</li>
            <li class=\"stat-count\"><span>Reach</span><span>+20</span></li>
            <li class=\"stat-percent\"><span><img src=\"/appimg/green-arrow.png\" width=\"20\" height=\"20\" alt=\"Increase\"></span><span class=\"label-green\">20</span></li>
          </ul>
        </div>
		</div>
		<div class=\"span3 \">
        <div class=\"stat-block\">
          <ul>
            <li class=\"stat-graph\" id=\"unique-visits\">3,4,6,5,15,40</li>
            <li class=\"stat-count\"><span>Activity</span><span>+25</span></li>
            <li class=\"stat-percent\"><span><img src=\"/appimg/green-arrow.png\" width=\"20\" height=\"20\" alt=\"Increased\"></span><span class=\"label-green\">25%</span></li>
          </ul>
        </div>
		</div>
		<div class=\"span3 \">
		<div class=\"stat-block\">
          <ul>
            <li class=\"stat-graph\" id=\"weekly-sales\">90,85,90,100,115,126</li>
            <li class=\"stat-count\"><span>Engagement</span><span>126</span></li>
            <li class=\"stat-percent\"><span><img src=\"/appimg/green-arrow.png\" width=\"20\" height=\"20\" alt=\"Increased\"></span><span class=\"label-green\">5%</span></li>
          </ul>
        </div>
		</div>
		
		
	</div>
	<div class=\"row-fluid\">
		<div class=\"span4\">
        	<div class=\"graph-wrap\">
				<div class=\"chart-block\">
					<div id=\"chart4\">
					</div>
				</div>
            </div>
		</div>
		<div class=\"span6\">
			<div class=\"index-graph\">
				<div class=\"chart-block\">
					<div id=\"chart1\"> </div>
				</div>
			</div>
		</div>
		<div class=\"span2\">
        <div class=\"summary\">
			<h4>CURRENT COMMUNITY SIZE</h4>
			</br>
          <ul>
            <li><span class=\"summary-icon\"><img src=\"/appimg/user-accounts.png\" width=\"36\" height=\"36\" alt=\"All Accounts\"></span><span class=\"count\">373</span><span class=\"summary-title\"> All Accounts</span></li>
            <li><span style=\"width: 36px;height: 36px;float: left;margin-right: 10px;padding: 6px;\"><img src=\"/appimg/Facebook_Light_Logo.png\" width=\"36\" height=\"36\" alt=\"New Items\"></span><span class=\"count\">309</span><span class=\"summary-title\"> Facebook</span></li>
            <li><span style=\"width: 36px;height: 36px;float: left;margin-right: 10px;padding: 6px;\"><img src=\"/appimg/twitter-bird-white-on-blue.png\" width=\"36\" height=\"36\" alt=\"New Posts\"></span><span class=\"count\">42</span><span class=\"summary-title\"> Twitter</span></li>
            <li><span style=\"width: 36px;height: 36px;float: left;margin-right: 10px;padding: 6px;\"><img src=\"/appimg/linkedin-icon.png\" width=\"36\" height=\"36\" alt=\"New Orders\"></span><span class=\"count\">22</span><span class=\"summary-title\"> LinkedIn</span></li>
            
          </ul>
        </div>
      </div>
      
    </div>
	<div class=\"page-header\">
            <h3><span class=\"black-icons facebook\" style=\"margin-top:1px;\"></span> FACEBOOK</h3>
        </div>
	<div class=\"row-fluid\">
		
		
		<div class=\"span3\">
			<div class=\"board-stats\">
				<div class=\"statistics-wrap\">
					<div class=\"statistics-block\">
						<div class=\"stat-chart bar-chart\">1,0,2,3,0,1,2,1,0,2,0,0,-1,2,1,0,2,3,0</div>
						<div class=\"stat-info\"><span class=\"black-icons facebook_like\"></span> New Likes</div>
					</div>
				</div>
    
			</div>
		</div>
		<div class=\"span3\">
			<div class=\"board-stats\">
				<div class=\"statistics-wrap\">
					<div class=\"statistics-block\">
						<div class=\"stat-chart bar-chart\">10,12,15,9,8,5,14,12,4,20,25,26,17,10,9,9,10,7,13</div>
						<div class=\"stat-info\"><span class=\"black-icons documents\"></span> Page Impressions</div>
					</div>
				</div>
    
			</div>
		</div>
		<div class=\"span3\">
			<div class=\"board-stats\">
				<div class=\"statistics-wrap\">
					<div class=\"statistics-block\">
						<div class=\"stat-chart bar-chart\">306,307,307,307,307,307,307,307,308,308,308,308,308,308,308,308,308,308</div>
						<div class=\"stat-info\"><span class=\"black-icons users\"></span> Total Fans</div>
					</div>
				</div>
    
			</div>
		</div>
		<div class=\"span2\">
			<div class=\"board-stats\">
				<div class=\"statistics-wrap\">
					<div class=\"statistics-block\">
						<div class=\"stat-chart pie-chart\">170,139</div>
						<div class=\"stat-info\"><span class=\"black-icons male_contour\"></span><span class=\"black-icons female_contour\"></span> Demographics</div>
					</div>
				</div>
    
			</div>
		</div>
		
	</div>
	<div class=\"page-header\">
            <h3><span class=\"black-icons twitter\" style=\"margin-top:1px;\"></span> TWITTER</h3>
      </div>
    <div class=\"row-fluid\">
	    <div class=\"span3\">
			<div class=\"board-stats\">
				<div class=\"statistics-wrap\">
					<div class=\"statistics-block\">
						<div class=\"stat-chart bar-chart\">1,0,2,3,0,1,2,1,0,2,0,0,-1,2,1,0,2,3</div>
						<div class=\"stat-info\"><span class=\"black-icons facebook_like\"></span> New Followers</div>
					</div>
				</div>
    
			</div>
		</div>
		<div class=\"span3\">
			<div class=\"board-stats\">
				<div class=\"statistics-wrap\">
					<div class=\"statistics-block\">
						<div class=\"stat-chart bar-chart\">10,12,15,9,8,5,14,12,4,20,25,26,17,10,9,9,10</div>
						<div class=\"stat-info\"><span class=\"black-icons documents\"></span> Page Impressions</div>
					</div>
				</div>
    
			</div>
		</div>
		<div class=\"span3\">
			<div class=\"board-stats\">
				<div class=\"statistics-wrap\">
					<div class=\"statistics-block\">
						<div class=\"stat-chart bar-chart\">37,38,38,38,38,38,38,38,39,40,39,39,39,40,40</div>
						<div class=\"stat-info\"><span class=\"black-icons users\"></span> Total Followers</div>
					</div>
				</div>
    
			</div>
		</div>
		<div class=\"span2\">
			<div class=\"board-stats\">
				<div class=\"statistics-wrap\">
					<div class=\"statistics-block\">
						<div class=\"stat-chart pie-chart\">170,139</div>
						<div class=\"stat-info\"><span class=\"black-icons male_contour\"></span><span class=\"black-icons female_contour\"></span> Demographics</div>
					</div>
				</div>
    
			</div>
		</div>
      
      
    </div>
	
	<div class=\"page-header\">
            <h3><img src=\"/appimg/linkedin-icon1.png\" style=\"margin-top:-6px;\"> LINKEDIN</h3>
      </div>
	
	<div class=\"row-fluid\">
		<div class=\"span3\">
			<div class=\"board-stats\">
				<div class=\"statistics-wrap\">
					<div class=\"statistics-block\">
						<div class=\"stat-chart bar-chart\">1,0,2,3,0,1,2,1,0,2,0,0,-1,2,1</div>
						<div class=\"stat-info\"><span class=\"black-icons facebook_like\"></span> New Followers</div>
					</div>
				</div>
    
			</div>
		</div>
		<div class=\"span3\">
			<div class=\"board-stats\">
				<div class=\"statistics-wrap\">
					<div class=\"statistics-block\">
						<div class=\"stat-chart bar-chart\">10,12,15,9,8,5,14,12,4,20,25,26,17,10,9,9,10,7</div>
						<div class=\"stat-info\"><span class=\"black-icons documents\"></span> Page Impressions</div>
					</div>
				</div>
    
			</div>
		</div>
		<div class=\"span3\">
			<div class=\"board-stats\">
				<div class=\"statistics-wrap\">
					<div class=\"statistics-block\">
						<div class=\"stat-chart bar-chart\">17,18,18,18,18,18,18,18,19,20,19,19,19,20,20,21</div>
						<div class=\"stat-info\"><span class=\"black-icons users\"></span> Total Followers</div>
					</div>
				</div>
    
			</div>
		</div>
		<div class=\"span2\">
			<div class=\"board-stats\">
				<div class=\"statistics-wrap\">
					<div class=\"statistics-block\">
						<div class=\"stat-chart pie-chart\">170,139</div>
						<div class=\"stat-info\"><span class=\"black-icons male_contour\"></span><span class=\"black-icons female_contour\"></span> Demographics</div>
					</div>
				</div>
    
			</div>
		</div>
      
    </div>"

                (:div :class "dashboard-widget"
                      (:div :class "row-fluid"
                            (:div :class "span2"
                                  (:div :class "dashboard-wid-wrap"
                                        (:div :class "dashboard-wid-content"
                                              (:a :href "/dyb/generic"
                                                     (:i :class "dashboard-icons mail_blk")
                                                     (:span :class "dasboard-icon-title"
                                                            "Inbox")))))
                            (:div :class "span2"
                                  (:div :class "dashboard-wid-wrap"
                                        (:div :class "dashboard-wid-content"
                                              (:a :href "/dyb/generic-scheduler"
                                                     (:i :class "dashboard-icons month_calendar_blk")
                                                     (:span :class "dasboard-icon-title"
                                                            "Scheduler")))))
                            (:div :class "span2"
                                  (:div :class "dashboard-wid-wrap"
                                        (:div :class "dashboard-wid-content"
                                              (:a :href "/dyb/search-stream"
                                                     (:i :class "dashboard-icons magnifying_glass_blk")
                                                     (:span :class "dasboard-icon-title"
                                                            "Search Streams")))))
                            (:div :class "span2"
                                  (:div :class "dashboard-wid-wrap"
                                        (:div :class "dashboard-wid-content"
                                              (:a :href "/dyb/search-stream"
                                                     (:i :class "dashboard-icons graph_blk")
                                                     (:span :class "dasboard-icon-title"
                                                            "Reporting (Coming Soon)")))))
                            (:div :class "span2"
                                  (:div :class "dashboard-wid-wrap"
                                        (:div :class "dashboard-wid-content"
                                              (:a :href "/dyb/channel-users"
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
                                              (:span (:img :src "/appimg/green-arrow.png"
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
                                              (:span (:img :src "/appimg/green-arrow.png"
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
                                              (:span (:img :src "/appimg/green-arrow.png"
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
                                              (:span (:img :src "/appimg/green-arrow.png"
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
                                              (:span (:img :src "/appimg/green-arrow.png"
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
                                              (:span (:img :src "/appimg/green-arrow.png"
                                                           :height "20"
                                                           :width "20"
                                                           :alt "Increased"))
                                              (:span :class "label-green"
                                                     "42%"))))))
                      

                      )
|#

)))))

