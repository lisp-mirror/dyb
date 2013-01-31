(in-package :dyb)


(defun universal-today ()
  (multiple-value-bind (sec min hour day month year)
          (decode-universal-time           
           (get-universal-time)
           (time-zone))
    (declare (ignore sec min hour))
    (encode-universal-time 0 0 0 day month year (time-zone))))

(defun universal-dat-strip-time (date)
  (multiple-value-bind (sec min hour day month year)
          (decode-universal-time           
           date
           (time-zone))
    (declare (ignore sec min hour))
    (encode-universal-time 0 0 0 day month year (time-zone))))

(defun or-zero (values)
  :documetation "Selects the first not 0 value."
  (let ((value 0))
    (dolist (val values)
      (when (= value 0)
          (unless (= val 0)
            (setf value val))))
    value))


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

(defun posts-scheduled-range-count (start-date end-date &key post-type)
  (let ((count 0))
    (find-docs 'vector 
               (lambda (doc)

                 (when (match-context-entities (get-val doc 'channel-user))
                   
                   (when (and (>= (get-val doc 'scheduled-date) start-date)
                              (<= (get-val doc 'scheduled-date) end-date))
                     (string-equal (get-val doc 'action-status) "completed")
                     (if post-type
                         (if (string-equal post-type (get-val doc 'post-type))
                             (incf count 1))
                         (incf count 1))
                     )))
               (generic-actions-collection))
    count))

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

(defun fb-insight-count (insight-name &key target-date)
  (let ((count 0)
        (the-day (if target-date
                     target-date
                     (universal-today))))
    (dolist (user (coerce (channel-users) 'list ))
      (when (valid-channel-user user "Facebook")  
        ;;Need to make adjustments for dates because fb insights
        ;;are 3 days behind.
        (let* ((start-date (if (equal the-day (universal-today))
                              (- the-day  (* 60 60 24 4))
                              (- the-day  (* 60 60 24 1))
                              ))
              (end-date (if (equal the-day (universal-today))
                              (- the-day  (* 60 60 24 3))
                              the-day))
              (fans
               (get-facebook-insight-values 
                user 
                (get-facebook-insight-by-name insight-name)
                start-date
                end-date)))
          (when fans
            (dolist (fan fans)
              (when (get-val fan 'value)
                ;;TODO: Do we need to check for 0 incase 3 days is not enough?
                (setf count (get-val fan 'value))))))))
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
  (fb-insight-interval-count "page_fan_adds" interval))


(defun fb-insight-range (insight-name start-date end-date)
  (let ((range))
    (dolist (user (coerce (channel-users) 'list ))
      (when (valid-channel-user user "Facebook")  
        (let ((fans
                    (get-facebook-insight-values 
                     user 
                     (get-facebook-insight-by-name insight-name)
                     start-date 
                     end-date)))
               (when fans
                 (dolist (fan fans)
                   (break "fan ~A" fan)
                   (when (get-val fan 'value)
                     (setf range (append range (list (list (format-universal-date-dash (get-val fan 'end-time)) 
                                                           (get-val fan 'value)))))))))))
    range))

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
                 (dolist (fan fans)
                   (when (get-val fan 'value)
                     (incf count (get-val fan 'value))))))))
    count))

(defun fb-like-adds-range-count (start-date end-date)
  (fb-insight-range-count "page_fan_adds" start-date end-date))

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



(defun twitter-followers-day-range (start-date end-date)
  (let ((followers (make-hash-table :test 'equal))
        (followers-list)
        (final-list))
    (dolist (user (coerce (channel-users) 'list ))
      (let ((stamp-date (universal-dat-strip-time (get-val user 'stamp-date)))
            (previous-stamp-date))
        (when (valid-channel-user user "Twitter")
            (when (and (>= stamp-date start-date)
                       (<= stamp-date end-date))
              (if (get-val user 'user-data)
                  (when (gethash "followers" (get-val user 'user-data))

                    (setf (gethash stamp-date followers)
                          (length (assoc-path
                                   (gethash "followers"
                                            (get-val user 'user-data))
                                   :ids)))
                    
                    (setf previous-stamp-date stamp-date))))
           
            (dolist (old (get-val user 'old-versions))
                (setf stamp-date (universal-dat-strip-time (get-val old 'stamp-date)))
                (when (and (>= stamp-date start-date)
                             (<= stamp-date end-date))

                  (when (not (equal previous-stamp-date stamp-date))
                    (when (gethash "followers" (get-val old 'user-data))   
                      
                      (setf (gethash stamp-date followers) 
                            (length (assoc-path
                                     (gethash "followers"
                                              (get-val old 'user-data))
                                     :ids)))))
                  (setf previous-stamp-date stamp-date))))))
    (maphash 
     (lambda (date value)
       (setf followers-list (append followers-list 
                                    (list (list date 
                                                value)))))
     followers)
    (dolist (follow (sort followers-list #'< :key #'car))
      (setf final-list (append
                            final-list
                            (list (list (format-universal-date-dash (first follow))
                                        (second follow)))) ))
    final-list))




(defun twitter-followers-day-range-count (start-date end-date)
  (let ((count 0))
    (dolist (user (coerce (channel-users) 'list ))
      (when (valid-channel-user user "Twitter")
        (when (and (>= (get-val user 'stamp-date) start-date)
                   (<= (get-val user 'stamp-date) end-date))
          (if (get-val user 'user-data)
            (when (gethash "followers" (get-val user 'user-data))
              (incf count (length (assoc-path
                                   (gethash "followers"
                                            (get-val user 'user-data))
                                   :ids))))))
        (unless (and (>= (get-val user 'stamp-date) start-date)
                   (<= (get-val user 'stamp-date) end-date))
          (let ((val 0))
            (dolist (old (get-val user 'old-versions))
              (when (gethash "followers" (get-val old 'user-data))
                (setf val (length (assoc-path
                                   (gethash "followers"
                                            (get-val old 'user-data))
                                   :ids)))))
            (incf count val)))))
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


(defun twitter-retweet-day-range-count (start-date end-date)
  (let ((count 0))
    (dolist (user (coerce (channel-users) 'list ))
      (when (valid-channel-user user "Twitter")
        (when (and (>= (get-val user 'stamp-date) start-date)
                   (<= (get-val user 'stamp-date) end-date))
          (if (get-val user 'user-data)
            (when (gethash "profile" (get-val user 'user-data))
              (incf count (gpv
                                   (gethash "profile"
                                            (get-val user 'user-data))
                                   :status :retweet--count)))))
        (unless (and (>= (get-val user 'stamp-date) start-date)
                   (<= (get-val user 'stamp-date) end-date))
          (let ((val 0))
            (dolist (old (get-val user 'old-versions))
              (when (gethash "profile" (get-val old 'user-data))
                (setf val (gpv
                                   (gethash "profile"
                                            (get-val old 'user-data))
                                   :status :retweet--count)))
              )
            (incf count val)))

        ))
    (if (> count 0)
        (- count 1)
        count)))

(defun twitter-statuses-day-range-count (start-date end-date)
  (let ((count 0))
    (dolist (user (coerce (channel-users) 'list ))
      (when (valid-channel-user user "Twitter")
        (when (and (>= (get-val user 'stamp-date) start-date)
                   (<= (get-val user 'stamp-date) end-date))
          (if (get-val user 'user-data)
            (when (gethash "profile" (get-val user 'user-data))
              (incf count (gpv
                                   (gethash "profile"
                                            (get-val user 'user-data))
                                   :statuses--count)))))
        (unless (and (>= (get-val user 'stamp-date) start-date)
                   (<= (get-val user 'stamp-date) end-date))
          (let ((val 0))
            (dolist (old (get-val user 'old-versions))
              (when (gethash "profile" (get-val old 'user-data))
                (setf val (gpv
                                   (gethash "profile"
                                            (get-val old 'user-data))
                                   :statuses--count)))
              )
            (incf count val)))

        ))
    (if (> count 0)
        (- count 1)
        count)))

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
                      (:span (:img :src (if (>= total-percent 0)
                                            "/appimg/green-arrow.png"
                                            "/appimg/red-arrow.png")
                                   :width "20"
                                   :height "20"
                                   :alt "Increase")
                             (if (>= total-percent 0)
                                 (htm (:span :class "label-green" 
                                             (str (format nil "~A%" (format-money total-percent) ))))
                                 (htm (:span :class "label-red" 
                                             (str (format nil "~A%" (format-money total-percent) ))))))))))))

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

(defun network-size-graph (min-date max-date data)
  
  (with-html-string
    (:div :class "span9"
          (:div :class "graph-wrap"
                (:div :class "chart-block"
                      (let ((network-size 
                             (make-widget
                              'line-graph :name "currentnetworksize"
                              :data data)))
                        (setf (get-val network-size 'data) data)
                        (setf (title network-size) "Current Network Size")
                        (setf (x network-size)
                              `(:type :date
                                :tick-interval "7 days"
                                :min ,min-date
                                :max ,max-date
                                :tick-options (:format-string "%b&nbsp;%#d")))
                        (setf (y network-size)
                              '(
                                ;;:type :log
                                :min 0
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
                              '(:show t :placement :outside))
                        (setf (series network-size)
                              '((:show-marker nil
                                 :color "#00FFFF"
                                 :label "TW")
                                (:show-marker nil
                                 :color "#0000FF"
                                 :label "FB")
                                (:show-marker nil
                                 :color "#04B45F"
                                 :label "LNK")))
                        (setf (series-defaults network-size) 
                              '(:show "true"
                                :xaxis "xaxis"
                                :yaxis "yaxis"
                                :line-width 3
                                :shadow "false"))
                        (render network-size)))))))

(defun engagement-graph (data)
  (with-html-string
    (:div :class "span6"
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


(defun facebook-last-insight ()
  (dolist (user (coerce (channel-users) 'list ))
      (when (match-context-entities user)
        (when (and user (string-equal (get-val user 'doc-status) "Active"))
          ;;TODO: How to get error messages in for users without access tokens.
          (when (string-equal (get-val user 'channel-user-type) "Facebook")
            (when (get-val user 'last-access-token)
              (return-from facebook-last-insight  (get-last-insight-date user))))))))



(defun calc-prev-cur-percentage (prev cur)
  (if (> prev cur)
      (if (not (= prev 0))
          (* (/ (- cur
                         prev)
                      prev)
                   100)
          100)
      (if (not (= cur 0))
          (* (/ (- prev
                   cur)
                cur)
             -100)
          -100)))

(defun interval-selection ()
  (with-html-to-string ()
    (:div :class "row-fluid"
          (:div :class "nonboxy-widget"
                (:div :class "widget-head"
                      (:h5 (:i :class "black-icons month_calendar")
                           (str "Dashboard Interval")))

                (:div :class "widget-content"
                      (:div :class "widget-box"
                            (:form :name "dash-date-form" 
                                   (:div :class "row-fluid"
                                         (:div :class "span1"
                                               (:input :type :radio :name "dashboard-interval" (str "7 Days")))
                                         (:div :class "span1"
                                               (:input :type :radio :name "dashboard-interval" (str "30 Days")))
                                         (:div :class "span1"
                                               (:input :type :radio :name "dashboard-interval" (str "365 Days"))))
                                   (:div :class "row-fluid"
                                         (:div :class "span5"
                                               (str "From Date")
                                               (render-edit-field 
                                                "interval-start-date"
                                                (parameter "interval-start-date")
                                                :type :date
                                                :required nil
                                                :width "100px")
                                               (str " To Date")
                                               (render-edit-field 
                                                "interval-end-date"
                                                (parameter "interval-end-date")
                                                :type :date
                                                :required nil
                                                :width "100px")
                                               ))
                                   ))
                      (:div :class "widget-bottom"
                            (:input :type "submit" 
                                    :method "post"
                                    :class "btn btn-info" 
                                    :name "set-dash-interval" 
                                    :value "Set Interval")))))))

(defun dashboard-overview ()
  (with-html-to-string ()
    ))

(defconstant +24h-secs+ (* 60 60 24))

(defun calc-date-interval ()
  (let* ((today (universal-today))
        (interval 80)
        (start-date (- today (* +24h-secs+ interval)))
        (end-date today))
    (when (and (not (empty-p (parameter "interval-start-date")))
             (not (empty-p (parameter "interval-end-date"))))
        (setf start-date (string-to-date (parameter "interval-start-date")))
        (setf end-date (string-to-date (parameter "interval-end-date")))
        (setf interval (truncate (/ (- end-date start-date)
                                     +24h-secs+))))
    (unless (and (not (empty-p (parameter "interval-start-date")))
                 (not (empty-p (parameter "interval-end-date")))
                 (not (empty-p (parameter "dashboard-interval"))))
      (cond ((string-equal (parameter "dashboard-interval") "7 Days")
             (setf interval 7))
            ((string-equal (parameter "dashboard-interval") "30 Days")
             (setf interval 30))
            ((string-equal (parameter "dashboard-interval") "365 Days")
             (setf interval 365)))
      (setf start-date (- today (* +24h-secs+ interval)))
      (setf end-date today))

    (setf (session-value 'dashboard-interval) interval)
    (values interval start-date end-date)))

(defun network-graph-data-list (data)
  (let ((format-list)) 
    (dolist (item data)
      (setf format-list
            (append format-list
                    (list
                     `(,(format-universal-date-dash (first item)) 
                        ,(second item))))))
    `(,format-list)))

(define-easy-handler (dashboard-page :uri "/dyb/dashboard") ()

  (multiple-value-bind (interval interval-start-date interval-end-date)
      (calc-date-interval)

    (let* ((page (make-widget 'page :name "dashboard-page"))
           (now (universal-today))
           (previous-interval-start-date 
            (- now (* +24h-secs+ (* interval 2))))
           (previous-interval-end-date 
            (- now (* +24h-secs+ interval)))
           (posts-scheduled-count 
            (posts-scheduled-range-count 
             interval-start-date 
             interval-end-date))
           (posts-scheduled-previous-count 
            (posts-scheduled-range-count 
             previous-interval-start-date 
             previous-interval-end-date))
           (fb-comments-count 
            (fb-insight-range-count 
             "page_comment_adds" 
             interval-start-date 
             interval-end-date))
           (fb-comments-previous-count 
            (fb-insight-range-count 
             "page_comment_adds" 
             previous-interval-start-date 
             previous-interval-end-date))

           (fb-friends-count (fb-friends-count))
           (fb-fans-count 
            (fb-insight-count 
             "page_fans"
             :target-date interval-end-date))
           (fb-fans-previous-count 
            (fb-insight-count 
             "page_fans"
             :target-date previous-interval-end-date))

           (fb-fans-adds-count 
            (fb-insight-range-count 
             "page_fan_adds" 
             interval-start-date 
             interval-end-date))
           (fb-fans-adds-previous-count 
            (fb-insight-range-count 
             "page_fan_adds" 
             previous-interval-start-date 
             previous-interval-end-date))
         
           (twitter-retweets 
            (twitter-retweet-day-range-count 
             interval-start-date 
             interval-end-date))
           (twitter-retweets-previous 
            (twitter-retweet-day-range-count 
             previous-interval-start-date 
             previous-interval-end-date))
           (twitter-followers-count 
            (twitter-followers-day-range-count 
             interval-start-date 
             interval-end-date))
           (twitter-followers-previous-count 
            (twitter-followers-day-range-count 
             previous-interval-start-date 
             previous-interval-end-date))

           (twitter-followers-interval-list 
            (twitter-followers-day-range
             interval-start-date 
             interval-end-date))

           (fb-fans-interval-list
            (fb-insight-range
             "page_fans" 
             interval-start-date 
             interval-end-date))
           

           (linkedin-connections-count (linkedin-connections-count))
           (fb-fans-9  (fb-insight-range-count 
                        "page_fans" 
                        (- now (* +24h-secs+ 9)) 
                        (- now (* +24h-secs+ 8))))
           (fb-fans-8  (fb-insight-range-count 
                        "page_fans" 
                        (- now (* +24h-secs+ 8)) 
                        (- now (* +24h-secs+ 7))))
           (fb-fans-7 (fb-insight-range-count 
                       "page_fans" 
                       (- now (* +24h-secs+ 7)) 
                       (- now (* +24h-secs+ 6))))
           (fb-fans-6 (fb-insight-range-count 
                       "page_fans" 
                       (- now (* +24h-secs+ 6)) 
                       (- now (* +24h-secs+ 5))))
           (fb-fans-5 (fb-insight-range-count 
                       "page_fans" 
                       (- now (* +24h-secs+ 5)) 
                       (- now (* +24h-secs+ 4))))
           (fb-fans-4 (fb-insight-range-count 
                       "page_fans" 
                       (- now (* +24h-secs+ 4)) 
                       (- now (* +24h-secs+ 3))))
           (fb-fans-3 (fb-insight-range-count 
                       "page_fans" 
                       (- now (* +24h-secs+ 3)) 
                       (- now (* +24h-secs+ 2))))
           (fb-fans-2 (fb-insight-range-count 
                       "page_fans" 
                       (- now (* +24h-secs+ 2)) 
                       (- now (* +24h-secs+ 1))))
           (fb-fans-1 (fb-insight-range-count 
                       "page_fans" 
                       (- now (* +24h-secs+ 1)) 
                       (- now)))
         
           (twit-fans-8 (twitter-followers-day-range-count
                         (- now (* +24h-secs+ 8)) 
                         (- now (* +24h-secs+ 7))))
           (twit-fans-7 (twitter-followers-day-range-count 
                         (- now (* +24h-secs+ 7)) 
                         (- now (* +24h-secs+ 6))))
           (twit-fans-6 (twitter-followers-day-range-count 
                         (- now (* +24h-secs+ 6)) 
                         (- now (* +24h-secs+ 5))))
           (twit-fans-5 (twitter-followers-day-range-count 
                         (- now (* +24h-secs+ 5)) 
                         (- now (* +24h-secs+ 4))))
           (twit-fans-4 (twitter-followers-day-range-count 
                         (- now (* +24h-secs+ 4)) 
                         (- now (* +24h-secs+ 3))))
           (twit-fans-3 (twitter-followers-day-range-count 
                         (- now (* +24h-secs+ 3)) 
                         (- now (* +24h-secs+ 2))))
           (twit-fans-2 (twitter-followers-day-range-count 
                         (- now (* +24h-secs+ 2)) 
                         (- now (* +24h-secs+ 1))))
           (twit-fans-1 (twitter-followers-day-range-count 
                         (- now (* +24h-secs+ 1)) 
                         now))
           (twit-retweets-8 (twitter-retweet-day-range-count
                             (- now (* +24h-secs+ 8)) 
                             (- now (* +24h-secs+ 7))))
           (twit-retweets-7 (twitter-retweet-day-range-count 
                             (- now (* +24h-secs+ 7)) 
                             (- now (* +24h-secs+ 6))))
           (twit-retweets-6 (twitter-retweet-day-range-count 
                             (- now (* +24h-secs+ 6)) 
                             (- now (* +24h-secs+ 5))))
           (twit-retweets-5 (twitter-retweet-day-range-count 
                             (- now (* +24h-secs+ 5)) 
                             (- now (* +24h-secs+ 4))))
           (twit-retweets-4 (twitter-retweet-day-range-count 
                             (- now (* +24h-secs+ 4)) 
                             (- now (* +24h-secs+ 3))))
           (twit-retweets-3 (twitter-retweet-day-range-count 
                             (- now (* +24h-secs+ 3)) 
                             (- now (* +24h-secs+ 2))))
           (twit-retweets-2 (twitter-retweet-day-range-count 
                             (- now (* +24h-secs+ 2)) 
                             (- now (* +24h-secs+ 1))))
           (twit-retweets-1 (twitter-retweet-day-range-count 
                             (- now (* +24h-secs+ 1)) 
                             now))

           (fb-fan-adds-9  (fb-insight-range-count 
                            "page_fan_adds" 
                            (- now (* +24h-secs+ 9)) 
                            (- now (* +24h-secs+ 8))))
           (fb-fan-adds-8  (fb-insight-range-count 
                            "page_fan_adds" 
                            (- now (* +24h-secs+ 8)) 
                            (- now (* +24h-secs+ 7))))
           (fb-fan-adds-7 (fb-insight-range-count 
                           "page_fan_adds" 
                           (- now (* +24h-secs+ 7)) 
                           (- now (* +24h-secs+ 6))))
           (fb-fan-adds-6 (fb-insight-range-count 
                           "page_fan_adds" 
                           (- now (* +24h-secs+ 6)) 
                           (- now (* +24h-secs+ 5))))
           (fb-fan-adds-5 (fb-insight-range-count 
                           "page_fan_adds" 
                           (- now (* +24h-secs+ 5)) 
                           (- now (* +24h-secs+ 4))))
           (fb-fan-adds-4 (fb-insight-range-count 
                           "page_fan_adds" 
                           (- now (* +24h-secs+ 4)) 
                           (- now (* +24h-secs+ 3))))
           (fb-fan-adds-3 (fb-insight-range-count 
                           "page_fan_adds" 
                           (- now (* +24h-secs+ 3)) 
                           (- now (* +24h-secs+ 2))))
           (fb-fan-adds-2 (fb-insight-range-count 
                           "page_fan_adds" 
                           (- now (* +24h-secs+ 2)) 
                           (- now (* +24h-secs+ 1))))
           (fb-fan-adds-1 (fb-insight-range-count 
                           "page_fan_adds" 
                           (- now (* +24h-secs+ 1)) 
                           now))
           (fb-page-impressions-8 (fb-insight-range-count 
                                   "page_impressions" 
                                   (- now (* +24h-secs+ 8)) 
                                   (- now (* +24h-secs+ 7))))
           (fb-page-impressions-7 (fb-insight-range-count 
                                   "page_impressions" 
                                   (- now (* +24h-secs+ 7)) 
                                   (- now (* +24h-secs+ 6))))
           (fb-page-impressions-6 (fb-insight-range-count 
                                   "page_impressions" 
                                   (- now (* +24h-secs+ 6)) 
                                   (- now (* +24h-secs+ 5))))
           (fb-page-impressions-5 (fb-insight-range-count 
                                   "page_impressions" 
                                   (- now (* +24h-secs+ 5)) 
                                   (- now (* +24h-secs+ 4))))
           (fb-page-impressions-4  (fb-insight-range-count 
                                    "page_impressions" 
                                    (- now (* +24h-secs+ 4)) 
                                    (- now (* +24h-secs+ 3))))
           (fb-page-impressions-3 (fb-insight-range-count 
                                   "page_impressions" 
                                   (- now (* +24h-secs+ 3)) 
                                   (- now (* +24h-secs+ 2))))
           (fb-page-impressions-2 (fb-insight-range-count 
                                   "page_impressions" 
                                   (- now (* +24h-secs+ 2)) 
                                   (- now (* +24h-secs+ 1))))
           (fb-page-impressions-1 (fb-insight-range-count 
                                   "page_impressions" 
                                   (- now (* +24h-secs+ 1)) 
                                   now)))






      #|(break "~A ~A ~A ~A ~A ~A ~A"
      posts-scheduled-count
      fb-comments-count
      fb-likes-made-count
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
               ;; (str (format nil "Facebook insights recalculated up to ~A. We apologize for any inconvenience." (format-universal-date-time (facebook-last-insight))))
                
                
                
                (:div :class "container-fluid"
                      #|
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
                      "/dyb/generic"))
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
    |#
                      
                      (str (interval-selection))

                      (:div :class "row-fluid"
          (:div :class "nonboxy-widget"
                (:div :class "widget-head"
                      (:h5 (:i :class "black-icons month_calendar")
                           (str "Overview")))

                (:div :class "widget-content"
                      (:div :class "widget-box"

                            (if (string-equal (get-val (current-user) 'email) "admin@dyb.co.za")
                                (htm (str (format nil "fb-fans=~A | fb-page-impressions=~A | twit-followers=~A | tweets=~A | tweet-impressions=~A | twit-retweets=~A | linkedin-conn=~A "
                                                  (or-zero (list fb-fans-1 fb-fans-2 
                                                                 fb-fans-3 fb-fans-4 
                                                                 fb-fans-5 fb-fans-6 
                                                                 fb-fans-7 fb-fans-8 
                                                                 fb-fans-9))
                                                  (fb-insight-range-count 
                                                   "page_impressions" 
                                                   (- now (* +24h-secs+ 7)) 
                                                   now)
                                                  twitter-followers-count
                                            
                                                  (posts-scheduled-range-count 
                                                   (- now (* +24h-secs+ 7)) 
                                                   now
                                                   :post-type "Twitter")
                                                  (* twitter-followers-count
                                                     (posts-scheduled-range-count 
                                                      (- now (* +24h-secs+ 7)) 
                                                      now
                                                      :post-type "Twitter"))
                                            
                                                  twitter-retweets
                                                  linkedin-connections-count
                                                  ))))
                            (:div :class "row-fluid"
                            
                                  (str (let* ((prev-tweets (posts-scheduled-range-count 
                                                            (- now (* +24h-secs+ 14)) 
                                                            (- now (* +24h-secs+ 8))
                                                            :post-type "Twitter"))
                                              (tweets (posts-scheduled-range-count 
                                                       (- now (* +24h-secs+ 7)) 
                                                       now
                                                       :post-type "Twitter"))
                                              (prev (+ ;;fb-friends-count 
                                                     (or-zero (list fb-fans-8 fb-fans-9))
                                                     twitter-followers-count
                                                     prev-tweets
                                                     (* twitter-followers-count
                                                        prev-tweets)      
                                                     (fb-insight-range-count 
                                                      "page_impressions" 
                                                      (- now (* +24h-secs+ 14)) 
                                                      (- now (* +24h-secs+ 8)))
                                                     twitter-retweets-previous
                                                     linkedin-connections-count
                                                     (or-zero (list fb-page-impressions-8 
                                                                    fb-page-impressions-8))
                                                     ))
                                              (cur (+ ;;fb-friends-count 
                                                    (or-zero (list fb-fans-1 fb-fans-2 
                                                                   fb-fans-3 fb-fans-4 
                                                                   fb-fans-5 fb-fans-6 
                                                                   fb-fans-7 fb-fans-8 
                                                                   fb-fans-9))
                                                     
                                                    twitter-followers-count
                                                    tweets
                                                    (* twitter-followers-count
                                                       tweets)
                                                    (fb-insight-range-count 
                                                     "page_impressions" 
                                                     (- now (* +24h-secs+ 7)) 
                                                     now)
                                                    twitter-retweets
                                                    linkedin-connections-count
                                                     
                                                    )))
                                         (dash-small-stat-graph  
                                          "Reach"
                                          "new-visits"
                                          (format nil "~A,~A" 
                                                  prev
                                                  cur)  
                                          cur
                                          (calc-prev-cur-percentage prev cur))))
                                  (str (let ((prev (posts-scheduled-range-count 
                                                    previous-interval-start-date 
                                                    previous-interval-end-date))
                                             (cur (posts-scheduled-range-count 
                                                   interval-start-date 
                                                   interval-end-date)))
                                         (dash-small-stat-graph 
                                          "Activity"
                                          "unique-visits"
                                          (format nil "~A,~A" 
                                                  prev
                                                  cur)
                                          cur
                                          (calc-prev-cur-percentage prev cur))))
                                  (str (let ((prev (+ fb-fans-adds-previous-count
                                                      fb-comments-count 
                                                      ;;fb-likes-made-count
                                                      twitter-retweets))
                                             (cur (+ fb-fans-adds-count
                                                     fb-comments-count 
                                                     ;;fb-likes-made-count
                                                     twitter-retweets)))
                                         (dash-small-stat-graph 
                                          "Engagement"
                                          "weekly-sales"
                                          (format nil "~A,~A" 
                                                  prev
                                                  cur)
                                          cur
                                          (calc-prev-cur-percentage prev cur)))))
                            )
                      )))

                      (:div :class "row-fluid"
                            (:div :class "nonboxy-widget"
                                  (:div :class "widget-head"
                                        (:h5 (:i :class "black-icons month_calendar")
                                             (str "Network")))
                                  (break "~A" `((,@twitter-followers-interval-list)
                                                      ,(if fb-fans-interval-list
                                                          `(,@fb-fans-interval-list))))
                                  (:div :class "widget-content"
                                        (:div :class "widget-box"
                                              (str (network-size-graph  
                                                    (format-universal-date-dash interval-start-date)
                                                    (format-universal-date-dash interval-end-date)
                                                    (if (and twitter-followers-interval-list fb-fans-interval-list)
                                                        `((,@twitter-followers-interval-list)
                                                          (,@fb-fans-interval-list))
                                                        (if twitter-followers-interval-list
                                                            `((,@twitter-followers-interval-list))
                                                            (if fb-fans-interval-list
                                                                `((,@fb-fans-interval-list)) )))
                                                    
                                                    
                                                    ))
                                              )
                                        )))
                      (:div :class "row-fluid"
                            (:div :class "nonboxy-widget"
                                  (:div :class "widget-head"
                                        (:h5 (:i :class "black-icons month_calendar")
                                             (str "Engagement & Community")))

                                  (:div :class "widget-content"
                                        (:div :class "widget-box"
                                              (:div :class "row-fluid"
                                                    (str (engagement-graph `((("Likes" ,(or-zero 
                                                                                         (list fb-fan-adds-1 fb-fan-adds-2 fb-fan-adds-3 fb-fan-adds-4 fb-fan-adds-5 
                                                                                               fb-fan-adds-6 fb-fan-adds-7 fb-fan-adds-8 fb-fan-adds-9)))
                                                                              ("Clicks" ,(or-zero 
                                                                                          (list fb-page-impressions-1 fb-page-impressions-2 fb-page-impressions-3 fb-page-impressions-4 fb-page-impressions-5 
                                                                                                fb-page-impressions-6 fb-page-impressions-7 fb-page-impressions-8)))
                                                                              ("Comments" ,fb-comments-count)
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
                                                                        (+ ;;fb-friends-count
                                                                         (or-zero (list fb-fans-1 fb-fans-2 fb-fans-3 fb-fans-4 fb-fans-5 fb-fans-6 fb-fans-7 fb-fans-8 fb-fans-9))
                                                                         ;;fb-fans-count
                                                                         twitter-followers-count
                                                                         ;;linkedin-connections-count
                                                                         ;;TODO: linkeded in followers
                                                                         )
                                                                        "/appimg/user-accounts.png"
                                                                        "All Accounts"
                                                                        nil
                                                                        )))
                                                                 (:li
                                                                  (str (community-summary-item  
                                                                        " Facebook"
                                                                        (or-zero (list fb-fans-1 fb-fans-2 fb-fans-3 fb-fans-4 fb-fans-5 fb-fans-6 fb-fans-7 fb-fans-8 fb-fans-9))
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
                                              )
                                        )

                                 ))

                   (:div :class "row-fluid"
                            (:div :class "nonboxy-widget"
                                  (:div :class "widget-head"
                                        (:h5 (:i :class "black-icons facebook")
                                             "FACEBOOK (Last 7 Days)"))

                                  (:div :class "widget-content"
                                        (:div :class "widget-box"
                                              (:div :class "row-fluid"
                            
                            (str (board-stats  (format nil "~A,~A,~A,~A,~A,~A,~A" 
                                                      fb-fan-adds-8
                                                      fb-fan-adds-7
                                                      fb-fan-adds-6
                                                      fb-fan-adds-5
                                                      fb-fan-adds-4
                                                      fb-fan-adds-3
                                                      fb-fan-adds-2
                                                     
                                                      )
                                              "New Likes" 
                                              (list "facebook_like") 
                                              "bar-chart" "span3"))
                            (str (board-stats (format nil "~A,~A,~A,~A,~A,~A,~A" 
                                                      fb-page-impressions-8
                                                      fb-page-impressions-7
                                                      fb-page-impressions-6
                                                      fb-page-impressions-5
                                                      fb-page-impressions-4
                                                      fb-page-impressions-3
                                                      fb-page-impressions-2)
                                              "Page Impressions" 
                                              (list "documents")
                                              "bar-chart" "span3"))
                            (str (board-stats 
                                              (format nil "~A,~A,~A,~A,~A,~A,~A" 
                                                      fb-fans-8
                                                      fb-fans-7
                                                      fb-fans-6
                                                      fb-fans-5
                                                      fb-fans-4
                                                      fb-fans-3
                                                      fb-fans-2
                                                      
                                                      )
                                              "Total Fans" 
                                              (list "users")
                                              "bar-chart" "span3"))
                            #|(str (board-stats "0,0"
                            "Demographics" 
                            (list "male_contour" "female_contour")
                            "pie-chart" "span2"))|#
                            )))))
                   
                (:div :class "row-fluid"
                            (:div :class "nonboxy-widget"
                                  (:div :class "widget-head"
                                        (:h5 (:i :class "black-icons twitter")
                                             "Twitter (Last 7 Days)"))

                                  (:div :class "widget-content"
                                        (:div :class "widget-box"
                                              (:div :class "row-fluid"
                            
                            (str (board-stats 
                                   (format nil "~A,~A,~A,~A,~A,~A,~A" 
                                           (- twit-fans-7 twit-fans-8)
                                           (- twit-fans-6 twit-fans-7)
                                           (- twit-fans-5 twit-fans-6 )
                                           (- twit-fans-4 twit-fans-5 )
                                           (- twit-fans-3 twit-fans-4 )
                                           (- twit-fans-2 twit-fans-3 )
                                           (- twit-fans-1 twit-fans-2 ))
                                  
                                  
                                              "New Followers" 
                                              (list "users") 
                                              "bar-chart" "span3"))
                            (str (board-stats (format nil "~A,~A,~A,~A,~A,~A,~A" 
                                                      
                                                      twit-retweets-7
                                                      twit-retweets-6
                                                      twit-retweets-5
                                                      twit-retweets-4
                                                      twit-retweets-3
                                                      twit-retweets-2
                                                      twit-retweets-1
                                                      )
                                              "Impressions" 
                                              (list "documents")
                                              "bar-chart" "span3"))
                            (str (board-stats (format nil "~A,~A,~A,~A,~A,~A,~A" 
                                                     
                                                      twit-fans-7
                                                      twit-fans-6
                                                      twit-fans-5
                                                      twit-fans-4
                                                      twit-fans-3
                                                      twit-fans-2
                                                      twit-fans-1
                                                      )
                                              "Total Fans" 
                                              (list "users")
                                              "bar-chart" "span3"))
                            #|(str (board-stats "0,0"
                            "Demographics" 
                            (list "male_contour" "female_contour")
                            "pie-chart" "span2"))|#
                            )))))
                      
                      
                      
                      
                      
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
    |#)
                
                )
))))

  )

