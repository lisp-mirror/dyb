(in-package :dyb)

(defun universal-today ()
  (multiple-value-bind (sec min hour day month year)
          (decode-universal-time           
           (get-universal-time)
           (time-zone))
    (declare (ignore sec min hour))
    (encode-universal-time 0 0 0 day month year (time-zone))))

(defun universal-date-strip-time (date)
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
        (unless (= (second val) 0)
          (setf value (second val)))))
    value))

(defun retweets-of-my-tweets ()
  (find-docs 'list
                (lambda (doc)
                  (typecase (get-val doc 'channel-user) 
                    (channel-user 
                     (when (and
                            (gpv (get-val doc 'payload) :retweeted)
                            (string-equal 
                             (format nil "~A" 
                                     (get-val (get-val doc 'channel-user) 'user-id))
                             (gpv (get-val doc 'payload) :user :id--str)))
                       (break "~A" doc)))))
                (generic-post-collection)))

#|
(find-docs 'list
                (lambda (doc)
                  (typecase (get-val doc 'channel-user) 
                    (channel-user 
                     (when (and
                            (search "#Algorith" (gpv (get-val doc 'payload) :text))
                            )
                       doc))))
                (generic-post-collection))
|#

(defun posts-scheduled-range-count (start-date end-date &key post-type)
  (let ((count 0))
    (find-docs 
     'vector 
     (lambda (doc)
       (when (match-context-entities (get-val doc 'channel-user))
         (when (and (>= (get-val doc 'scheduled-date) start-date)
                    (<= (get-val doc 'scheduled-date) end-date))
           (when (string-equal (get-val doc 'action-status) "completed")
            
             (if post-type
                 (when (string-equal post-type (get-val doc 'post-type))
                   (incf count 1))
                 (incf count 1))))))
     (generic-actions-collection))
    count))

(defun posts-scheduled-range (start-date end-date &key post-type)
  (let ((range)
        (sorted-range)
        (final-range)
        (data-hash (make-hash-table :test 'equal)))
    (find-docs 
     'vector 
     (lambda (doc)
       (when (match-context-entities (get-val doc 'channel-user))
         (when (and (>= (get-val doc 'scheduled-date) start-date)
                    (<= (get-val doc 'scheduled-date) end-date))
           (when (string-equal (get-val doc 'action-status) "completed")
               (when (string-equal post-type (get-val doc 'post-type))
                 (let ((current-val (gethash (universal-date-strip-time
                                              (get-val doc 'scheduled-date))
                                             data-hash))
                       (val 0))
                   (if current-val
                     (setf val current-val))
                   (setf val (incf val))
                   (setf (gethash (universal-date-strip-time
                                   (get-val doc 'scheduled-date))
                                  data-hash)
                         val)))))))
     (generic-actions-collection))
    (maphash  
     (lambda (key val)
       (setf range
             (append range 
                     (list (list key val)))))
     data-hash)
    (setf sorted-range (sort range #'> :key #'car))
    (dolist (post sorted-range)
      (setf final-range 
            (append final-range 
                    (list 
                     (list (format-universal-date-dash (first post))
                           (second post))))))
    final-range))



(defun insight-count (channel insight-name &key target-date)
  (let ((count 0)
        (the-day (if target-date
                     target-date
                     (universal-today))))
    (dolist (user (coerce (channel-users) 'list ))
      (when (valid-channel-user user channel)  
        ;;Need to make adjustments for dates because insights that are behind
        (let* ((start-date (if (equal the-day (universal-today))
                              (- the-day  (* 60 60 24 4))
                              (- the-day  (* 60 60 24 1))))
              (end-date (if (equal the-day (universal-today))
                              (- the-day  (* 60 60 24 3))
                              the-day))
              (fans
               (get-generic-insight-values 
                user 
                insight-name
                start-date
                end-date)))
          (when fans
            (dolist (fan fans)
              (when (get-val fan 'value)
                ;;TODO: Do we need to check for 0 incase 3 days is not enough?
                (setf count (get-val fan 'value))))))))
    count))


(defun insight-range (channel insight-name start-date end-date)
  (let ((range))
    (dolist (user (coerce (channel-users) 'list ))
      (when (valid-channel-user user channel)  
        (let ((insights
                    (get-generic-insight-values 
                     user 
                     insight-name
                     start-date 
                     end-date)))
               (when insights
                 (dolist (insight insights)
                   (when (get-val insight 'value)
                     (setf range 
                           (append range 
                                   (list 
                                    (list 
                                     (format-universal-date-dash (get-val insight 'end-time)) 
                                     (get-val insight 'value)))))))))))
    range))



(defun insight-range-count (channel insight-name start-date end-date)
  (let ((count 0))
    (dolist (user (coerce (channel-users) 'list ))
      (when (valid-channel-user user channel)  
        (let ((fans
               (get-generic-insight-values 
                user 
                insight-name
                start-date 
                end-date)))
          (when fans
            (dolist (fan fans)
              (when (get-val fan 'value)
                (incf count (get-val fan 'value))))))))
    count))


(defun compound-insight-count (channel insight-name keys &key target-date)
  (let ((count 0)
        (the-day (if target-date
                     target-date
                     (universal-today))))
    (dolist (user (coerce (channel-users) 'list ))
      (when (valid-channel-user user channel)  
        ;;Need to make adjustments for dates because insights that are behind
        (let* ((start-date (if (equal the-day (universal-today))
                              (- the-day  (* 60 60 24 4))
                              (- the-day  (* 60 60 24 1))
                              ))
              (end-date (if (equal the-day (universal-today))
                              (- the-day  (* 60 60 24 3))
                              the-day))
              (insights
               (get-generic-insight-values 
                user 
                insight-name
                start-date
                end-date)))
          (when insights
            (dolist (insight insights)
              (when (get-val insight 'value)
                ;;TODO: Do we need to check for 0 incase 3 days is not enough?
                (setf count (apply #'gpv
                                        (get-val insight 'value)
                                        (if (listp keys)
                                            keys
                                            (list keys))))))))))
    count))

(defun compound-insight-range (channel insight-name start-date end-date keys)
  (let ((range))
    (dolist (user (coerce (channel-users) 'list ))
      (when (valid-channel-user user channel)  
        (let ((insights
                    (get-generic-insight-values 
                     user 
                     insight-name
                     start-date 
                     end-date)))
               (when insights
                 (dolist (insight insights)
                   (when (get-val insight 'value)
                     (setf range 
                           (append range 
                                   (list 
                                    (list 
                                     (format-universal-date-dash (get-val insight 'end-time)) 
                                     (apply #'gpv
                                        (get-val insight 'value)
                                        (if (listp keys)
                                            keys
                                            (list keys)))))))))))))
    range))

(defun compound-insight-range-count (channel insight start-date end-date keys)
  (let ((count 0))
    (dolist (channel-user (coerce (channel-users) 'list ))
      (when (valid-channel-user channel-user channel)
        (find-docs   
         'list
         (lambda (doc)
                 (let ((date (universal-date-strip-time 
                               (get-val doc 'end-time))))
                   (when (and 
                          (equal (id channel-user) (id (get-val doc 'channel-user)))
                          (string-equal (get-val doc 'insight) 
                                        insight)
                          (and (>= date 
                                   start-date)
                               (<= date 
                                   end-date)))
                     
                     (incf count (apply #'gpv
                                        (get-val doc 'value)
                                        (if (listp keys)
                                            keys
                                            (list keys)))))))
         (generic-insight-value-collection))))
    count))

(defun compound-insight-range-last-val (channel insight start-date end-date keys)
  (let ((count 0))
    (dolist (channel-user (coerce (channel-users) 'list ))
      (when (valid-channel-user channel-user channel)
        (find-docs   
         'list
         (lambda (doc)
                 (let ((date (universal-date-strip-time 
                               (get-val doc 'end-time))))
                   (when (and 
                          (equal (id channel-user) (id (get-val doc 'channel-user)))
                          (string-equal (get-val doc 'insight) 
                                        insight)
                          (and (>= date 
                                   start-date)
                               (<= date 
                                   end-date)))
                     
                     (setf count (apply #'gpv
                                        (get-val doc 'value)
                                        (if (listp keys)
                                            keys
                                            (list keys)))))))
         (generic-insight-value-collection))))
    count))


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


(defun fb-post-likes (start-date end-date)
  (let ((count 0))
    (loop for post across (generic-posts) 
       when (match-context-entities (channel-user post) )
       when (string-equal (get-val post 'post-type) 
                          "Facebook")
       when (get-val post 'payload)
       when (gpv (get-val post 'payload) :likes :count)
       when  (and (<= start-date 
                      (parse-facebook-created-at 
                       (gpv (get-val post 'payload) :created--time))) 
                  (>= end-date 
                      (parse-facebook-created-at 
                       (gpv (get-val post 'payload) :created--time))))
       do (incf count (gpv (get-val post 'payload) :likes :count)))
    count))


(defun fb-comment-dates-count (payload start-date end-date)
  (let ((count 0))   
    (dolist (comment  (gpv payload :comments :data))
      (when (and (<= start-date 
                     (parse-facebook-created-at 
                      (gpv comment :created--time))) 
                 (>= end-date 
                     (parse-facebook-created-at 
                      (gpv comment :created--time))))
        (incf count)))
    count))

(defun fb-comments-made (start-date end-date)
  (let ((count 0))
    (loop for post across (generic-posts) 
       when (match-context-entities (channel-user post) )
       when (string-equal (get-val post 'post-type) 
                          "Facebook")
       do (incf count (fb-comment-dates-count 
                        
                       (get-val post 'payload)
                       start-date
                       end-date)))
    count))


(defun twitter-at-mentions (start-date end-date)
  (let ((count 0))
    (loop for post across (generic-posts) 
       when (match-context-entities (channel-user post) )
       when (string-equal (get-val post 'post-type) "Twitter")
       when (and (<= start-date (get-val post 'created-date)) 
                 (>= end-date (get-val post 'created-date)))
       when (string-equal (get-val post 'payload-type) "mentions-timeline")
       do (incf count))
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
                                             (str (format nil "~A%" (truncate total-percent) ))))
                                 (htm (:span :class "label-red" 
                                             (str (format nil "~A%" (truncate (* total-percent -1)) ))))))))))))

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

(defun network-size-graph (min-date max-date data interval)
  
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
                                      :tick-interval (if (<= interval 7)
                                                         "1 days"
                                                         (if (and (> interval 7) 
                                                                  (< interval 100))
                                                             "7 days"
                                                             "30 days"))
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

(defun engagement-graph (data data-lables)
  (with-html-string
    (:div :class "span6"
          (:div :class "graph-wrap"
                (:div :class "chart-block"
                      (let ((engagement 
                             (make-widget
                              'line-graph :name "engagementgraph"
                              :data data
                              :data-lables data-lables)))
                        (setf (get-val engagement 'data-lables) data-lables)
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



(defun calc-prev-cur-percentage (prev cur)
  (if (and (= prev 0) (= cur 0))
      0
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
              -100))))

(defun interval-selection ()
  (with-html-to-string ()
    (:div :class "row-fluid"
          (:div :class "nonboxy-widget"
                (:div :class "widget-head"
                      (:h5 (:i :class "black-icons month_calendar")
                           (str "Dashboard Interval")))

                (:div :class "widget-content"
                      (:form :name "dash-date-form" :action ""  :method "post"
                                 
                                   
                             (:div :class "widget-box"
                                   (:div :class "row-fluid"
                                         (:div :class "span2"
                                               (:input :type :radio 
                                                       :id "shit"
                                                       :name "dashboard-interval" 
                                                       :value "7 Days"
                                                       :checked (string-equal (parameter "dashboard-interval") "7 Days")
                                                       (str "7 Days")))
                                         (:div :class "span2"
                                               (:input :type :radio 
                                                       :name "dashboard-interval" 
                                                       :value "30 Days"
                                                       :checked (or (string-equal (parameter "dashboard-interval") "30 Days")
                                                                    (not (parameter "dashboard-interval")))
                                                       (str "30 Days")))
                                         (:div :class "span2"
                                               (:input :type :radio 
                                                       :name "dashboard-interval" 
                                                       :value "365 Days"
                                                       :checked (string-equal (parameter "dashboard-interval") "365 Days")
                                                       (str "365 Days"))))
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
                                   )
                             (:div :class "widget-bottom"
                                   (:input :type "submit" 
                                    
                                           :class "btn btn-info" 
                                           :name "set-dash-interval" 
                                           :value "Set Interval"))))))))

(defun dashboard-overview ()
  (with-html-to-string ()
    ))

(defconstant +24h-secs+ (* 60 60 24))

(defun calc-date-interval ()
  (let* ((today (universal-today))
        (interval 30)
        (start-date (- today (* +24h-secs+ interval)))
        (end-date today))
    
    (when (and (not (empty-p (parameter "interval-start-date")))
               (not (empty-p (parameter "interval-end-date"))))

      (setf start-date (string-to-date (parameter "interval-start-date")))

      (setf end-date (string-to-date (parameter "interval-end-date")))

      (unless (> end-date start-date)
        (setf start-date (- today (* +24h-secs+ interval)))
        (setf end-date today)
        )
      (when (> end-date start-date)
        
        
        (setf interval (truncate (/ (- end-date start-date)
                                    +24h-secs+)))))
    
    (unless (and (not (empty-p (parameter "interval-start-date")))
                 (not (empty-p (parameter "interval-end-date")))
                 )

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

(defun strip-dates-from-range (range interval)
  (let ((new-range)
        (count 1))
    (dolist (item range)
          (when (<= count interval)
            (setf new-range (append new-range (list (second item))))
            )
          (incf count)
          )
    new-range))

(defun create-bar-range-string (range interval)
  (let ((stripped (strip-dates-from-range (reverse range) interval))
        (range-string))
    (dolist (val (reverse stripped))
      (if (empty-p range-string)
          (setf range-string (format nil "~A" val))
          (setf range-string (format nil "~A,~A" range-string val))))
    range-string))

(defun fix-nan (value)
  (if value
      value
      0))

(defun set-calc-vals (istd iend
                      prev-istd 
                      prev-iend)
  (let ((calc-values (make-hash-table :test 'equal)))
    (flet ((sv (key val)
             (setf (gethash key calc-values) val)))

      (sv 'posts-scheduled-count 
          (posts-scheduled-range-count 
           istd 
           iend))

      (sv 'posts-scheduled-prev-count 
          (posts-scheduled-range-count 
           prev-istd 
           prev-iend))

      (sv 'fb-comments-made-count 
          (fb-comments-made 
           istd 
           iend))

      (sv 'fb-comments-made-prev-count 
          (fb-comments-made 
           prev-istd 
           prev-iend))

      (sv 'fb-fans-adds-count 
          (insight-range-count 
           "Facebook"
           "page_fan_adds" 
           istd 
           iend))
               
      (sv 'fb-fans-adds-prev-count 
          (insight-range-count
           "Facebook"
           "page_fan_adds" 
           prev-istd 
           prev-iend))
         
      (sv 'fb-story-adds-count 
          (insight-range-count 
           "Facebook"
           "page_story_adds" 
           istd 
           iend))

      (sv 'fb-story-adds-prev-count 
          (insight-range-count
           "Facebook"
           "page_story_adds" 
           prev-istd 
           prev-iend))

      (sv 'fb-fans-interval-list
          (insight-range
           "Facebook"
           "page_fans" 
           istd 
           iend))
           
      (sv 'fb-fans-interval-prev-list
          (insight-range
           "Facebook"
           "page_fans" 
           prev-istd 
           prev-iend))
           
      (sv 'fb-fans-adds-interval-list
          (insight-range
           "Facebook"
           "page_fan_adds" 
           istd 
           iend))

      (sv 'fb-fans-adds-interval-prev-list
          (insight-range
           "Facebook"
           "page_fan_adds" 
           prev-istd 
           prev-iend))

      (sv 'fb-fans-removes-interval-list
          (insight-range
           "Facebook"
           "page_fan_removes" 
           istd 
           iend))

      (sv 'fb-fans-removes-interval-prev-list
          (insight-range
           "Facebook"
           "page_fan_removes" 
           prev-istd 
           prev-iend))

      (sv 'fb-impressions-interval-list
          (insight-range
           "Facebook"
           "page_impressions" 
           istd 
           iend))

      (sv 'fb-page-impressions-count
          (insight-range-count 
           "Facebook"
           "page_impressions" 
           istd 
           iend))

      (sv 'fb-page-impressions-prev-count
          (insight-range-count 
           "Facebook"
           "page_impressions" 
           prev-istd 
           prev-iend))

      (sv 'fb-post-likes-count
          (fb-post-likes
           istd 
           iend ))

      (sv 'fb-post-likes-prev-count
          (fb-post-likes
           prev-istd 
           prev-iend ))


      (sv 'twitter-retweets-list 
          (compound-insight-range 
           "Twitter" "twitter-profile" 
           istd 
           iend 
           (list :status :retweet--count)))

      (sv 'twitter-retweets 
          (compound-insight-range-count 
           "Twitter" "twitter-profile"
           istd 
           iend
           (list :status :retweet--count))
          )

      (sv 'twitter-retweets-prev 
          (compound-insight-range-count 
           "Twitter" "twitter-profile"
           prev-istd 
           prev-iend
           (list :status :retweet--count))
          )

      (sv 'twitter-followers-count 
          (compound-insight-range-last-val 
           "Twitter" "twitter-profile"  
           istd 
           iend
           :followers--count))

      (sv 'twitter-followers-prev-count 
          (compound-insight-range-last-val 
           "Twitter" "twitter-profile"  
           prev-istd 
           prev-iend
           :followers--count))

      (sv 'twitter-followers-interval-list
          (compound-insight-range 
           "Twitter" "twitter-profile" 
           istd 
           iend
           :followers--count))

      (sv 'twitter-followers-interval-prev-list 
          (compound-insight-range 
           "Twitter" "twitter-profile" 
           prev-istd 
           prev-iend
           :followers--count))
           
      (sv 'twitter-at-mentions-count
          (twitter-at-mentions 
           istd 
           iend ))

      (sv 'twitter-at-mentions-prev-count
          (twitter-at-mentions 
           prev-istd 
           prev-iend))

      (sv 'tweets-scheduled-count
          (posts-scheduled-range-count 
           istd 
           iend
           :post-type "Twitter"))

      (sv 'tweets-scheduled-prev-count
          (posts-scheduled-range-count 
           prev-istd 
           prev-iend
           :post-type "Twitter"))
           
      (sv 'tweets-scheduled-list
          (posts-scheduled-range 
           istd 
           iend
           :post-type "Twitter"))
      (sv 'tweets-sheduled-prev-list
          (posts-scheduled-range 
           prev-istd 
           prev-iend
           :post-type "Twitter"))
          
      )
    calc-values))



(define-easy-handler (dashboard-page :uri "/dyb/dashboard") ()
  (multiple-value-bind (interval interval-start-date interval-end-date)
      (calc-date-interval)
    (let* (
           
               (page (make-widget 'page :name "dashboard-page"))
               (now (universal-today))
               (previous-interval-start-date 
                (- now (* +24h-secs+ (* interval 2))))
               (previous-interval-end-date 
                (- now (* +24h-secs+ interval)))

               (calc-values (set-calc-vals 
                             interval-start-date 
                             interval-end-date
                             previous-interval-start-date 
                             previous-interval-end-date))


           
           (linkedin-connections-count (linkedin-connections-count))
           )

      (flet ((gv (key)
                (gethash key   calc-values)))

        (with-html
      
          (render page
                  :body 
                  (with-html-to-string ()
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

                            
                            (:div :class "row-fluid"
                            
                                  (str (let* ((prev (+  
                                                     (or-zero (reverse (gv '(gv 'fb-fans-interval-prev-list))))
                                                     (gv 'fb-page-impressions-prev-count)
                                                     (gv 'twitter-followers-prev-count)
                                                     (* (gv 'twitter-followers-prev-count)
                                                        (gv 'tweets-scheduled-prev-count))      
                                                     (gv 'twitter-retweets-prev)
                                                     (gv 'twitter-at-mentions-prev-count)
                                                     linkedin-connections-count))
                                              (cur (+  
                                                    (or-zero (reverse (gv 'fb-fans-interval-list)))
                                                    (gv 'fb-page-impressions-count)
                                                    (gv 'twitter-followers-count)
                                                    (* (gv 'twitter-followers-count)
                                                       (gv 'tweets-scheduled-count))
                                                    (gv 'twitter-retweets)
                                                    (gv 'twitter-at-mentions-count)
                                                    linkedin-connections-count)))
                                         (dash-small-stat-graph  
                                          "Reach"
                                          "new-visits"
                                          (format nil "~A,~A" 
                                                  prev
                                                  cur)  
                                          cur
                                          (calc-prev-cur-percentage prev cur) )))
                                  (str (dash-small-stat-graph 
                                          "Activity"
                                          "unique-visits"
                                          (format nil "~A,~A" 
                                                  (gv 'posts-scheduled-prev-count)
                                                  (gv 'posts-scheduled-count))
                                          (gv 'posts-scheduled-count)
                                          (calc-prev-cur-percentage 
                                           (gv 'posts-scheduled-prev-count) 
                                           (gv 'posts-scheduled-count))))
                                  (str (let ((prev (+ (gv 'fb-fans-adds-prev-count)
                                                    
                                                      (gv 'fb-comments-made-prev-count)
                                                      ;;(- (gv 'fb-story-adds-count) 
                                                      ;;   (gv 'fb-fans-adds-count)
                                                      ;;   (gv 'fb-comments-made-count))
                                                      (gv 'twitter-retweets-prev)
                                                      (gv 'twitter-at-mentions-prev-count)))
                                             (cur (+ (gv 'fb-fans-adds-count)
                                                    
                                                     (gv 'fb-comments-made-count)
                                                     ;;(- (gv 'fb-story-adds-prev-count) 
                                                     ;;   (gv 'fb-fans-adds-prev-count)
                                                     ;;   (gv 'fb-comments-made-prev-count))
                                                     (gv 'twitter-retweets)
                                                     (gv 'twitter-at-mentions-count))))
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
                                  
                                  (:div :class "widget-content"
                                        (:div :class "widget-box"
                                              (str (network-size-graph  
                                                    (format-universal-date-dash interval-start-date)
                                                    (format-universal-date-dash interval-end-date)
                                                    (if (and (gv 'twitter-followers-interval-list) (gv 'fb-fans-interval-list))
                                                        `((,@(gv 'twitter-followers-interval-list))
                                                          (,@(gv 'fb-fans-interval-list)))
                                                        (if (gv 'twitter-followers-interval-list)
                                                            `((,@(gv 'twitter-followers-interval-list)))
                                                            (if (gv 'fb-fans-interval-list)
                                                                `((,@(gv 'fb-fans-interval-list))) )))
                                                      
                                                    

                                                     interval))
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
                                                    (str (engagement-graph `((("Post Likes" ,(gv 'fb-post-likes-count))
                                                                             ;; ("Clicks" ,(gv 'fb-page-impressions-count))
                                                                              ("Comments" ,(gv 'fb-comments-made-count))
                                                                              ("Retweets" ,(gv 'twitter-retweets))
                                                   
                                                                              ;;("FB Shares" ,(gv 'fb-story-adds-count))
                                                                              ("Mentions" ,(gv 'twitter-at-mentions-count))
                                                                              ("Direct Messages" 0)))
                                                                           (format nil "[~A,~A,~A,~A]" 
                                                                                   (gv 'fb-post-likes-count) 
                                                                                   (gv 'fb-comments-made-count) 
                                                                                   (gv 'twitter-retweets)
                                                                                   (gv 'twitter-at-mentions-count))))
                                                    (:div :class "span2"
                                                          (:div :class "summary"
                                                                (:h4 "CURRENT COMMUNITY SIZE")
                                                                (:br)
                                                                (:ul
                                                                 (:li
                                                                  (str (community-summary-item  
                                                                        "All Accounts"
                                                                        (+ 
                                                                         (or-zero (reverse (gv 'fb-fans-interval-list)) )
                                                                         ;;fb-fans-count
                                                                         (gv 'twitter-followers-count)
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
                                                                        (or-zero (reverse (gv 'fb-fans-interval-list)) )
                                                                        "/appimg/Facebook_Light_Logo.png"
                                                                        "Facebook Friends"
                                                                        t
                                                                        ))
                                          
                                                                  )
                                                                 (:li
                                                                  (str (community-summary-item  
                                                                        " Twitter"
                                                                        (gv 'twitter-followers-count)
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
                            
                            (str (board-stats  (create-bar-range-string 
                                                (gv 'fb-fans-adds-interval-list) 7) 
                                              "New Page Likes" 
                                              (list "facebook_like") 
                                              "bar-chart" "span3"))
                            (str (board-stats (create-bar-range-string 
                                               (gv 'fb-impressions-interval-list) 7)
                                              "Page Impressions" 
                                              (list "documents")
                                              "bar-chart" "span3"))
                            (str (board-stats 
                                              (create-bar-range-string 
                                                (gv 'fb-fans-interval-list) 7)
                                              "Total Fans" 
                                              (list "users")
                                              "bar-chart" "span3"))
                            (str (board-stats  (create-bar-range-string 
                                                (gv 'fb-fans-removes-interval-list) 7) 
                                              "Page Unlikes" 
                                              (list "facebook_unlike") 
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
                            
                            (str 
                             (let ((new-followers (strip-dates-from-range 
                                                   (reverse (gv 'twitter-followers-interval-list)) 8)))
                               
                               (board-stats 
                                (format nil "~A,~A,~A,~A,~A,~A,~A" 
                                        (- (fix-nan (nth 6 new-followers)) (fix-nan (nth 7 new-followers)))
                                        (- (fix-nan (nth 5 new-followers)) (fix-nan (nth 6 new-followers)))
                                        (- (fix-nan (nth 4 new-followers)) (fix-nan (nth 5 new-followers)))
                                        (- (fix-nan (nth 3 new-followers)) (fix-nan (nth 4 new-followers)))
                                        (- (fix-nan (nth 2 new-followers)) (fix-nan (nth 3 new-followers)))
                                        (- (fix-nan (nth 1 new-followers)) (fix-nan (nth 2 new-followers)))
                                        (- (fix-nan (nth 0 new-followers)) (fix-nan (nth 1 new-followers))))
                                  
                                  
                                "New Followers" 
                                (list "users") 
                                "bar-chart" "span3")))
                            (str 
                             (let ((followers (strip-dates-from-range 
                                                 (reverse (gv 'twitter-followers-interval-list)) 7))
                                    (tweets (strip-dates-from-range 
                                             (gv 'tweets-scheduled-list) 7)))
                                (board-stats  
                               
                                 (format nil "~A,~A,~A,~A,~A,~A,~A" 
                                         (* (fix-nan (nth 6  followers) ) (fix-nan (nth 6 tweets)))
                                         (* (fix-nan (nth 5  followers) ) (fix-nan (nth 5 tweets)))
                                         (* (fix-nan (nth 4  followers) ) (fix-nan (nth 4 tweets)))
                                         (* (fix-nan (nth 3  followers) ) (fix-nan (nth 3 tweets)))
                                         (* (fix-nan (nth 2  followers) ) (fix-nan (nth 2 tweets)))
                                         (* (fix-nan (nth 1  followers) ) (fix-nan (nth 1 tweets)))
                                         (* (fix-nan (nth 0  followers) ) (fix-nan (nth 0 tweets)))
                                        )
                                 "Impressions" 
                                 (list "documents")
                                 "bar-chart" "span3")))
                            
                            (str
                             
                             (board-stats (create-bar-range-string 
                                                (gv 'twitter-followers-interval-list) 7)
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
                    (:div :class "row-fluid"
                            (:div :class "nonboxy-widget"
                            
                                  (when (string-equal (get-val (current-user) 'email) "admin@dyb.co.za")
                                      (htm 
                                        (:table 
                                         (:tr 
                                          (:th  :style "border: 1px solid black;" "Key")
                                          (:th  :style "border: 1px solid black;" "Value"))
                                         (maphash (lambda (key val)
                                                    (htm (:tr
                                                          (:td :style "border: 1px solid black; width: 250px;" (str key))
                                                          (:td :style "border: 1px solid black;" (str val)))))
                                                  calc-values))
                                        ))))
)
)))))

  )

