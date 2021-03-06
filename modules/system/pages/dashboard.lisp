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

(defun short-url-clicks (start-date end-date)
  (let ((count 0))
    (find-docs 'list
               (lambda (doc)
                 (typecase (get-val doc 'channel-user) 
                   (channel-user
                    (when (string-equal (get-val doc 'post-type) "Twitter")
                      (when (and
                             (match-context-entities (get-val doc 'channel-user))
                             (gpv (get-val doc 'payload) :entities :urls)                  
                             (and (>= (get-val doc 'created-date) start-date)
                                  (<= (get-val doc 'created-date) end-date)))
                        (dolist (url (gpv (get-val doc 'payload) :entities :urls))
                          (let ((short-url 
                                  (expand-short-url (last (split-string url #\/)))))
                            (when short-url
                              (incf count (or (click-count short-url) 0))))))))))
               (generic-post-collection))
    count))

(defun retweets-of-my-tweets (start-date end-date)
  (let ((count 0))
    (find-docs 'list
               (lambda (doc)
                 (typecase (get-val doc 'channel-user) 
                   (channel-user
                    (when (string-equal (get-val doc 'post-type) "Twitter")
                      (when (and
                             (match-context-entities (get-val doc 'channel-user))
                             (> (or (gpv (get-val doc 'payload) :retweet--count) 0) 0)
                             (string-equal 
                              (get-val (get-val doc 'channel-user) 'channel-user-name) 
                              (gpv (get-val doc 'payload) :user :screen--name))
                             (and (>= (get-val doc 'created-date) start-date)
                                  (<= (get-val doc 'created-date) end-date)))
                        (incf count (or (gpv (get-val doc 'payload) :retweet--count) 0))
                        doc)))))
               (generic-post-collection))
    count))

(defun mentions-of-me-in-tweets (start-date end-date)
  (let ((count 0))
    (find-docs 'list
               (lambda (doc)
                 (typecase (get-val doc 'channel-user) 
                   (channel-user
                    (when (string-equal (get-val doc 'post-type) "Twitter")
                      (when (and
                             (match-context-entities (get-val doc 'channel-user))
                             (gpv (get-val doc 'payload) :entities :user--mentions)
                             
                             (and (>= (get-val doc 'created-date) start-date)
                                  (<= (get-val doc 'created-date) end-date)))
                        (dolist (mention (gpv (get-val doc 'payload) :entities :user--mentions))
                         (when (string-equal
                                (get-val (get-val doc 'channel-user) 'channel-user-name)
                                (gpv mention :screen--name))
                           ;(break "~A" (get-val doc 'payload))
                           (incf count)))
                        )))))
               (generic-post-collection))
    count))


(defun mentions-of-me-in-tweets-mentioner-followers (start-date end-date)
  (let ((count 0))
    (find-docs 'list
               (lambda (doc)
                 (typecase (get-val doc 'channel-user) 
                   (channel-user
                    (when (string-equal (get-val doc 'post-type) "Twitter")
                      (when (and
                             (match-context-entities (get-val doc 'channel-user))
                             (gpv (get-val doc 'payload) :entities :user--mentions)
                             
                             (and (>= (get-val doc 'created-date) start-date)
                                  (<= (get-val doc 'created-date) end-date)))
                        (dolist (mention (gpv (get-val doc 'payload) :entities :user--mentions))
                         (when (string-equal
                                (get-val (get-val doc 'channel-user) 'channel-user-name)
                                (gpv mention :screen--name))
                           (incf count (gpv (get-val doc 'payload) :user :followers--count))))
                        )))))
               (generic-post-collection))
    count))

(defun mentions-of-me-in-tweets-mentioner-followers-range (start-date end-date)
  (let (range
        (data-hash (make-hash-table :test 'equal)))
    (find-docs 'list
               (lambda (doc)
                 (typecase (get-val doc 'channel-user) 
                   (channel-user
                    (when (string-equal (get-val doc 'post-type) "Twitter")
                      (when (and
                             (match-context-entities (get-val doc 'channel-user))
                             (gpv (get-val doc 'payload) :entities :user--mentions)
                             (and (>= (get-val doc 'created-date) start-date)
                                  (<= (get-val doc 'created-date) end-date)))
                        (dolist (mention (gpv (get-val doc 'payload) :entities :user--mentions))
                          (when (string-equal
                                 (get-val (get-val doc 'channel-user) 'channel-user-name)
                                 (gpv mention :screen--name))
                            (let ((current-val (gethash (universal-date-strip-time
                                                         (get-val doc 'created-date))
                                                        data-hash))
                                  (val 0))
                              (if current-val
                                  (setf val current-val))
                              (setf val (incf val (gpv (get-val doc 'payload) :user :followers--count)))
                              (setf (gethash (universal-date-strip-time
                                              (get-val doc 'created-date))
                                             data-hash)
                                    val)))))))))
               (generic-post-collection))
    (maphash  
     (lambda (key val)
       (push (list key val) range))
     data-hash)
    (setf range (sort range #'> :key #'car))
    (loop for post in range
          do (setf (car post) (format-universal-date-dash (car post))))
    range))

(defun mentions-of-me-in-tweets-mentioner-count-range (start-date end-date)
  (let ((range)
        (sorted-range)
        (final-range)
        (data-hash (make-hash-table :test 'equal)))

    (find-docs 'list
               (lambda (doc)
                 (typecase (get-val doc 'channel-user) 
                   (channel-user
                    (when (string-equal (get-val doc 'post-type) "Twitter")
                      (when (and
                             (match-context-entities (get-val doc 'channel-user))
                             (gpv (get-val doc 'payload) :entities :user--mentions)
                             
                             (and (>= (get-val doc 'created-date) start-date)
                                  (<= (get-val doc 'created-date) end-date)))
                        (dolist (mention (gpv (get-val doc 'payload) :entities :user--mentions))
                          (when (not (string-equal
                                      (get-val (get-val doc 'channel-user) 'channel-user-name)
                                      (gpv mention :screen--name)))
                           (let ((current-val (or (gethash (gpv mention :screen--name)
                                                           data-hash)
                                                  0))
                                 )
                             
                             ;;(setf val (incf val (gpv (get-val doc 'payload) :user :followers--count)))
                             (setf (gethash (gpv mention :screen--name)
                                            data-hash)
                                   (incf current-val)))
                           ))
                        )))))
               (generic-post-collection))
    (maphash  
     (lambda (key val)
       (setf range
             (append range 
                     (list (list key val)))))
     data-hash)
    (setf sorted-range (sort range #'> :key #'second))
    (dolist (post sorted-range)
      (setf final-range 
            (append final-range 
                    (list 
                     (list (format-universal-date-dash (first post))
                           (second post))))))
    final-range))

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
  (let (range)
    (loop for user across (channel-users)
          when (valid-channel-user user channel)
          do
          (map-generic-insight-values
           (lambda (insight)
             (when (value insight)
               (push (list 
                      (format-universal-date-dash (end-time insight)) 
                      (value insight))
                     range)))
           user insight-name start-date end-date))
    (nreverse range)))

(defun insight-range-count (channel insight-name start-date end-date)
  (let ((count 0))
    (loop for user across (channel-users)
          when (valid-channel-user user channel)  
          do
          (map-generic-insight-values
           (lambda (insight)
             (when (value insight)
               (incf count (value insight))))
           user insight-name start-date end-date))
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
                (let ((date (get-val insight 'end-time))
                      (prev-date))
                  (unless (equal prev-date date)
                    (setf count (apply #'gpv
                                       (get-val insight 'value)
                                       (if (listp keys)
                                           keys
                                           (list keys)))))
                  (setf prev-date date))))))))
    count))

(defun compound-insight-range (channel insight-name start-date end-date keys)
  (let ((range)
        (sorted-range)
        (final-range)
        (data-hash (make-hash-table :test 'equal)))
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
                     (setf (gethash (get-val insight 'end-time)
                                      data-hash)
                             (apply #'gpv
                                        (get-val insight 'value)
                                        (if (listp keys)
                                            keys
                                            (list keys))))

                     ))))))
    (maphash  
     (lambda (key val)
       (setf range
             (append range 
                     (list (list key val)))))
     data-hash)
    (setf sorted-range (sort range #'< :key #'car))
    (dolist (post sorted-range)
      (setf final-range 
            (append final-range 
                    (list 
                     (list (format-universal-date-dash (first post))
                           (second post))))))
    final-range))

(defun compound-insight-range-count (channel insight start-date end-date keys)
  (let ((count 0))
    (dolist (channel-user (coerce (channel-users) 'list ))
      (when (valid-channel-user channel-user channel)
        (find-docs   
         'list
         (lambda (doc)
                 (let ((date (universal-date-strip-time 
                               (get-val doc 'end-time)))
                       (prev-date))
                   (unless (equal prev-date date)
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
                                              (list keys))))))
                   (setf prev-date date)))
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
       when (gpv 
             (gethash :post-likes 
                      (get-val post 'post-data)) 
             :summary :total--count)
       when  (and (<= start-date 
                      (parse-facebook-created-at 
                       (gpv (get-val post 'payload) :created--time))) 
                  (>= end-date 
                      (parse-facebook-created-at 
                       (gpv (get-val post 'payload) :created--time))))
       do (incf count (if (gpv 
                           (gethash :post-likes 
                                    (get-val post 'post-data)) 
                           :summary :total--count)
                          (gpv 
                           (gethash :post-likes 
                                    (get-val post 'post-data)) 
                           :summary :total--count)
                          0
                          )))
    count))


(defun fb-comment-dates-count (post start-date end-date)
  (let ((count 0))   
    (dolist (comment  (gpv (gethash :post-comments (get-val post 'post-data)) :data))
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
                        
                       post
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

(defun dash-small-stat-graph (title graph-id range total-count total-percent &key tooltip)
  (with-html-string
    (:div :class "span3"
         
          (:div :class "stat-block"
                 
                (:ul 
                 (:li :class "stat-graph"
                      :id graph-id
                      (str range)
                      )
                 (:li :class "stat-count"
                      :title (str tooltip) 
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

(defun board-stats (range title icons chart span &key tooltip )
  (with-html-string
    (:div :class span
         
          (:div
                :class "board-stats"
                (:div :class "statistics-wrap"
                      (:div :class "statistics-block"
                            (:div :class (format nil "stat-chart ~A" chart)
                                  (str range))
                            (:div :class "stat-info"
                                   :title tooltip
                                  (dolist (icon icons)
                                    (htm (:span :class (format nil "black-icons ~A" icon))))
                                  (str title))
                            ))))))

(defun %line-graph (name title min-date max-date data interval 
                    &key series span)
  (with-html
    (:div :class (or span "span9")
          (:div :class "graph-wrap"
                (:div :class "chart-block"
                      (let ((network-size 
                              (make-widget
                               'line-graph :name name)))
                        (setf (data network-size) data)
                        (setf (title network-size) title)
                        (setf (x network-size)
                              `(:type :date
                                :tick-interval ,(if (<= interval 7)
                                                    "1 days"
                                                    (if (and (> interval 7) 
                                                             (< interval 100))
                                                        "14 days"
                                                        "60 days"))
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
                              '(:show nil :placement :outside))
                        
                        (setf (series network-size)
                              series)
                        (setf (series-defaults network-size) 
                              '(:show "true"
                                :xaxis "xaxis"
                                :yaxis "yaxis"
                                :line-width 3
                                :shadow "false"))
                        (render network-size)))))))

(defun %engagement-graph (data data-lables)
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
  (with-html-string
    (:div :class "row-fluid"
          (:div :class "nonboxy-widget"
                (:div :class "widget-head"
                      (:h5 ;;(:i :class "black-icons month_calendar")
                           (str "Dashboard Interval")))

                (:div :class "widget-content"
                      (:div :class "row-fluid"
                            
                            (:div :class "widget-box"
                                         (:div :class "row-fluid"
                                               
                                               (:div :class "span3"
                                                     (:span :style "font-weight:bold;" "Fixed Interval:")
                                                     (:form :name "dash-date-form" :action ""  :method "post"
                                                            (:div :class "control-group"
                                                                  (:label :class "control-label")
                                                                  (:div :class "controls"
                                                                        (:label :class "radio"
                                                           
                                                                                (:input :type :radio 
                                                                                        :id "shit"
                                                                                        :name "dashboard-interval" 
                                                                                        :value "7 Days"
                                                                                        :checked (string-equal (parameter "dashboard-interval") "7 Days")
                                                                                        )
                                                                                (str "7 Days"))
                                                                        (:label :class "radio"
                                                                                (:input :type :radio 
                                                                                        :name "dashboard-interval" 
                                                                                        :value "30 Days"
                                                                                        :checked (or (string-equal (parameter "dashboard-interval") "30 Days")
                                                                                                     (not (parameter "dashboard-interval")))
                                                                                        )
                                                                                (str "30 Days"))
                                                                        (:label :class "radio"
                                                                                (:input :type :radio 
                                                                                        :name "dashboard-interval" 
                                                                                        :value "365 Days"
                                                                                        :checked (string-equal (parameter "dashboard-interval") "365 Days")
                                                                                        )
                                                                                (str "365 Days"))
                                                                        ))
                                                            (:div :class "widget-bottom"
                                                                  (:input :type "submit"                                     
                                                                          :class "btn btn-info" 
                                                                          :name "set-dash-interval" 
                                                                          :value "GO"))
                                                            (:br)))
                                               (:div :class "span5"
                                                     (:span :style "font-weight:bold;" "Custom Interval:")
                                                     (:br)
                                                     (:br)
                                                     (:form :name "dash-date-formx" :action ""  :method "post"                        
                                                                  
                                                            (:div :class "row-fluid"
                                                                  (:div :class "span4" 
                                                                        (str "From Date")
                                                                        (render-edit-field 
                                                                         "interval-start-date"
                                                                         (parameter "interval-start-date")
                                                                         :type :date
                                                                         :required nil
                                                                         :width "100px"))
                                                                  (:div :class "span4" 
                                                                        (str " To Date")
                                                                        (render-edit-field 
                                                                         "interval-end-date"
                                                                         (parameter "interval-end-date")
                                                                         :type :date
                                                                         :required nil
                                                                         :width "100px"))
                                                           
                                                                  )
                                                            (:div :class "widget-bottom"
                                                                  (:input :type "submit"                                     
                                                                          :class "btn btn-info" 
                                                                          :name "set-dash-interval" 
                                                                          :value "GO")))
                                                     
                                                     ))
                                         (:div :class "row-fluid"
                                               
                                              )
                                         )
                            

                            )
                      (:div :class "row-fluid"
                            (:br)
                            )
                      )))))

(defun dashboard-overview ()
  (with-html-string
    ))


(defun calc-date-interval ()
  (let* ((today (universal-today))
        (interval 30)
        (start-date (- today (* +24h-secs+ interval)))
        (end-date today))
    
    (when (and (not (empty-p (parameter "interval-start-date")))
               (not (empty-p (parameter "interval-end-date"))))

      (setf start-date (string-to-date (parameter "interval-start-date")))

      (setf end-date (+ (string-to-date (parameter "interval-end-date"))  +24h-secs+ interval))

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

(defun merge-ranges (ranges)
  (let ((data-hash (make-hash-table :test 'equal))
        (merged-range)
        (sorted-range)
        (final-range))
    (dolist (range-vals ranges)
      (dolist (range range-vals)
        (let* ((current-date (string-to-date (first range) 
                                             :date-spacer #\-
                                             :reverse-date-sequence-p t))
               (current-val (gethash current-date data-hash))
               (val 0))
          (if current-val
              (setf val current-val))
          (setf val (second range))
          (setf (gethash current-date
                         data-hash)
                val))))
    (maphash  
     (lambda (key val)
       (setf merged-range
             (append merged-range 
                     (list (list key val)))))
     data-hash)
    (setf sorted-range (sort merged-range #'> :key #'car))
    (dolist (post sorted-range)
      (setf final-range 
            (append final-range 
                    (list 
                     (list (format-universal-date-dash (first post))
                           (second post))))))
    final-range))

(defun multiply-ranges (ranges &key (smooth-out-p t))

  (let ((ranges-hash-list)
        (data-hash (make-hash-table :test 'equal))
        (range)
        (sorted-range)
        (final-range))
    (dolist (range-values ranges)
      (let ((range-hash (make-hash-table :test 'equal)))
        (dolist (range range-values)

          (let* ((current-date (string-to-date (first range) 
                                               :date-spacer #\-
                                               :reverse-date-sequence-p t))
                 (current-val (gethash current-date data-hash))
                 (val 0))
            
            (if current-val
                (setf val current-val))
            (setf val (second range))
            (setf (gethash current-date
                           range-hash)
                  val)))
        (setf ranges-hash-list (append ranges-hash-list (list range-hash)))))
    ;(break "?")
    (maphash   
     (lambda (key value)
       (let ((current-val (if value
                              value
                              0)))
         
         (dolist (range-hash (rest ranges-hash-list))
           ;;stop crashes of ranges that are not in synq

           (when smooth-out-p
             (if (gethash key range-hash)
                 (setf current-val (* current-val (gethash key range-hash)))
                 ))
           (unless smooth-out-p
             (setf current-val (* current-val (or (gethash key range-hash) 0)))))
         
         (setf (gethash key data-hash) current-val)))
     (first ranges-hash-list))

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


(defun get-list-of-context-users ()
  (loop for user across (users)
       when (match-context-entities user)
       collect user))

(defclass action-posts ()
  ((action :initarg :action)
   (fb-posts :initarg :fb-posts)
   (tweets :initarg :tweets)))

(defclass user-posts ()
  ((user :initarg :user)
   (actions :initarg :actions)))

(defun get-list-posts-user (user start-date end-date) 
  (let* ((generic-actions 
          (find-docs 'list
                     (lambda (doc)
                       (and
                        (string-equal (get-val doc 'action-status) "Completed")
                        (get-val doc 'action-log)
                        ;;(match-context-entities (get-val doc 'channel-user))
                        (and (>= (get-val doc 'scheduled-date) start-date)
                             (<= (get-val doc 'scheduled-date) end-date))
                        (or (string-equal (get-val doc 'user) 
                                          (get-val user 'email))
                            ;;doing this because in some old data the system posted all
                            ;;scheduled posts as admin instead of using the correct user
                            
                            (find (get-val user 'email)
                                  (old-versions doc)
                                  :test (lambda (email post-user)
                                            
                                          (if (string-equal
                                               email 
                                               (get-val post-user 'user))
                                              doc)))
                            )))
                     (generic-actions-collection)))
         (fb-posts (make-hash-table))
         (tweets (make-hash-table))
         (user-action-posts))
    (find-docs 'list 
               (lambda (doc)
                 (let ()
                   (dolist (action generic-actions)
                     
                     (cond ((and (string-equal (get-val doc 'post-type) 
                                                 "Facebook")
                                   (string-equal (get-val action 'post-type) 
                                                 "Facebook"))
                              (when (string-equal 
                                     (gpv (get-val 
                                           (if (listp (get-val action 'action-log))
                                               (car (last (get-val action 'action-log)))
                                               (get-val action 'action-log)) 
                                           'message) 
                                          :post--id) 
                                     (post-id doc))
                            
                                (setf (gethash action fb-posts) 
                                      (append (gethash action fb-posts) (list doc)))))
                             ((and (string-equal (get-val doc 'post-type) 
                                                 "Twitter")
                                   (string-equal (get-val action 'post-type) 
                                                 "Twitter"))
                          
                              (when (string-equal 
                                     (gpv (get-val 
                                           (if (listp (get-val action 'action-log))
                                               (car (last (get-val action 'action-log)))
                                               (get-val action 'action-log)) 
                                           'message) 
                                          :id--str) 
                                     (format nil "~A" (post-id doc)))
                                (setf (gethash action tweets) 
                                      (append (gethash action tweets) (list doc)))))))))
               (generic-post-collection))

    (dolist (action generic-actions) 
     (setf user-action-posts 
           (append user-action-posts 
                   (list (make-instance 'action-posts
                                        :action action
                                        :fb-posts (gethash action fb-posts)
                                        :tweets (gethash action tweets)
                                        ))
                   )))
    
    (make-instance 'user-posts
                   :user user
                   :actions user-action-posts)))
#|
       when  (and (<= start-date 
                      (parse-facebook-created-at 
                       (gpv (get-val post 'payload) :created--time))) 
                  (>= end-date 
                      (parse-facebook-created-at 
                       (gpv (get-val post 'payload) :created--time))))
|#



(defun fb-user-post-likes (posts)
  (let ((count 0))
    (loop for post in posts 
       when (match-context-entities (channel-user post) )
       when (string-equal (get-val post 'post-type) 
                          "Facebook")
       when (get-val post 'payload)
       when (gpv 
             (gethash :post-likes 
                      (get-val post 'post-data)) 
             :summary :total--count)

       do (incf count (gpv 
                       (gethash :post-likes 
                                (get-val post 'post-data)) 
                       :summary :total--count)))
    count))


(defun fb-user-post-comments (posts)
  (let ((count 0))
    (loop for post in posts    
       when (string-equal (get-val post 'post-type) 
                          "Facebook")
       when (get-val post 'payload)
       when (gpv 
             (gethash 
              :post-comments 
              (get-val post 'post-data))
             :summary :total--count)

       do (incf count (gpv 
                       (gethash 
                        :post-comments 
                        (get-val post 'post-data))
                       :summary :total--count)))
    count))




(defun tw-retweets-of-my-tweets (posts)
  (let ((count 0))
    (dolist (doc posts)
               (typecase (get-val doc 'channel-user) 
                   (channel-user
                    (when (string-equal (get-val doc 'post-type) "Twitter")
                      (when (and
                             (match-context-entities (get-val doc 'channel-user))
                             (> (or (gpv (get-val doc 'payload) :retweet--count) 0) 0)
                             (string-equal 
                              (get-val (get-val doc 'channel-user) 'channel-user-name) 
                              (gpv (get-val doc 'payload) :user :screen--name))
                             )
                        (incf count (or (gpv (get-val doc 'payload) :retweet--count) 0))
                        doc)))))
    count))

(defun users-posts (start-date end-date)
  (let ((users ;;(append (context-users-list) (list (get-user "admin@dyb.co.za")))
          (context-users-list))
        (posts))

    (dolist (user users)
      (pushnew  
       (list (email user) (get-list-posts-user user start-date end-date))
         posts
         :test 'equal
         :key #'car))
    posts))

(defun users-stats (users-posts)
  (let ((stats))

    (dolist (user-posts users-posts)
      
      (let* ((user-actions (second user-posts))
             (actions (if user-actions
                          (get-val user-actions 'actions)))
             (posts (or (length actions) 0))
             (likes 0)
             (comments 0)
             (retweets 0))

        
        
        (dolist (action actions)
          (incf likes (fb-user-post-likes 
                       (get-val action 'fb-posts)))
          (incf comments (fb-user-post-comments 
                       (get-val action 'fb-posts)))
          (incf retweets (tw-retweets-of-my-tweets 
                       (get-val action 'tweets))))
        (pushnew 
         (list (car user-posts)
               (list 
                (list "Posts" posts)
                (list "Likes" likes) 
                (list "Comments" comments)
                (list "Retweets" retweets)))
         stats)
        ))
    
    stats))

(defun content-stats (users-posts)
  (let ((stats))

    (dolist (user-posts users-posts)
      
      (let* ((user-actions (second user-posts))
             (actions (if user-actions
                          (get-val user-actions 'actions))))

        
        
        (dolist (action actions)
        ;;  (break "~A" (get-val action 'action))
          (pushnew 
           (list (format-universal-date-time
                  (get-val 
                   (get-val action 'action)
                   'scheduled-date))
                 (or (get-val 
                      (get-val action 'action)
                      'processed-content)
                     (get-val 
                      (get-val action 'action)
                      'action-content))
                 (list 
                 ; (list "Action" (get-val action 'action))
                  (list "Likes" (fb-user-post-likes 
                                 (get-val action 'fb-posts))) 
                  (list "Comments" (fb-user-post-comments 
                                    (get-val action 'fb-posts)))
                  (list "Retweets" (tw-retweets-of-my-tweets 
                                    (get-val action 'tweets)))))
           stats)
          )
        
        ))
    
    stats))


(defun tw-user-posts-mentions (posts)
  (let ((count 0))
    (dolist (doc posts)
      (typecase (get-val doc 'channel-user) 
        (channel-user
         (when (string-equal (get-val doc 'post-type) "Twitter")
           (when (gpv (get-val doc 'payload) :entities :user--mentions)
             (dolist (mention (gpv (get-val doc 'payload) :entities :user--mentions))
               ;;(break "~A" (get-val doc 'payload))
               (when (string-equal
                      (get-val (get-val doc 'channel-user) 'channel-user-name)
                      (gpv mention :screen--name))
                              
                 (incf count)))
                        
             )))))
    count))

(defun audiance-stats (users-posts)
  (let ((stats))

    (dolist (user-posts users-posts)
      
      (let* ((user-actions (second user-posts))
             (actions (if user-actions
                          (get-val user-actions 'actions))))

        
        
        (dolist (action actions)
        ;;  (break "~A" (get-val action 'action))

          
          (pushnew 
           (list (format-universal-date-time
                  (get-val 
                   (get-val action 'action)
                   'scheduled-date))
                 (or (get-val 
                      (get-val action 'action)
                      'processed-content)
                     (get-val 
                      (get-val action 'action)
                      'action-content))
                 (list 
                 ; (list "Action" (get-val action 'action))
                  (list "Eish" (tw-user-posts-mentions 
                                 (get-val action 'tweets))) 
                  
                  
                  ))
           stats)
          )
        
        ))
    
    stats))



(defun %bar-graph (name title data ticks &key series)
  (with-html-string
    (:div :class "span6"
          (:div :class "graph-wrap"
                (:div :class "chart-block"
                      (let ((graph 
                             (make-widget
                              'line-graph :name name
                              :data data
                             )))
                     ;;   (setf (get-val graph 'data-lables) `("xxxx" "yyyy"))
                        (setf (series graph)
                               series)
                        (setf (get-val graph 'data) data)
                        (setf (title graph) title)
                        
                        
                      
                        (setf (legend graph)
                              '(:show t :placement "w"))
                        
                        (setf (series-defaults graph) 
                              '(:show "true"
                          ;;      :xaxis "xaxis"
                         ;;       :yaxis "yaxis"
                                :renderer :bar
                             ;;   :point-labels
                             ;;   (:show  "true")
                                ))
                        
                        (setf (x graph) 
                              `(:renderer :category
                                          :ticks ,ticks))
                        (render graph)))))))


(defun posts-with-links-count (start-date end-date)
  (let ((count 0))
    (find-docs 
     'list 
     (lambda (doc)
       (when (match-context-entities (get-val doc 'channel-user))
         (when (and (>= (get-val doc 'scheduled-date) start-date)
                    (<= (get-val doc 'scheduled-date) end-date))
           (when (string-equal (get-val doc 'action-status) "completed")
            
             (when (or (post-url doc) (processed-content doc))
                        (when (processed-content doc)
                          

                          (unless (equal (length (format nil "~A" (processed-content doc)))
                                         (length (format nil "~A" (action-content doc))))
                            (incf count)))
                        (when (or (post-url doc) (short-url doc))
                          (incf count)))))))
     (generic-actions-collection))
    count))

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

      (sv 'posts-with-links-count
          (posts-with-links-count istd
                                  iend))

      (sv 'fb-scheduled-count
          (posts-scheduled-range-count 
           istd 
           iend
           :post-type "Facebook"))

      (sv 'fb-scheduled-prev-count
          (posts-scheduled-range-count
           prev-istd 
           prev-iend
           :post-type "Facebook"))

      (sv 'fb-scheduled-prev-count
          (posts-scheduled-range-count 
           prev-istd 
           prev-iend
           :post-type "Facebook"))
           
      (sv 'fb-scheduled-list
          (posts-scheduled-range 
           istd 
           iend
           :post-type "Facebook"))

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
           "page_impressions_unique" 
           istd 
           iend))

      (sv 'fb-page-impressions-count
          (insight-range-count 
           "Facebook"
           "page_impressions_unique" 
           istd 
           iend))

      (sv 'fb-page-impressions-prev-count
          (insight-range-count 
           "Facebook"
           "page_impressions_unique" 
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


     #| (sv 'twitter-retweets-list 
          (compound-insight-range 
           "Twitter" "twitter-profile" 
           istd 
           iend 
           (list :status :retweet--count)))
      |#

      
      (sv 'twitter-retweets 
          (retweets-of-my-tweets
           istd 
           iend
           )
          )
      
      (sv 'twitter-retweets-prev 
          (retweets-of-my-tweets
           prev-istd 
           prev-iend
           )
          )
      #|
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
      |#
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
          (mentions-of-me-in-tweets ;;twitter-at-mentions 
           istd 
           iend ))

      (sv 'twitter-at-mentions-prev-count
          (mentions-of-me-in-tweets;; twitter-at-mentions 
           prev-istd 
           prev-iend))

     (sv 'twitter-at-mentions-followers-count
          (mentions-of-me-in-tweets-mentioner-followers
           istd 
           iend ))

      (sv 'twitter-at-mentions-followers-prev-count
          (mentions-of-me-in-tweets-mentioner-followers
           prev-istd 
           prev-iend))

      (sv 'twitter-at-mentions-followers-range
          (mentions-of-me-in-tweets-mentioner-followers-range
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
      (sv 'tweets-retweeted-own-count
          (retweets-of-my-tweets
           istd 
           iend
           ))
      (sv 'mentions-of-me-in-tweets-count
            (mentions-of-me-in-tweets
             istd 
             iend
             ))
      (sv 'mentions-of-me-in-tweets-mentioner-count-range
          (mentions-of-me-in-tweets-mentioner-count-range
           istd
           iend))
          
      (sv 'short-url-clicks-count
       (short-url-clicks
        istd 
        iend))

      (sv 'short-url-clicks-prev-count
       (short-url-clicks
         prev-istd 
         prev-iend))
      (sv 'likedin-connections-count
          (linkedin-connections-count))
      (sv 'users-posts (users-posts 
                        istd
                        iend))
      (sv 'users-stats
          (users-stats (gethash 'users-posts calc-values)))

      (sv 'content-stats
          (content-stats (gethash 'users-posts calc-values)))
      (sv 'audiance-stats
          (audiance-stats (gethash 'users-posts calc-values))))
    calc-values))

(defparameter *calc-values* nil)

(defun gv (key)
  (gethash key *calc-values*))

(defun reach-small-graph ()
  (with-html
    (let* ((prev (+
                  (or (cadar (last (gv 'fb-fans-interval-prev-list))) 0)
                  (or (gv 'fb-page-impressions-prev-count) 0)
                  (or (gv 'twitter-followers-prev-count) 0)
                  (* (or (gv 'twitter-followers-prev-count) 0)
                     (or (gv 'tweets-scheduled-prev-count) 0))      
                  (or (gv 'twitter-retweets-prev) 0)
                  (or (gv 'twitter-at-mentions-prev-count) 0)
                  ;;linkedin-connections-count
                  ))
           (cur (+
                 (or (cadar (last (gv 'fb-fans-interval-list))) 0)
                 (or (gv 'fb-page-impressions-count) 0)
                 (or (gv 'twitter-followers-count) 0)
                 (* (or (gv 'twitter-followers-count) 0)
                    (or (gv 'tweets-scheduled-count) 0))
                 (or (gv 'twitter-retweets) 0)
                 (or (gv 'twitter-at-mentions-count) 0)
                 ;; linkedin-connections-count
                 )))
      (str (dash-small-stat-graph
            "Reach"
            "new-visits"
            (format nil "~A,~A" prev cur)  
            cur
            (calc-prev-cur-percentage prev cur) 
            :tooltip "FB-Fans + FB-Page-Impressions + TW-Followers + (TW-Followers * Scheduled-Tweets) + Retweets + TW-Mentions")))))

(defun activity-small-graph ()
  (with-html-string
    (str (dash-small-stat-graph 
          "Activity"
          "unique-visits"
          (format nil "~A,~A" 
                  (gv 'posts-scheduled-prev-count)
                  (gv 'posts-scheduled-count))
          (gv 'posts-scheduled-count)
          (calc-prev-cur-percentage 
           (gv 'posts-scheduled-prev-count) 
           (gv 'posts-scheduled-count))
          :tooltip "Posts Scheduled"))))

(defun engagement-small-graph ()
  (with-html-string
    (str (let ((prev (+ (gv 'fb-post-likes-prev-count)                                                    
                        (gv 'fb-comments-made-prev-count)
                        (gv 'twitter-retweets-prev)
                        (gv 'twitter-at-mentions-prev-count)
                        (gv 'short-url-clicks-prev-count)))
               (cur (+ (gv 'fb-post-likes-count)
                       (gv 'fb-comments-made-count)
                       (gv 'twitter-retweets)
                       (gv 'twitter-at-mentions-count)
                       (gv 'short-url-clicks-count))))
           (dash-small-stat-graph 
            "Engagement"
            "weekly-sales"
            (format nil "~A,~A" 
                    prev
                    cur)
            cur
            (calc-prev-cur-percentage prev cur)
            :tooltip "FB-Post-Likes + FB-Comments + Retweets + TW-Mentions + Clicks")))))

(defun engagement-pie-graph ()
  (%engagement-graph `((("Post Likes" ,(gv 'fb-post-likes-count))
                        ("Shortner Clicks" ,(gv 'short-url-clicks-count))
                        ("Comments" ,(gv 'fb-comments-made-count))
                        ("Retweets" ,(gv 'twitter-retweets))
                                                   
                        ;;("FB Shares" ,(gv 'fb-story-adds-count))
                        ("Mentions" ,(gv 'twitter-at-mentions-count)
                                    )
                        ;;("Direct Messages" 0)
                        ))
                     (format nil "[~A,~A,~A,~A,~A]" 
                             (gv 'fb-post-likes-count) 
                             (gv 'short-url-clicks-count)
                             (gv 'fb-comments-made-count) 
                             (gv 'twitter-retweets)
                             (gv 'twitter-at-mentions-count))))

(defun current-community-size ()
  (with-html-string
      (:div :class "summary"
            (:h4 "CURRENT COMMUNITY SIZE")
            (:br)
            (:ul
             (:li
              (str (community-summary-item  
                    "All Accounts"
                    (+ 
                     (or (cadar (last (gv 'fb-fans-interval-list))) 0)
                     (or (gv 'twitter-followers-count) 0)
                     (or (gv 'linkedin-connections-count) 0))
                    "/appimg/user-accounts.png"
                    "All Accounts"
                    nil)))
             (:li
              (str (community-summary-item  
                    " Facebook"
                    (or (cadar (last (gv 'fb-fans-interval-list))) 0)
                    "/appimg/Facebook_Light_Logo.png"
                    "Facebook Friends"
                    t)))
             (:li
              (str (community-summary-item  
                    " Twitter"
                    (or (gv 'twitter-followers-count) 0)
                    "/appimg/twitter-bird-white-on-blue.png"
                    "Twitter Followers"
                    t)))
             (:li
              (str (community-summary-item  
                    " LinkedIn"
                    (or (gv 'linkedin-connections-count) 0)
                    "/appimg/linkedin-icon.png"
                    "LinkedIn Connections"
                    t)))))))

 
(defun fill-blanks (range start-date end-date &key smooth-range)
  
  (let ((filled-range)
        (max-date 0))
    (dotimes (n (round (date-diff start-date end-date :return-type :days)))

      (let* ((u-date (+ start-date (* +24H-SECS+ n)))
             (date (format-universal-date-dash u-date))
             
             (range-val)
             (smooth-val 0)
             (range-x (if (listp (first (car range)))
                          (car range)
                          range)))
        
        (dolist (value  range-x)
          ;;(break "~A" value)
          (let ((range-date (string-to-date (first value) 
                                         :date-spacer #\- 
                                         :reverse-date-sequence-p t) ))
            (when (> range-date max-date)
                (setf max-date range-date))
            (when (<= range-date u-date )
                (if (<= range-date u-date )
                    (if (> (second value) 0.00) 
                        (setf smooth-val (second value)))))
            (if (string-equal  (first value) date)
                  (setf range-val value))))

        (when range-val
          (when smooth-range
            (when (= (second range-val) 0.00)
              (setf range-val (list (first range-val) (or smooth-val 0)))
              )))

        (unless range-val
          
          (setf range-val (list date (if smooth-range
                                         (or smooth-val 0)
                                         0))))
        
        (when (or (> max-date u-date) (= max-date 0))
          (setf filled-range (append filled-range (list range-val))))))
    filled-range))

(defun normalize-gaps (data)
  (loop for previous-time = nil then time
        for previous-value = nil then value
        for entry in data
        for (date value) = entry
        for time = (string-to-date date :date-spacer #\- :reverse-date-sequence-p t)
        when (and previous-value
                  previous-time
                  value
                  (not (eql previous-value value))
                  (> (- time previous-time) +24h-secs+))
        collect (list (format-universal-date-dash (- time +24h-secs+))
                      previous-value)
        collect entry))

(defun normalize-data (data start-date end-date)
  (when data
    (let* ((data (normalize-gaps data))
           (first (first data))
           (last (car (last data)))
           (start-date (format-universal-date-dash start-date))
           (end-date (format-universal-date-dash end-date))
           (start-equal (equal start-date (first first)))
           (end-equal (equal end-date (first last))))
      (cond ((and start-equal end-equal)
             data)
            (end-equal
             (cons (list start-date (second first))
                   data))
            (start-equal
             (cons (list end-date (second last))
                   data))
            (t
             (list* (list start-date (second first))
                    (list end-date (second last))
                    data))))))

(defun network-size-graph (min-date max-date interval)
  (%line-graph
   "networksizegraph"
   "Change in Network"
   (format-universal-date-dash min-date)
   (format-universal-date-dash max-date)
   (let ((twitter (normalize-data (gv 'twitter-followers-interval-list) min-date max-date))
         (facebook (normalize-data (gv 'fb-fans-interval-list) min-date max-date)))
     (cond ((and twitter facebook)
            (list twitter facebook))
           (twitter
            (list twitter))
           (facebook
            (list facebook))))
   interval
   :series  '((:show-marker nil
               :color "#00ACED"
               :label "TW")
              (:show-marker nil
               :color "#3B5999"
               :label "FB")
              (:show-marker nil
               :color "#04B45F"
               :label "LNK"))))

  (defun fb-new-page-likes-graph ()
    
    (with-html-string
      (str (board-stats  
            (create-bar-range-string 
             (gv 'fb-fans-adds-interval-list) 7) 
            "New Page Likes" 
            (list "facebook_like") 
            "bar-chart" "span3"
            :tooltip "The number of new people who have liked your Page."))))


(defun fb-new-page-likes-graph-html (min-date max-date interval)
  
  (%line-graph
   "fbnewpageslikes"
   "New Page Likes"
   (format-universal-date-dash min-date)
   (format-universal-date-dash max-date)
   `(( ,@(fill-blanks (gv 'fb-fans-adds-interval-list)
                      min-date max-date)))
   interval
   :series  '((:show-marker nil
               :color "#3B5999"
               :label "FB"))
   :span "span12"))

(defun fb-page-impressions-graph ()
  (with-html-string
    (str (board-stats 
          (create-bar-range-string 
           (gv 'fb-impressions-interval-list) 7)
          "Page Impressions" 
          (list "documents")
          "bar-chart" "span3"
          :tooltip "The total number of impressions seen of any content associated with your Page."))))

(defun fb-page-impressions-graph-html (min-date max-date interval)
  (%line-graph
   "fbpageimpressions"
   "Page Impressions"
   (format-universal-date-dash min-date)
   (format-universal-date-dash max-date)
   `(( ,@(fill-blanks (gv 'fb-impressions-interval-list)
                      min-date max-date)))
   interval
   :series  '((:show-marker nil
               :color "#3B5999"
               :label "FB"))
   :span "span12"))

(defun fb-total-fans-graph ()
  (with-html-string
    (str (board-stats 
          (create-bar-range-string 
           (gv 'fb-fans-interval-list) 7)
          "Total Fans" 
          (list "users")
          "bar-chart" "span3"
          :tooltip "The total number of people who have liked your Page."))))

(defun fb-total-fans-graph-html (min-date max-date interval)
  (%line-graph
   "fbtotalfans"
   "Total Fans"
   (format-universal-date-dash min-date)
   (format-universal-date-dash max-date)
   `(( ,@(fill-blanks (gv 'fb-fans-interval-list)
                      min-date max-date
                      :smooth-range t)))
   interval
   :series  '((:show-marker nil
               :color "#3B5999"
               :label "FB"))
   :span "span12"))

(defun fb-page-unlikes-graph ()
  (with-html-string
    (str (board-stats  
          (create-bar-range-string 
           (gv 'fb-fans-removes-interval-list) 7) 
          "Page Unlikes" 
          (list "facebook_unlike") 
          "bar-chart" "span3"
          :tooltip "Unlikes of your Page."))))

(defun fb-page-unlikes-graph-html (min-date max-date interval)
  (%line-graph
   "fbpageunlikes"
   "Page Unlikes"
   (format-universal-date-dash min-date)
   (format-universal-date-dash max-date)
   `(( ,@(fill-blanks (gv 'fb-fans-removes-interval-list)
                      min-date max-date)))
   interval
   :series  '((:show-marker nil
               :color "#3B5999"
               :label "FB"))
   :span "span12"))

(defun tw-new-followers-graph ()
  (with-html-string
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
        "bar-chart" "span3"
        :tooltip "Then number of new followers aquired.")))))



(defun calc-daily-change (date-val-list)
  (let ((final-list)
        (previous nil))
    (dolist (follower date-val-list)
      
      (when previous
        (setf final-list
              (append final-list 
                      (list (list (first follower) 
                                  (- (fix-nan (second previous))
                                     (fix-nan (second follower))
                                     ))))))
        
      (setf previous follower))
    final-list))

(defun calc-daily-change-reverse (date-val-list)
  (cons (list (caar date-val-list) 0)
        (loop for previous = 0 then (or count 0)
              for (date %count) in (cdr date-val-list)
              for count = (or %count 0)
              unless (eql count previous)
              collect (list date (- count previous)))))

(defun tw-new-followers-graph-html (min-date max-date interval)
  (%line-graph
   "twnewfollowers"
   "New Followers"
   (format-universal-date-dash min-date)
   (format-universal-date-dash max-date)
   (list (calc-daily-change-reverse (gv 'twitter-followers-interval-list)))
   interval
   :series  '((:show-marker nil
               :color "#00ACED"
               :label "TW"))
   :span "span12"))


(defun tw-impressions-graph ()
  (with-html-string
    (str 
     (let* ((tweets-followers 
             (multiply-ranges (list (gv 'tweets-scheduled-list)
                                    (gv 'twitter-followers-interval-list))) )
                                   
            (impressions (strip-dates-from-range 
                          (merge-ranges (list tweets-followers 
                                              (gv 'twitter-at-mentions-followers-list)))
                          7)))
                            (board-stats                                 
        (format nil "~A,~A,~A,~A,~A,~A,~A" 
                (fix-nan (nth 6 impressions))
                (fix-nan (nth 5 impressions))
                (fix-nan (nth 4 impressions))
                (fix-nan (nth 3 impressions))
                (fix-nan (nth 2 impressions))
                (fix-nan (nth 1 impressions))
                (fix-nan (nth 0 impressions))
                            
                )
        "Impressions" 
        (list "documents")
        "bar-chart" "span3"
        :tooltip "The number of tweets scheduled * followers + mentions in tweets.")))))

(defun tw-impressions-graph-html (min-date max-date interval)

  (%line-graph
   "twimpressions"
   "Impressions"
   (format-universal-date-dash min-date)
   (format-universal-date-dash max-date)
   `(( ,@(multiply-ranges (list (fill-blanks (gv 'twitter-followers-interval-list)
                                             min-date max-date)
                                (fill-blanks (gv 'tweets-scheduled-list)
                                             min-date max-date)
                                )
                          :smooth-out-p nil)))
   interval
   :series  '((:show-marker nil
               :color "#00ACED"
               :label "TW"))
   :span "span12"))

(defun tw-total-fans-graph ()
  (with-html-string
    (str
     (board-stats (create-bar-range-string 
                   (gv 'twitter-followers-interval-list) 7)
                  "Total Fans" 
                  (list "users")
                  "bar-chart" "span3"
                  :tooltip "Total number of followers."))))

(defun tw-total-fans-graph-html (min-date max-date interval)
  (%line-graph
   "twtotalfans"
   "Total Fans"
   (format-universal-date-dash min-date)
   (format-universal-date-dash max-date)
   `(( ,@(fill-blanks (gv 'twitter-followers-interval-list)
                      min-date max-date
                      :smooth-range t)))
   interval
   :series  '((:show-marker nil
               :color "#00ACED"
               :label "TW"))
   :span "span12"))

(defun tw-un-followed-graph ()
  (with-html-string
    (str 
     (let ((new-followers (strip-dates-from-range 
                           (reverse (gv 'twitter-followers-interval-list)) 8)))
       ;;(break "~a" (reverse (gv 'twitter-followers-interval-list)))
       (board-stats 
        (format nil "~A,~A,~A,~A,~A,~A,~A" 
                (- (fix-nan (nth 7 new-followers)) (fix-nan (nth 6 new-followers)) )
                (- (fix-nan (nth 6 new-followers)) (fix-nan (nth 5 new-followers)) )
                (- (fix-nan (nth 5 new-followers)) (fix-nan (nth 4 new-followers)) )
                (- (fix-nan (nth 4 new-followers)) (fix-nan (nth 3 new-followers)) )
                (- (fix-nan (nth 3 new-followers)) (fix-nan (nth 2 new-followers)) )
                (- (fix-nan (nth 2 new-followers)) (fix-nan (nth 1 new-followers)) )
                (- (fix-nan (nth 1 new-followers)) (fix-nan (nth 0 new-followers)) ))
                                  
                                  
        "Un-Followed" 
        (list "users") 
        "bar-chart" "span3"
        :tooltip "The number of followers that was lost.")))))

(defun tw-un-followed-graph-html (min-date max-date interval)
  (%line-graph
   "twunfollowed"
   "Un-Followed"
   (format-universal-date-dash min-date)
   (format-universal-date-dash max-date)
   `(( ,@(calc-daily-change (fill-blanks (gv 'twitter-followers-interval-list)
                                         min-date max-date))))
   interval
   :series  '((:show-marker nil
               :color "#00ACED"
               :label "TW"))
   :span "span12"))

(defun posts-links-html ()
  (%bar-graph "_" " "   
              `((,(or (gv 'posts-scheduled-count) 0))
                (,(or (gv 'posts-with-links-count) 0)))
              '("Posts")
              :series '((:show-marker nil
                         :color "#00ACED"
                         :label "Posts")
                        (:show-marker nil
                         :color "#3B5999"
                         :label "Post with Links")
                        )))

(defun user-stats-html (user-stats)
  (with-html-string
    (:div :class "span8"
          (:div :class "nonboxy-widget"
                (:div :class "table_content"
                      (:table :class " table table-bordered dataTable" ;;data-tbl-simple
                              :id "DataTables_Table_0"
                              (:thead
                               (:tr :role "row"
                                    (:th :class "" :role "columnheader" :tabindex "0" 
                                         :aria-controls "DataTables_Table_0" :rowspan "1" :colspan "1" 
                                         :style "width: 50%;" :aria-sort "ascending" 
                                         :aria-label "User : activate to sort column descending"
                                         "User")
                                    (:th :class "" :role "columnheader" :tabindex "0" 
                                         :aria-controls "DataTables_Table_0" :rowspan "1" :colspan "1" 
                                         :style "width: 10%;" :aria-sort "ascending" 
                                         :aria-label "Posts : activate to sort column descending"
                                         "Posts")
                                                                                              
                                    (:th :class "" :role "columnheader" :tabindex "0" 
                                         :aria-controls "DataTables_Table_0" :rowspan "1" :colspan "1" 
                                         :style "width: 10%;text-align: center;" :aria-sort "ascending" 
                                         :aria-label "Likes : activate to sort column descending"
                                         "Likes")
                                    (:th :class "" :role "columnheader" :tabindex "0" 
                                         :aria-controls "DataTables_Table_0" :rowspan "1" :colspan "1" 
                                         :style "width: 10%;text-align: center;" :aria-sort "ascending" 
                                         :aria-label "Comments : activate to sort column descending"
                                         "Comments")
                                    (:th :class "" :role "columnheader" :tabindex "0" 
                                         :aria-controls "DataTables_Table_0" :rowspan "1" :colspan "1" 
                                         :style "width: 10%;text-align: center;" :aria-sort "ascending" 
                                         :aria-label "Mentions : activate to sort column descending"
                                         "Retweets")
                                    (:th :class "" :role "columnheader" :tabindex "0" 
                                         :aria-controls "DataTables_Table_0" :rowspan "1" :colspan "1" 
                                         :style "width: 10%;text-align: center;" :aria-sort "ascending" 
                                         :aria-label "Score : activate to sort column descending"
                                         "Score")))
                              (:tbody :role "alert" :aria-live "polite" :aria-relevant "all"
                                      (let ((count 1))
                                        (dolist (user user-stats)
                                          (htm
                                           (:tr :class (if (oddp count)
                                                           "odd"
                                                           "even")
                                                (:td :class "sorting_1"
                                                     (str (first user)))
                                                (:td :class "sorting_1" :style "text-align: center;"
                                                     (:span :class "badge"
                                                            (str (second (first (second user))))))
                                                (:td :class "sorting_1" :style "text-align: center;"
                                                     (:span :class (if (< 0 (second (second (second user))))
                                                                       "badge badge-success"
                                                                       "badge badge-important")
                                                            (str (second (second (second user))))))
                                                (:td :class "sorting_1" :style "text-align: center;"
                                                     (:span :class (if (< 0 (second (third (second user))))
                                                                       "badge badge-success"
                                                                       "badge badge-important")
                                                            (str (second (third (second user))))))
                                                (:td :class "sorting_1" :style "text-align: center;"
                                                     (:span :class (if (< 0 (second (fourth (second user))))
                                                                       "badge badge-success"
                                                                       "badge badge-important")
                                                            (str (second (fourth (second user))))))
                                                (:td :class "sorting_1" :style "text-align: center;"
                                                     (:span :class "badge badge-inverse"
                                                            (str (+
                                                                  ;;(second (first (second user)))
                                                                  (second (second (second user)))
                                                                  (second (third (second user)))
                                                                  (second (fourth (second user)))))))))
                                          (incf count))))))))))

(defun content-stats-html (content-stats)
  (with-html-string
    (:div :class "span12"
          (:div :class "nonboxy-widget"
                (:div :class "table_content"
                      (let ((content (sort content-stats 
                                           #'(lambda (x y)
                                               (let ((stat1 (third x))
                                                     (stat2 (third y)))
                                                 (> (+
                                                     (second (first stat1))
                                                     (second (second stat1))
                                                     (second (third stat1))
                                                                                  
                                                     )
                                                    (+
                                                     (second (first stat2))
                                                     (second (second stat2))
                                                     (second (third stat2)))))))))
                        (htm (:table :class " table table-bordered dataTable" ;;data-tbl-simple
                                     :id "DataTables_Table_0"
                                     (:thead
                                      (:tr :role "row"
                                           (:th :class "" :role "columnheader" :tabindex "0" 
                                                :aria-controls "DataTables_Table_0" :rowspan "1" :colspan "1" 
                                                :style "width: 10%;" :aria-sort "ascending" 
                                                :aria-label "Date & Time : activate to sort column descending"
                                                "Date & Time")
                                           (:th :class "" :role "columnheader" :tabindex "0" 
                                                :aria-controls "DataTables_Table_0" :rowspan "1" :colspan "1" 
                                                :style "width: 50%;" :aria-sort "ascending" 
                                                :aria-label "Post : activate to sort column descending"
                                                "Post")
                                                                                               
                                           (:th :class "" :role "columnheader" :tabindex "0" 
                                                :aria-controls "DataTables_Table_0" :rowspan "1" :colspan "1" 
                                                :style "width: 10%;" :aria-sort "ascending" 
                                                :aria-label "Likes : activate to sort column descending"
                                                "Likes")
                                           (:th :class "" :role "columnheader" :tabindex "0" 
                                                :aria-controls "DataTables_Table_0" :rowspan "1" :colspan "1" 
                                                :style "width: 10%;" :aria-sort "ascending" 
                                                :aria-label "Comments : activate to sort column descending"
                                                "Comments")
                                           (:th :class "" :role "columnheader" :tabindex "0" 
                                                :aria-controls "DataTables_Table_0" :rowspan "1" :colspan "1" 
                                                :style "width: 10%;" :aria-sort "ascending" 
                                                :aria-label "Mentions : activate to sort column descending"
                                                "Retweets")
                                           (:th :class "" :role "columnheader" :tabindex "0" 
                                                :aria-controls "DataTables_Table_0" :rowspan "1" :colspan "1" 
                                                :style "width: 10%;" :aria-sort "ascending" 
                                                :aria-label "Score : activate to sort column descending"
                                                "Score")))
                                     (:tbody :role "alert" :aria-live "polite" :aria-relevant "all"
                                             (let ((count 1))
                                               (dolist (action (if (stringp content)
                                                                   (if (> (length content) 10)
                                                                       (subseq content 0 10)
                                                                       content)
                                                                   content))
                                                 (let ((date (first action))
                                                       (text (second action))
                                                       (stats (third action)))
                                                   (htm
                                                    (:tr :class (if (oddp count)
                                                                    "odd"
                                                                    "even")
                                                         (:td :class "sorting_1"
                                                              (str date))
                                                         (:td :class "sorting_1"
                                                              (:span (str (format nil "~A"
                                                                                  (if (stringp text)
                                                                                      (if (> (length text) 70)
                                                                                          (format nil "~A..." (subseq text 0 70))
                                                                                          text)
                                                                                      text)))))
                                                         (:td :class "sorting_1" :style "text-align: center;"
                                                              (:span :class (if (< 0 (second (first stats)))
                                                                                "badge badge-success"
                                                                                "badge badge-important")
                                                                     (str (second (first stats)))))
                                                         (:td :class "sorting_1" :style "text-align: center;"
                                                              (:span :class (if (< 0 (second (second stats)))
                                                                                "badge badge-success"
                                                                                "badge badge-important")
                                                                     (str (second (second stats)))))
                                                         (:td :class "sorting_1" :style "text-align: center;"
                                                              (:span :class (if (< 0 (second (third stats)))
                                                                                "badge badge-success"
                                                                                "badge badge-important")
                                                                     (str (second (third stats)))))
                                                         (:td :class "sorting_1" :style "text-align: center;"
                                                              (:span :class "badge badge-inverse"
                                                                                                                  
                                                                     (str (+
                                                                           (second (first stats))
                                                                           (second (second stats))
                                                                           (second (third stats)))))))))
                                                 (incf count))
                                               ))))))))))


(defun mentioner-stats-html (mentioner-stats)
  (with-html-string
    (:div :class "span8"
          (:div :class "nonboxy-widget"
                (:div :class "table_content"
                      (:table :class " table table-bordered dataTable" ;;data-tbl-simple
                              :id "DataTables_Table_0"
                              (:thead
                               (:tr :role "row"
                                    (:th :class "" :role "columnheader" :tabindex "0" 
                                         :aria-controls "DataTables_Table_0" :rowspan "1" :colspan "1" 
                                         :style "width: 50%;" :aria-sort "ascending" 
                                         :aria-label "Mentioner : activate to sort column descending"
                                         "Mentioner")
                                   
                                    (:th :class "" :role "columnheader" :tabindex "0" 
                                         :aria-controls "DataTables_Table_0" :rowspan "1" :colspan "1" 
                                         :style "width: 10%;text-align: center;" :aria-sort "ascending" 
                                         :aria-label "Mentions : activate to sort column descending"
                                         "Mentions")))
                              (:tbody :role "alert" :aria-live "polite" :aria-relevant "all"
                                      (let ((count 1))
                                        (dolist (mentioner mentioner-stats)
                                          (htm
                                           (:tr :class (if (oddp count)
                                                           "odd"
                                                           "even")
                                                (:td :class "sorting_1"
                                                     (str (first mentioner)))
                                                
                                                (:td :class "sorting_1" :style "text-align: center;"
                                                     (:span :class "badge badge-inverse"
                                                            (str (second mentioner))))))
                                          (incf count))))))))))

(define-easy-handler (dashboard-page :uri "/dyb/dashboard") ()
  (multiple-value-bind (interval interval-start-date interval-end-date)
      (calc-date-interval)
    (let* ((page (make-widget 'page :name "dashboard-page"))
           (now (universal-today))
           (previous-interval-start-date 
             (- now (* +24h-secs+ (* interval 2))))
           (previous-interval-end-date 
             (- now (* +24h-secs+ interval)))
           (*calc-values* (set-calc-vals
                           interval-start-date
                           interval-end-date
                           previous-interval-start-date 
                           previous-interval-end-date)))
      (render page
              :body 
              (with-html-string
                (:div :class "container-fluid"
                      (str (interval-selection))
                      (:div :class "row-fluid"
                            (:div :class "nonboxy-widget"
                                  (:div :class "widget-head"
                                        (:h5 
                                         (str "Overview")))
                                  (:div :class "widget-content"
                                        (:div :class "widget-box"
                                              (:div :class "row-fluid"
                                                    (reach-small-graph)
                                                    (str (activity-small-graph))
                                                    (str (engagement-small-graph)))))))

                      (:div :class "row-fluid"
                            (:div :class "nonboxy-widget"
                                  (:div :class "widget-head"
                                        (:h5 
                                         (str "Network")))
                                  
                                  (:div :class "widget-content"
                                        (:div :class "widget-box"
                                              (:div :class "row-fluid"
                                                        
                                                    (network-size-graph
                                                     interval-start-date
                                                     interval-end-date
                                                     interval)
                                                       
                                                    (:div :class "span2"
                                                          (:div :class "summary"
                                                                (:h5 "Legend")
                                                                (:br)
                                                                (:ul
                                                               
                                                                 (:li
                                                                  (:span
                                                                   :style "width: 36px;height: 36px;float: left;margin-right: 10px;padding: 6px;"
                                                                   (:img :src "/appimg/Facebook_Light_Logo.png"
                                                                         :width "36" :height "36" :alt "")
                                                                   )
                                                                  )
                                                                 (:li
                                                                  (:span
                                                                   :style "width: 36px;height: 36px;float: left;margin-right: 10px;padding: 6px;"
                                                                   (:img :src "/appimg/twitter-bird-white-on-blue.png"
                                                                         :width "36" :height "36" :alt "")
                                                                   )
                                                                  )
                                                                 ))))))))

                       
                      (:div :class "row-fluid"
                            (:div :class "nonboxy-widget"
                                  (:div :class "widget-head"
                                        (:h5 
                                         (str "Engagement & Community")))

                                  (:div :class "widget-content"
                                        (:div :class "widget-box"
                                              (:div :class "row-fluid"
                                                    (str (engagement-pie-graph))
                                                    (:div :class "span2"
                                                          (str (current-community-size ))))))))
                      
                      (:div :class "row-fluid"
                            (:div :class "nonboxy-widget"
                                  (:div :class "widget-head"
                                        (:h5
                                         "Posts with Links"))

                                  (:div :class "widget-content"
                                        (:div :class "row-fluid"
                                              (str (posts-links-html)))))) 
  
                      (:div :class "row-fluid"
                            (:div :class "nonboxy-widget"
                                  (:div :class "widget-head"
                                        (:h5 (:i :class "black-icons facebook")
                                             "FACEBOOK"))

                                  (:div :class "widget-content"
                                        (:div :class "row-fluid"
                                              (fb-new-page-likes-graph-html
                                               interval-start-date
                                               interval-end-date
                                               interval))
                                        (:div :class "row-fluid"
                                              (fb-page-impressions-graph-html
                                               interval-start-date
                                               interval-end-date
                                               interval))
                                        (:div :class "row-fluid"
                                              (fb-total-fans-graph-html
                                               interval-start-date
                                               interval-end-date
                                               interval))
                                        (:div :class "row-fluid"
                                              (fb-page-unlikes-graph-html
                                               interval-start-date
                                               interval-end-date
                                               interval)) 

                                        )))
                        
                      (:div :class "row-fluid"
                            (:div :class "nonboxy-widget"
                                  (:div :class "widget-head"
                                        (:h5 (:i :class "black-icons twitter")
                                             "TWITTER"))

                                  (:div :class "widget-content"
                                        (:div :class "row-fluid"
                                                
                                              (tw-new-followers-graph-html
                                               interval-start-date
                                               interval-end-date
                                               interval))
                                        (:div :class "row-fluid"
                                              (tw-impressions-graph-html
                                               interval-start-date
                                               interval-end-date
                                               interval))
                                        (:div :class "row-fluid"
                                              (tw-total-fans-graph-html
                                               interval-start-date
                                               interval-end-date
                                               interval))
                                        (:div :class "row-fluid"
                                              (tw-un-followed-graph-html
                                               interval-start-date
                                               interval-end-date
                                               interval))
                                        )
                                  ))

                         
                      (:div :class "row-fluid"
                            (:div :class "nonboxy-widget"
                                  (:div :class "widget-head"
                                        (:h5 
                                         "User stats"))

                                  (:div :class "widget-content"
                                        (:div :class "widget-box"
                                              (:div :class "row-fluid"
                                                    (str (user-stats-html (gv 'users-stats))))))))
                      (:div :class "row-fluid"
                            (:div :class "nonboxy-widget"
                                  (:div :class "widget-head"
                                        (:h5 
                                         "Content Stats"))

                                  (:div :class "widget-content"
                                        (:div :class "widget-box"
                                              (:div :class "row-fluid"
                                                    (str (content-stats-html
                                                          (gv 'content-stats))))))))

                      (:div :class "row-fluid"
                            (:div :class "nonboxy-widget"
                                  (:div :class "widget-head"
                                        (:h5 
                                         "Mentioner Stats"))

                                  (:div :class "widget-content"
                                        (:div :class "widget-box"
                                              (:div :class "row-fluid"
                                                    (str (mentioner-stats-html 
                                                          (if (> (length (gv 'mentions-of-me-in-tweets-mentioner-count-range)) 10)
                                                              (subseq (gv 'mentions-of-me-in-tweets-mentioner-count-range)
                                                                      0 9)
                                                              (gv 'mentions-of-me-in-tweets-mentioner-count-range)))))))))
                        
                        
                      #|
                      (:div :class "row-fluid"
                      (:div :class "nonboxy-widget"
                      (:div :class "widget-head"
                      (:h5 (:i :class "black-icons twitter")
                      "Twitter"))

                      (:div :class "widget-content"
                      (:div :class "widget-box"
                      (:div :class "row-fluid"
                      (str (tw-new-followers-graph))
                      (str (tw-impressions-graph))
                      (str (tw-total-fans-graph))
                      (str (tw-un-followed-graph)))))))
                          
                      |#
                          
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
                                                 *calc-values*)))))) )
                  )))))

