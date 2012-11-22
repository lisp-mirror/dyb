(in-package :dyb)

(defclass some-widget (ajax-widget)())

(defmethod render ((widget some-widget) &key)
(if (parameter "some-param1")
    (with-html :href
               (fmt "Hello ~a" (parameter "some-param")))))

(define-easy-handler (ajax-widget-page :uri "/dyb/test-ajax") ()
  (let ((page (make-widget 'html-framework-page
                           :name "ajax-test"))
        (widget (make-widget 'some-widget :name "eish")))
    (render page
            :body
            (with-html-string
              (:a :href
                  (js-link 
                   (js-render widget
                              (js-pair "some-param" "testing")
                              (js-pair "some-param1" "fuck")))
                  (make-icon "card--pencil"
                             :title "Hello World Example."))
              (render widget)))))


(define-easy-handler (manual-updates-page :uri "/dyb/manual-updates") ()
  (let ((page (make-widget 'html-framework-page
                           :name "ajax-test"))
        (result))
    (when (parameter "get-facebook-data")
      (facebook-refresh-feeds)
      (facebook-refresh-friends)
      (facebook-refresh-profiles))
    (when (parameter "get-search-stream-data")
      (social-mention-refresh-searches))
    (when (parameter "schedule-actions")
      (post-scheduled-actions))
    (when (parameter "get-twitter-old")
      (setf result (twitter-refresh-home-timelines))
      (twitter-refresh-followers)
      (twitter-refresh-profiles))
    (when (parameter "get-linkedin-updates")
      (linkedin-refresh-updates)
      (linkedin-refresh-connections)
      (linkedin-refresh-profiles)
      )

    

    (render page :body
            (with-html-to-string ()
              (:form :name "fetch-data"
                     :method :post
                     (:input :type "submit" :name "get-facebook-data" 
                             :value "Get Facebook Data"))
              (:form :name "fetch-data"
                     :method :post
                     (:input :type "submit" :name "get-search-stream-data" 
                             :value "Get Search Stream Data"))
              (:form :name "fetch-data"
                     :method :post
                     (:input :type "submit" :name "schedule-actions" 
                             :value "Schedule Actions"))
              (:form :name "fetch-data"
                     :method :post
                     (:input :type "submit" :name "get-twitter-old" 
                             :value "Get Tweets"))
              (:form :name "fetch-data"
                     :method :post
                     (:input :type "submit" :name "get-linkedin-updates" 
                             :value "Get LinkedIn Updates"))

              (:p (:strong (str result)))))))