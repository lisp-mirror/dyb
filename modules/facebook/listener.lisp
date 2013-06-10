(in-package :dyb)

(defun facebook-valid-user (channel-user)
  (if (and channel-user (string-equal (get-val channel-user 'doc-status) "Active"))
    (if (string-equal (get-val channel-user 'channel-user-type) "Facebook")
      (if (get-val channel-user 'last-access-token)
        channel-user))))

(defun paging (channel-user result result-function &key result-args)
  
  (let ((pages-read (make-hash-table :test 'equal)))
    (labels ( (paging-xx (channel-userx resultx result-functionx &key result-argsx)
              (when (and resultx result-functionx)
                (when (gpv resultx :paging :next)
               
                  (setf resultx (handle-endpoint channel-user
                                                (drakma-request
                                                 (gpv resultx :paging :next))))
                  (apply result-functionx channel-userx (if result-argsx
                                                          (list result-argsx (list resultx))
                                                          (list resultx)) )
                  (unless (gethash (gpv resultx :paging :next)  pages-read)
                  ;;  (break "~A "  pages-read)
                    (when (< (hash-table-count  pages-read) 3)
                      (setf (gethash (gpv resultx :paging :next)  pages-read) 
                            (gpv resultx :paging :next))
                      (paging-xx channel-userx resultx result-functionx)))))))
      (when (and result result-function)
        (paging-xx channel-user result result-function :result-argsx result-args)))))



(defun facebook-request-handler (channel-user request-function result-function 
                                 &key request-args result-args)
  (when (facebook-valid-user channel-user)
          (multiple-value-bind (result error)
              (apply request-function channel-user request-args)
            (unless error            
              (when (and result result-function)
              
                (apply result-function channel-user (if result-args
                                                        (list result-args (list result))
                                                        (list result)) )))
            ;;(break "~A" (gpv result :paging :next))
          ;;  (paging channel-user result result-function :result-args result-args)
            )))

(defun facebook-friends-refresh (channel-user)
  (facebook-request-handler channel-user
                            #'facebook-friends
                            #'update-generic-insight
                            :result-args "facebook-friends"))

(defun facebook-refresh-friends ()
  (dolist (user (coerce (channel-users) 'list ))
    (facebook-friends-refresh user)))

(defun parse-facebook-profile (channel-user)
  (facebook-request-handler channel-user
                            #'facebook-profile
                            #'update-generic-insight
                            :result-args "facebook-profile"))

(defun facebook-refresh-profiles ()
  (dolist (user (coerce (channel-users) 'list ))
    (parse-facebook-profile user)))


(defun facebook-page-insights-refresh (channel-user last-date)
  (facebook-request-handler channel-user
                            #'facebook-page-insights
                            #'parse-facebook-insights
                            :request-args (list (if last-date
                                                    (if (> last-date 0)
                                                        last-date))
                                                (if last-date
                                                    (if (> last-date 0)
                                                        (+ last-date (* 60 60 24)))))))

(defun facebook-refresh-page-insights ()
  (dolist (user (coerce (channel-users) 'list ))
    (when (and user (string-equal (get-val user 'doc-status) "Active"))
      (when (string-equal (get-val user 'channel-user-type) "Facebook")
        (when (get-val user 'last-access-token)
          (let ((last-date (get-last-insight-date user "page_fan_adds")))
            (facebook-page-insights-refresh 
             user 
             (if (> last-date 0)
                 last-date))))))))

(defun facebook-page-insights-history (from-date days)
  (when (and from-date days)
    (dolist (channel-user (coerce (channel-users) 'list ))       
      (when (facebook-valid-user channel-user)
            (loop for i from 0 to (- days 1)
               do
               (facebook-page-insights-refresh 
                channel-user 
                (+ from-date (* 60 60 24 i))))))))

(defun get-last-post-date (user)
  (let ((date 0))
    (find-docs 'list 
               (lambda (doc)
                 (when (string-equal (get-val doc 'post-type) "Facebook")
                   (when (get-val doc 'channel-user)
                     (typecase  (get-val doc 'channel-user)
                       (channel-user 
                        (if (string-equal (get-val (get-val doc 'channel-user) 
                                                   'channel-user-name) 
                                          (get-val user 'channel-user-name))
                            (if (> (get-val doc 'created-date) date)
                                (setf date (get-val doc 'created-date)))))))))
               (generic-post-collection))
    date))

(defun facebook-refresh-feeds ()
  (dolist (channel-user (coerce (channel-users) 'list ))
    (when (facebook-valid-user channel-user)
          (let ((last-date (get-last-post-date channel-user)))
            (multiple-value-bind (posts error)
                (facebook-feed channel-user (if (> last-date 0)
                                        last-date))
              (unless error
                (parse-facebook-posts channel-user posts 'facebook-feed)))))))
