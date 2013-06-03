(in-package :dyb)

(defclass error-log (doc)
  ((task-name :initarg :task-name
              :initform nil
              :accessor task-name)
   (condition-class :initarg :condition-class
                    :initform nil
                    :accessor condition-class)
   (printed-condition :initarg :printed-condition
                      :initform nil
                      :accessor printed-condition)
   (backtrace :initarg :backtrace
              :initform nil
              :accessor backtrace))
  (:metaclass storable-versioned-class))

(defun error-log-collection ()
  (get-collection (system-db) "error-log"))

(defun error-logs ()
  (docs (error-log-collection)))

(defmethod doc-collection ((doc error-log))
  (error-log-collection))

(add-collection (system-db) "error-log" 
                :collection-class 'dyb-collection)

(defun short-backtrace ()
  (let* ((*package* (find-package :cl))
         (string
           (with-output-to-string (string)
             (sb-debug::map-backtrace
              (lambda (x)
                (print (sb-di:debug-fun-name (sb-di:frame-debug-fun x))
                       string)))) ))
    (if (>= (length string) 65535)
        (subseq string 0 65535)
        string)))

(defun log-error (task-name condition)
  (persist (make-instance 'error-log
                          :task-name task-name
                          :condition-class (class-name (class-of condition))
                          :backtrace (short-backtrace)
                          :printed-condition (princ-to-string condition))))

;;;

(defun send-error-email (condition task-name)
  (handler-case
      (send-system-mail
       (frmt "[DYB]: scheduler error in task ~s." task-name)
       (frmt "The task ~s has been stopped at ~a due to the following error:

~a

   [Condition of type ~a]

~a"
             task-name
             (current-date-time)
             condition
             (type-of condition)
             (with-output-to-string (*debug-io*) (sb-debug:backtrace))))
    (error (c)
      (log-error (frmt "Failed to send email for error in ~s" task-name)
                 c))))

(defun start-task-thread (task-name function)
  (bordeaux-threads:make-thread
   (lambda ()
     (block nil
       (tagbody
        :retry
          (handler-bind ((usocket:timeout-error
                           (lambda (c)
                             (declare (ignore c))
                             (go :retry)))
                         (serious-condition
                           (lambda (condition)
                             (send-error-email condition task-name)
                             (log-error task-name condition)
                             (return))))
            (funcall function)))))
   :name task-name))

(defun post-scheduled-action (action)
  (with-slots (action-status post-type action-type
                             post-id) action
    (when (and action
               (equal action-status "Pending"))
      (let ((from-user (or (get-channel-user-by-user-name
                            (get-val action 'from-user-id)
                            (get-val action 'post-type))
                           (get-channel-user-by-user-id
                            (get-val action 'from-user-id)
                            (get-val action 'post-type)))))
        (multiple-value-bind (result error-message)
            (cond ((equal post-type "Facebook")
                   (cond ((equal action-type "Post")
                          (cond
                            ((and (not-empty-p (get-val action 'image-url))
                                  (not-empty-p (get-val action 'post-url)))
                             (post-facebook-link-image 
                              (get-val action 'from-user-id)
                              (or (get-val action 'processed-content)
                                  (get-val action 'action-content))
                              (or (format-short-url
                                   (get-val action 'short-url))
                                  (get-val action 'post-url))
                              (get-val action 'image-url)))

                            ((not-empty-p (get-val action 'image-url))
                             (post-facebook-image (get-val action 'from-user-id)
                                                  (or (get-val action 'processed-content)
                                                      (get-val action 'action-content))
                                                  (get-val action 'image-url)))
                            ((or (not-empty-p (get-val action 'short-url))
                                 (not-empty-p (get-val action 'post-url)))
                             (post-facebook-url (get-val action 'from-user-id)
                                                (or (get-val action 'processed-content)
                                                    (get-val action 'action-content))
                                                (or (format-short-url
                                                     (get-val action 'short-url))
                                                    (get-val action 'post-url))))
                            (t
                             (post-facebook (get-val action 'from-user-id)
                                            (or (get-val action 'processed-content)
                                                (get-val action 'action-content))))))
                         ((equal action-type "Comment")
                          (comment-facebook from-user
                                            post-id
                                            ;;  (get-val action 'from-user-id)
                                            (or (get-val action 'processed-content)
                                                (get-val action 'action-content))))
                         ((equal action-type "Like")
                          (facebook-like from-user  post-id
                                         ;;(get-val action 'from-user-id)
                                         ))))

                  ((equal post-type "Twitter")
                   (cond ((or (equal action-type "Tweet")
                              (equal action-type "Post"))
                          (post-twitter 
                           from-user
                           (or (get-val action 'processed-content)
                               (get-val action 'action-content))
                           :image-path (get-val action 'image-url)
                           ))
                         ((equal action-type "Retweet")
                          (retweet from-user post-id))
                         ((equal action-type "Reply")
                          (tweet-reply    
                           from-user
                           (or (get-val action 'processed-content)
                               (get-val action 'action-content))
                           ;;TODO: Get right user name
                           (get-val action 'to-user-id)
                            post-id
                           ))
                         ((equal action-type "Favourite")
                          (twitter-favourite from-user post-id))))
                  ((equal post-type "LinkedIn")
                   (cond ((equal action-type "Post")
                          (if (string-equal (get-val from-user 'profile-type) "Page")
                              (linkedin-company-share
                               from-user
                               (or (get-val action 'processed-content)
                                   (get-val action 'action-content))
                               :submited-url (or (format-short-url
                                                  (get-val action 'short-url))
                                                 (get-val action 'post-url))
                               :submitted-image-url (get-val action 'post-url))
                              (linkedin-share
                               from-user
                               (or (get-val action 'processed-content)
                                   (get-val action 'action-content))
                               :submited-url (or (format-short-url (get-val action 'short-url))
                                                 (get-val action 'post-url))
                               :submitted-image-url (get-val action 'post-url)))))))
          (if error-message
              (add-generic-action-log action "Error" error-message "Pending")
              (add-generic-action-log action "Result" result "Completed")))))))

(defun post-scheduled-actions ()
  (loop for action across (generic-actions)
        when (equal (action-status action) "Pending")
        do
        (let ((now (get-universal-time)))
          (typecase action
            (generic-action
             (when (< (scheduled-date action) now)
               (block nil
                 (handler-bind
                     ((serious-condition
                        (lambda (condition)
                          (cond ((>= (length (action-log action)) 5)
                                 (setf (action-status action) "Abandoned Retries")
                                 (persist action)
                                 (log-error (frmt "post-scheduled-actions ~a ~a"
                                                  (post-type action)
                                                  (action-type action))
                                            condition)
                                 (post-action-abandoned-email action))
                                (t
                                 (add-generic-action-log action
                                                         "Error"
                                                         (princ-to-string condition)
                                                         "Pending")))
                          (return))))
                   (post-scheduled-action action)))))))))

(defun start-actions-scheduler ()
  (start-task-thread
   "post-scheduled-actions"
   (lambda ()
     (loop
      (post-scheduled-actions)
      (sleep 120)))))

(defun start-facebook-listener ()
  (start-task-thread
   "facebook-refresh-feeds"
   (lambda ()
     (loop
      (facebook-refresh-feeds)
      (sleep 600)))))

(defun start-facebook-slow-listener ()
  (start-task-thread
   "facebook-refresh-friends-and-profiles"
   (lambda ()
     (loop
      (facebook-refresh-friends)
      (facebook-refresh-profiles)
      (facebook-page-insights-history
       (- (universal-today) (* 60 60 24 10))
       10)
      (sleep 86400)))))

(defun start-twitter-listener ()
  (start-task-thread
   "twitter-refresh-home-timelines"
   (lambda ()
     (loop
      (twitter-refresh-home-timelines)
      (sleep 600)))))

(defun start-twitter-mention-listener ()
  (start-task-thread
   "twitter-refresh-mention-timelines"
   (lambda ()
     (loop
      (twitter-refresh-mention-timelines)
      (sleep 600)))))

(defun start-twitter-slow-listener ()
  (start-task-thread
   "twitter-refresh-followers-and-profiles"
   (lambda ()
     (loop
      (twitter-refresh-followers)
      (twitter-refresh-profiles)
      (sleep 86400)))))

(defun start-linkedin-listener ()
  (start-task-thread
   "linkedin-refresh-updates"
   (lambda ()
     (loop
      (linkedin-refresh-updates)
      (sleep 600)))))

(defun start-linkedin-slow-listener ()
  (start-task-thread
   "linkedin-refresh-connections-and-profiles"
   (lambda ()
     (loop
      (linkedin-refresh-connections)
      (linkedin-refresh-profiles)
      (sleep 86400)))))


(defun start-social-mention-slow-listener ()
  (start-task-thread
   "facebook-refresh-friends-and-profiles"
   (lambda ()
     (loop
      (social-mention-refresh-searches)
      (sleep 86400)))))

(defun count-lost-actions ()
  (loop with now = (get-universal-time)
        for action across (generic-actions)
        count (and (equal (action-status action) "Pending")
                   (> (- now (scheduled-date action)) (* 15 60)))))

(defun action-monitor ()
  (let ((lost-actions (count-lost-actions)))
    (when (plusp lost-actions)
      (send-error-email
       (frmt "[DYB]: lost ~a scheduled post~:p" lost-actions)
       (frmt "I'm sorry to inform you but there ~[are~;is~:;are~]~:* ~
              ~a action~:p pining for the fjords."
             lost-actions)))))

(defun start-action-monitor ()
  (start-task-thread
   "action-monitor"
   (lambda ()
     (loop
      (sleep (* 30 60))
      (action-monitor)))))
;;;

(defun start-scheduler ()
  (when (equal *installation* "Live Serve")
    (start-actions-scheduler)
    (start-action-monitor)
    (start-facebook-listener)
    (start-facebook-slow-listener)
    (start-twitter-mention-listener)
    (start-twitter-listener)
    (start-twitter-slow-listener)
;;    (start-linkedin-listener)
;;    (start-linkedin-slow-listener)
    (start-social-mention-slow-listener)))

;;TODO: handle disconnects etcs
;;(create-twitter-user-stream-listeners)

#|
(trivial-timers:schedule-timer
 (trivial-timers:make-timer #'facebook-refresh-feeds)
 10
 :repeat-interval 360)
|#
