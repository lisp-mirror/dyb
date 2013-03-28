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
  (:metaclass storable-class))

(defun error-log-collection ()
  (get-collection (system-db) "error-log"))

(defun error-logs ()
  (docs (error-log-collection)))

(defmethod doc-collection ((doc error-log))
  (error-log-collection))

(add-collection (system-db) "error-log" 
                :collection-class 'dyb-collection
                :load-from-file-p t)

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
                 condition))))

(defun start-task-thread (task-name function)
  (bordeaux-threads:make-thread  
   (lambda ()
     (block nil
       (handler-bind ((serious-condition
                        (lambda (condition)
                          (send-error-email condition task-name)
                          (log-error task-name condition)
                          (return))))
         (funcall function))))
   :name task-name))

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

;;;

(defun start-scheduler ()
  (when (equal *installation* "Live Serve")
    (start-actions-scheduler)
    (start-facebook-listener)
    (start-facebook-slow-listener)
    (start-twitter-mention-listener)
    (start-twitter-listener)
    (start-twitter-slow-listener)
    (start-linkedin-listener)
    (start-linkedin-slow-listener)
    (start-social-mention-slow-listener)))

;;TODO: handle disconnects etcs
;;(create-twitter-user-stream-listeners)

#|
(trivial-timers:schedule-timer 
 (trivial-timers:make-timer #'facebook-refresh-feeds)
 10
 :repeat-interval 360)
|#
