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

(defun log-error (task-name condition)
  (persist (make-instance 'error-log
                          :task-name task-name
                          :condition-class (class-name (class-of condition))
                          :backtrace
                          (with-output-to-string (*debug-io*)
                            (sb-debug:backtrace))
                          :printed-condition (princ-to-string condition))))

;;;

(defun start-task-thread (task-name function)
  (bordeaux-threads:make-thread  
   (lambda ()
     (loop
      (sleep 3)
      (block nil
        (handler-bind ((serious-condition
                         (lambda (condition)
                           (log-error task-name condition)
                           (return))))
          (funcall function)))))
   :name task-name))

(defun start-actions-scheduler ()
  (start-task-thread
   "post-scheduled-actions"
   (lambda ()
     (loop
      (post-scheduled-actions)
      (sleep 120)))))
(if (string-equal *installation* "Live Serve")
    (start-actions-scheduler))

(defun start-facebook-listener ()
  (start-task-thread
   "facebook-refresh-feeds"
   (lambda ()
     (loop
      (facebook-refresh-feeds)
      (sleep 600)))))
(if (string-equal *installation* "Live Serve")
    (start-facebook-listener))

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
(if (string-equal *installation* "Live Serve")
    (start-facebook-slow-listener))

(defun start-twitter-listener ()
  (start-task-thread
   "twitter-refresh-home-timelines"
   (lambda ()
     (loop
        (twitter-refresh-home-timelines)
        
      (sleep 600)))))
(if (string-equal *installation* "Live Serve")
    (start-twitter-listener))

(defun start-twitter-mention-listener ()
  (start-task-thread
   "twitter-refresh-home-timelines"
   (lambda ()
     (loop
        
        (twitter-refresh-mention-timelines)
      (sleep 600)))))
(if (string-equal *installation* "Live Serve")
    (start-twitter-mention-listener))

(defun start-twitter-slow-listener ()
  (start-task-thread
   "twitter-refresh-followers-and-profiles"
   (lambda ()
     (loop
      (twitter-refresh-followers)
      (twitter-refresh-profiles)
      (sleep 86400)))))
(if (string-equal *installation* "Live Serve")
    (start-twitter-slow-listener))


(defun start-linkedin-listener ()
  (start-task-thread
   "linkedin-refresh-updates"
   (lambda ()
     (loop
      (linkedin-refresh-updates)
      (sleep 600)))))
(if (string-equal *installation* "Live Serve")
    (start-twitter-listener))

(defun start-linkedin-slow-listener ()
  (start-task-thread
   "linkedin-refresh-connections-and-profiles"
   (lambda ()
     (loop
      (linkedin-refresh-connections)
      (linkedin-refresh-profiles)
      (sleep 86400)))))
(if (string-equal *installation* "Live Serve")
    (start-twitter-slow-listener))


(defun start-social-mention-slow-listener ()
  (start-task-thread
   "facebook-refresh-friends-and-profiles"
   (lambda ()
     (loop
      (social-mention-refresh-searches)     
      (sleep 86400)))))
(if (string-equal *installation* "Live Serve")
    (start-social-mention-slow-listener))



;;TODO: handle disconnects etcs
;;(create-twitter-user-stream-listeners)

#|
(trivial-timers:schedule-timer 
 (trivial-timers:make-timer #'facebook-refresh-feeds)
 10
 :repeat-interval 360)
|#
