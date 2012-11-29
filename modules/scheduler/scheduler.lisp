(in-package :dyb)


(defun start-actions-scheduler ()
  (bordeaux-threads:make-thread  
   (lambda ()
     (loop
        (sleep 120)
        (post-scheduled-actions)))))

(start-actions-scheduler)

(defun start-facebook-listener ()
  (bordeaux-threads:make-thread  
   (lambda ()
     (loop
        (sleep 600)
        (facebook-refresh-feeds)))))

(start-facebook-listener)

(defun start-facebook-slow-listener ()
  (bordeaux-threads:make-thread  
   (lambda ()
     (loop
        (sleep 86400)
      
        (facebook-refresh-friends)
        (facebook-refresh-profiles)))))

(start-facebook-slow-listener)

(defun start-twitter-listener ()
  (bordeaux-threads:make-thread  
   (lambda ()
     (loop
        (sleep 600)
        (twitter-refresh-home-timelines)
        
        ))))

(start-twitter-listener)

(defun start-twitter-slow-listener ()
  (bordeaux-threads:make-thread  
   (lambda ()
     (loop
        (sleep 86400)
        (twitter-refresh-followers)
        (twitter-refresh-profiles)))))

(start-twitter-slow-listener)


(defun start-linkedin-listener ()
  (bordeaux-threads:make-thread  
   (lambda ()
     (loop
        (sleep 600)
      (linkedin-refresh-updates)))))

(start-twitter-listener)

(defun start-linkedin-slow-listener ()
  (bordeaux-threads:make-thread  
   (lambda ()
     (loop
        (sleep 86400)
      (linkedin-refresh-connections)
      (linkedin-refresh-profiles)))))

(start-twitter-slow-listener)

;;TODO: handle disconnects etcs
;;(create-twitter-user-stream-listeners)

#|
(trivial-timers:schedule-timer 
 (trivial-timers:make-timer #'facebook-refresh-feeds)
 10
 :repeat-interval 360)
|#
