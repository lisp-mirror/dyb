(in-package :ems)

(defun start-facebook-listener ()
  (bordeaux-threads:make-thread  
   (lambda ()
     (loop
        (sleep 600)
        (update-facebook-posts-for-users)))))

;;(start-facebook-listener)

;(update-social-mention-for-searches)

;;(listen-twitter-users)

#|
(trivial-timers:schedule-timer 
 (trivial-timers:make-timer #'update-facebook-posts-for-users)
 10
 :repeat-interval 360)
|#
