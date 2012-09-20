(in-package :ems)

(bordeaux-threads:make-thread  
 (loop
    (sleep 600)
    (update-facebook-posts-for-users)))


#|
(trivial-timers:schedule-timer 
 (trivial-timers:make-timer #'update-facebook-posts-for-users)
 10
 :repeat-interval 360)
|#
