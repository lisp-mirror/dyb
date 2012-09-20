(in-package :ems)

(trivial-timers:schedule-timer 
 (trivial-timers:make-timer #'update-facebook-posts-for-users)
 10
 :repeat-interval 360)


