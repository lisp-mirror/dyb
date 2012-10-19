;;TESTS
(in-package :ems)

(defparameter xxx "")

(defun twitter-listener-x (service-user)
  (when service-user
    (when (get-val service-user 'last-access-token)
      (let ((result (twitter-home-timeline 
                     (get-val service-user 'last-access-token) 
                     (get-val service-user 'last-token-secret))))
        (json::decode-json-from-string (flexi-streams:octets-to-string result))))))

;;(setf xxx (twitter-listener-x (elt (service-users) 0)))

;;(twitter-refresh-home-timeline (elt (service-users) 0))


;;"Tue Oct 16 19:53:59 +0000 2012"



