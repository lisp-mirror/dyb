;;TESTS
(in-package :ems)

(defparameter xxx "")

(defun twitter-listener-x (channel-user)
  (when channel-user
    (when (get-val channel-user 'last-access-token)
      (let ((result (twitter-home-timeline 
                     (get-val channel-user 'last-access-token) 
                     (get-val channel-user 'last-token-secret))))
        (json::decode-json-from-string (flexi-streams:octets-to-string result))))))

;;(setf xxx (twitter-listener-x (elt (channel-users) 0)))

;;(twitter-refresh-home-timeline (elt (channel-users) 0))


;;"Tue Oct 16 19:53:59 +0000 2012"



