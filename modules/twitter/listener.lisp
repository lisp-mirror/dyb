(in-package :ems)


(defun twitter-refresh-home-timelinex (channel-user)
  (when channel-user
    (when (get-val channel-user 'last-access-token)
      (let ((result (twitter-home-timeline (get-val channel-user 'last-access-token) 
                                           (get-val channel-user 'last-token-secret))))
        (json::decode-json-from-string (flexi-streams:octets-to-string result))))))

(defun twitter-refresh-home-timeline (channel-user)
  (when channel-user
    (when (get-val channel-user 'last-access-token)
      (let ((result (twitter-home-timeline (get-val channel-user 'last-access-token) 
                                           (get-val channel-user 'last-token-secret))))
        (parse-tweets 
         (json::decode-json-from-string (flexi-streams:octets-to-string result))
         'home-timeline)))))

(defun twitter-refresh-home-timelines ()
  (dolist (user (coerce (channel-users) 'list ))
        (when (and user (string-equal (get-val user 'doc-status) "Active"))
          ;;TODO: How to get error messages in for users without access tokens.
          (when (string-equal (get-val user 'channel-user-type) "Twitter")
            (when (get-val user 'last-access-token)
              (twitter-refresh-home-timeline user))))))

(defun twitter-user-stream-listener (channel-user)
  (when channel-user
    (when (get-val channel-user 'last-access-token)
      (let ((stream (twitter-get-stream (get-val channel-user 'last-access-token) (get-val channel-user 'last-token-secret))))
        (when stream
            (loop for i below 200
               for line = (read-line stream nil nil) 
               when (and line (> 1 0) (> (length line) 2))
               do (parse-tweets (list (json::decode-json-from-string line))
                                'user-stream))
            (close stream)
            ;;(values)
            )))))

(defun create-twitter-user-stream-listener (user)
  (bordeaux-threads:make-thread  
   (lambda ()
     (loop
        ;(sleep 600)
        (twitter-user-stream-listener user)))))

(defun create-twitter-user-stream-listners ()
  (dolist (user (coerce (channel-users) 'list ))
        (when (and user (string-equal (get-val user 'doc-status) "Active"))
          ;;TODO: How to get error messages in for users without access tokens.
          (when (string-equal (get-val user 'channel-user-type) "Twitter")
            (when (get-val user 'last-access-token)
              (create-twitter-user-stream-listener user))))))


