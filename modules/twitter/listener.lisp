(in-package :dyb)


(defun twitter-refresh-user-followers (channel-user)
  (when channel-user
    (when (get-val channel-user 'last-access-token)
      (let* ((channel (get-social-channel (get-val channel-user 'channel-user-type)))
             (result (twitter-followers 
                      (get-val channel 'app-id)
                      (get-val channel 'app-secret)
                      (get-val channel-user 'last-access-token) 
                      (get-val channel-user 'last-token-secret)))
             (message))

        (if (stringp result)
            (setf result (json::decode-json-from-string 
                          result))
            (setf result (json::decode-json-from-string 
                          (flexi-streams:octets-to-string result))))
        (if (assoc-path result :errors)
            (setf message (cdr (assoc-path result :errors :message))))
        (values result message)))))

(defun twitter-refresh-followers ()
  (dolist (user (coerce (channel-users) 'list ))
      (when (and user (string-equal (get-val user 'doc-status) "Active"))
        ;;TODO: How to get error messages in for users without access tokens.
        (when (string-equal (get-val user 'channel-user-type) "Twitter")
          (when (get-val user 'last-access-token)
            (multiple-value-bind (followers error)
                (twitter-refresh-user-followers user)
              (unless error
                (unless (get-val user 'user-data)
                  (setf (get-val user 'user-data) (make-hash-table :test 'equal)))
                
                (setf (gethash "followers"
                               (get-val user 'user-data))
                      (list (assoc-path followers :ids)))
              (persist user))
              ))))))

(defun twitter-refresh-home-timelinex (channel-user)
  (when channel-user
    (when (get-val channel-user 'last-access-token)
      (let* ((channel (get-social-channel (get-val channel-user 'channel-user-type)))
            (result (twitter-home-timeline 
                     (get-val channel 'app-id)
                     (get-val channel 'app-secret)
                     (get-val channel-user 'last-access-token) 
                     (get-val channel-user 'last-token-secret))))
        (json::decode-json-from-string (flexi-streams:octets-to-string result))))))

(defun twitter-refresh-home-timeline (channel-user)
  (when channel-user
    (when (get-val channel-user 'last-access-token)
      (let* ((channel (get-social-channel (get-val channel-user 'channel-user-type)))
            (result (twitter-home-timeline
                     (get-val channel 'app-id)
                     (get-val channel 'app-secret)
                     (get-val channel-user 'last-access-token) 
                     (get-val channel-user 'last-token-secret))))
        (parse-tweets 
         channel-user
         (json::decode-json-from-string (flexi-streams:octets-to-string result))
         'home-timeline)))))

(defun twitter-refresh-home-timelines ()
  (dolist (user (coerce (channel-users) 'list ))
      (when (and user (string-equal (get-val user 'doc-status) "Active"))
        ;;TODO: How to get error messages in for users without access tokens.
        (when (string-equal (get-val user 'channel-user-type) "Twitter")
          (when (get-val user 'last-access-token)
            (let ((result (twitter-refresh-home-timeline user))) 
              (if result
                  (return-from twitter-refresh-home-timelines result))
              ))))))

(defun twitter-user-stream-listener (channel-user)
  (when channel-user
    (when (get-val channel-user 'last-access-token)
      (let* ((channel (get-social-channel (get-val channel-user 'channel-user-type)))
             (stream (twitter-user-stream  (get-val channel 'app-id)
                                           (get-val channel 'app-secret)
                                           (get-val channel-user 'last-access-token) 
                                           (get-val channel-user 'last-token-secret))))
        (when stream
            (loop for i below 200
               for line = (read-line stream nil nil) 
               when (and line (> 1 0) (> (length line) 2))
               do (parse-tweets 
                   channel-user
                   (list (json::decode-json-from-string line))
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


