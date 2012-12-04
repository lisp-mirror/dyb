(in-package :dyb)

(defun twitter-refresh-user-profiles (channel-user)
  (when channel-user
    (when (get-val channel-user 'last-access-token)
      (twitter-profile
       channel-user))))

(defun twitter-refresh-profiles ()
  (dolist (user (coerce (channel-users) 'list ))
      (when (and user (string-equal (get-val user 'doc-status) "Active"))
        ;;TODO: How to get error messages in for users without access tokens.
        (when (string-equal (get-val user 'channel-user-type) "Twitter")
          (when (get-val user 'last-access-token)
            (multiple-value-bind (profile error)
                (twitter-refresh-user-profiles user)
              (unless error
                (unless (get-val user 'user-data)
                  (setf (get-val user 'user-data) (make-hash-table :test 'equal)))
                
                (setf (gethash "profile"
                               (get-val user 'user-data))
                      profile)
              (persist user))
              ))))))

(defun twitter-refresh-user-followers (channel-user)
  (when channel-user
    (twitter-followers 
     channel-user)))

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
              (persist user))))))))

(defun twitter-last-tweet-id (channel-user)
  (let ((tweet-id 1))
    (map-docs 'list
              (lambda (doc)
                (when (string-equal 
                       (get-val (get-val doc 'channel-user) 
                                'channel-user-name)
                       (get-val channel-user 'channel-user-name))
                  (when (string-equal (get-val doc 'post-type) "Twitter")
                    (when (> (raw-post-id doc 'twitter) tweet-id)
                      (setf tweet-id (raw-post-id doc 'twitter))
                      nil))))
               (generic-post-collection))
    tweet-id))


(defun twitter-refresh-home-timeline (channel-user)
  (when channel-user
    (when (get-val channel-user 'last-access-token)
      
      
      (let* ((since-id (twitter-last-tweet-id channel-user))
             (result (twitter-home-timeline
                      channel-user  :since-id since-id)))
        (parse-tweets 
         channel-user
         result
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
      (let* ((channel (get-social-channel (get-val channel-user 'channel-user-type)))
             (stream (twitter-user-stream  (get-val channel 'app-id)
                                           (get-val channel 'app-secret)
                                           (get-val channel-user 'last-access-token) 
                                           (get-val channel-user 'last-token-secret))))
        (when stream
            (loop for i below 200
               for line = (read-line stream nil nil) 
               when (and line (> 1 0) (> (length line) 2))
               do (break "~A" line) 
                 (parse-tweets 
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

(defun create-twitter-user-stream-listeners ()
  (dolist (user (coerce (channel-users) 'list ))
        (when (string-equal (get-val user 'channel-user-type) "Twitter")
            (when (get-val user 'last-access-token)
              (create-twitter-user-stream-listener user)))))


