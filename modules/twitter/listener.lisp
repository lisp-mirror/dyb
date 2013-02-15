(in-package :dyb)


(defun twitter-valid-user (channel-user)
  (if (and channel-user (string-equal (get-val channel-user 'doc-status) "Active"))
    (if (string-equal (get-val channel-user 'channel-user-type) "Twitter")
      (if (get-val channel-user 'last-access-token)
        channel-user))))

(defun twitter-request-handler (channel-user request-function result-function 
                                 &key request-args result-args)
  (when (facebook-valid-user channel-user)
          (multiple-value-bind (result error)
              (apply request-function channel-user request-args)
            (unless error            
              (when (and result result-function)
              
                (apply result-function channel-user (if result-args
                                                        (list result-args (list result))
                                                        (list result)) ))))))

(defun twitter-refresh-user-profiles (channel-user)
  (when channel-user
    (when (get-val channel-user 'last-access-token)
      (twitter-profile
       channel-user))))

(defun twitter-refresh-profiles ()
  (dolist (user (coerce (channel-users) 'list ))
      (when (twitter-valid-user user)
            (multiple-value-bind (profile error)
                (twitter-refresh-user-profiles user)
              (unless error
                (when profile
                  (let* ((end-time (universal-today))
                         (dup (get-generic-insight-value 
                               user 
                               "twitter-profile" end-time)))
                    (when (or (not dup) (not (get-val dup 'value)))
                      (when dup
                        (setf (get-val dup 'value) profile)
                        (setf (get-val dup 'end-time) end-time)
                        (persist dup))
                      (unless dup
                        (persist (make-generic-insight-value 
                                  user 
                                  "twitter-profile"
                                  profile
                                  end-time)))))))))))

(defun twitter-refresh-user-followers (channel-user)
  (when channel-user
    (twitter-followers 
     channel-user)))

(defun twitter-refresh-followers ()
  (dolist (user (coerce (channel-users) 'list ))
      (when (twitter-valid-user user)
            (multiple-value-bind (followers error)
                (twitter-refresh-user-followers user)
              (unless error
                (when followers
                  (let* ((end-time (universal-today))
                         (dup (get-generic-insight-value 
                               user 
                               "twitter-followers" end-time)))
                    (when (or (not dup) (not (get-val dup 'value)))
                      (when dup
                        (setf (get-val dup 'value) (list (assoc-path followers :ids)))
                        (setf (get-val dup 'end-time) end-time)
                        (persist dup))
                      (unless dup
                        (persist (make-generic-insight-value 
                                  user 
                                  "twitter-followers"
                                  (list (assoc-path followers :ids))
                                  end-time)))))))))))

(defun twitter-last-tweet-id (channel-user &key time-line)
  (let ((tweet-id 1))
    (map-docs 'list
              (lambda (doc)
                (when (string-equal (get-val doc 'post-type) "Twitter")
                  (when (get-val doc 'channel-user)
                  
                    (typecase (get-val doc 'channel-user)
                      (channel-user
                       (when (string-equal 
                              (get-val (get-val doc 'channel-user) 
                                       'channel-user-name)
                              (get-val channel-user 'channel-user-name))
                         (when (> (raw-post-id doc 'twitter) tweet-id)
                           (if time-line
                               (if (equal time-line (get-val doc 'payload-source))
                                   (setf tweet-id (raw-post-id doc 'twitter)))
                               (setf tweet-id (raw-post-id doc 'twitter)))
                             nil)))))))
               (generic-post-collection))
    tweet-id))

(defun twitter-first-tweet-id (channel-user &key time-line)
  (let ((tweet-id 1))
    (map-docs 'list
              (lambda (doc)
                (when (string-equal (get-val doc 'post-type) "Twitter")
                  (when (get-val doc 'channel-user)
                  
                    (typecase (get-val doc 'channel-user)
                      (channel-user
                       (when (string-equal 
                              (get-val (get-val doc 'channel-user) 
                                       'channel-user-name)
                              (get-val channel-user 'channel-user-name))
                         (if (= tweet-id 1)
                             (setf tweet-id (raw-post-id doc 'twitter)))
                         (when (< (raw-post-id doc 'twitter) tweet-id)
                           (if time-line
                               (if (equal time-line (get-val doc 'payload-source))
                                   (setf tweet-id (raw-post-id doc 'twitter)))
                               (setf tweet-id (raw-post-id doc 'twitter)))
                             nil)))))))
               (generic-post-collection))
    tweet-id))


(defun twitter-refresh-home-timeline (channel-user)
  (when channel-user
    (when (get-val channel-user 'last-access-token)
       (let* ((since-id (twitter-last-tweet-id channel-user :time-line 'home-timeline))
             (result (twitter-home-timeline
                      channel-user  :since-id since-id)))
        (parse-tweets 
         channel-user
         result
         'home-timeline)))))

(defun twitter-refresh-home-timeline-history (channel-user)
  (when channel-user
    (when (get-val channel-user 'last-access-token)
      (let* ((max-id (twitter-first-tweet-id channel-user :time-line 'home-timeline))
             (result (twitter-home-timeline
                      channel-user  :max-id max-id)))
        (parse-tweets 
         channel-user
         result
         'home-timeline)))))

(defun twitter-refresh-home-timelines-history ()
  (dolist (user (coerce (channel-users) 'list ))
      (when (and user (string-equal (get-val user 'doc-status) "Active"))
        ;;TODO: How to get error messages in for users without access tokens.
        (when (string-equal (get-val user 'channel-user-type) "Twitter")
          (when (get-val user 'last-access-token)

            (twitter-refresh-home-timeline-history user)
            )))))


(defun twitter-refresh-home-timelines ()
  (dolist (user (coerce (channel-users) 'list ))
      (when (twitter-valid-user user)

            (twitter-refresh-home-timeline user)
            )))

(defun twitter-refresh-mention-timeline (channel-user)
  (when channel-user
    (when (get-val channel-user 'last-access-token)
       (let* ((since-id (twitter-last-tweet-id channel-user :time-line 'mention-timeline))
             (result (twitter-mention-timeline
                      channel-user  :since-id since-id)))
        (parse-tweets 
         channel-user
         result
         'mention-timeline)))))

(defun twitter-refresh-mention-timelines ()
  (dolist (user (coerce (channel-users) 'list ))
      (when (twitter-valid-user user)
            (twitter-refresh-mention-timeline user))))

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
               do ;; (break "~A" line) 
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


