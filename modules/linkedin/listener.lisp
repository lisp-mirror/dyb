(in-package :dyb)

(defun linkedin-refresh-update (channel channel-user)
  (when channel-user
    (when (get-val channel-user 'last-access-token)
      (let (
            (result (if (string-equal (get-val channel-user 'profile-type) "Page")
                        (linkedin-company-updates
                         (get-val channel 'app-id)
                         (get-val channel 'app-secret)
                         (get-val channel-user 'last-access-token) 
                         (get-val channel-user 'last-token-secret)
                         (get-val channel-user 'user-id))
                        (linkedin-network-updates
                         (get-val channel 'app-id)
                         (get-val channel 'app-secret)
                         (get-val channel-user 'last-access-token) 
                         (get-val channel-user 'last-token-secret)))))
        
        (parse-linkedin-updates 
         channel-user
         (json::decode-json-from-string 
          (flexi-streams:octets-to-string result))
         'updates)))))

(defun linkedin-refresh-updates ()
  (let ((channel (get-social-channel "LinkedIn")))
    (dolist (user (coerce (channel-users) 'list ))
      (when (and user (string-equal (get-val user 'doc-status) "Active"))
        ;;TODO: How to get error messages in for users without access tokens.
        (when (string-equal (get-val user 'channel-user-type) "LinkedIn")
          (when (get-val user 'last-access-token)
            (linkedin-refresh-update channel user)))))))

(defun linkedin-refresh-connection (channel channel-user)
  (when channel-user
    (when (get-val channel-user 'last-access-token)
      (let ((message)
            (result (linkedin-connections
                     (get-val channel 'app-id)
                     (get-val channel 'app-secret)
                     (get-val channel-user 'last-access-token) 
                     (get-val channel-user 'last-token-secret))))

        (if (stringp result)
            (setf result (json::decode-json-from-string 
                          result))
            (setf result (json::decode-json-from-string 
                          (flexi-streams:octets-to-string result))))
        (if (assoc-path result :error)
            (setf message (cdr (assoc-path result :error :message))))
        (values result message)

        ))))

(defun linkedin-refresh-connections ()
  (let ((channel (get-social-channel "LinkedIn")))
    (dolist (user (coerce (channel-users) 'list ))
      (when (and user (string-equal (get-val user 'doc-status) "Active"))
        ;;TODO: How to get error messages in for users without access tokens.
        (when (string-equal (get-val user 'channel-user-type) "LinkedIn")
          (when (get-val user 'last-access-token)
            (multiple-value-bind (connections error)
                  (linkedin-refresh-connection channel user)
             ;; (break "~A" connections)
              (unless error
                (unless (get-val user 'user-data)
                  (setf (get-val user 'user-data) (make-hash-table :test 'equal)))
                
                (setf (gethash "connections"
                               (get-val user 'user-data))
                      connections)
                (persist user)))))))))

(defun linkedin-refresh-profile (channel channel-user)
  (when channel-user
    (when (get-val channel-user 'last-access-token)
      (let ((message)
            (result (linkedin-profile
                     (get-val channel 'app-id)
                     (get-val channel 'app-secret)
                     (get-val channel-user 'last-access-token) 
                     (get-val channel-user 'last-token-secret))))

        (if (stringp result)
            (setf result (json::decode-json-from-string 
                          result))
            (setf result (json::decode-json-from-string 
                          (flexi-streams:octets-to-string result))))
        (when (assoc-path result :error)
            (setf message (cdr (assoc-path result :error :message)))
            )
        (unless (assoc-path result :error)
            (setf (gethash "profile" (get-val channel-user 'user-data)) result)
            (persist channel-user))
        (values result message)

        ))))

(defun linkedin-refresh-profiles ()
  (let ((channel (get-social-channel "LinkedIn")))
    (dolist (user (coerce (channel-users) 'list ))
      (when (and user (string-equal (get-val user 'doc-status) "Active"))
        ;;TODO: How to get error messages in for users without access tokens.
        (when (string-equal (get-val user 'channel-user-type) "LinkedIn")
          (when (get-val user 'last-access-token)
            (multiple-value-bind (connections error)
                  (linkedin-refresh-profile channel user)
              (unless error
                (unless (get-val user 'user-data)
                  (setf (get-val user 'user-data) (make-hash-table :test 'equal)))
                
                (setf (gethash "profile"
                               (get-val user 'user-data))
                      connections)
                (persist user)))))))))

