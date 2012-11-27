(in-package :dyb)

(defun linkedin-refresh-update (channel-user)
  (when channel-user
    (when (get-val channel-user 'last-access-token)
      (let ((result (if (string-equal (get-val channel-user 'profile-type) "Page")
                        (linkedin-company-updates
                         channel-user
                         )
                        (linkedin-network-updates
                         channel-user))))

        (parse-linkedin-updates 
         channel-user
         result
         'updates)))))

(defun linkedin-refresh-updates ()
  (dolist (user (coerce (channel-users) 'list ))
      (when (and user (string-equal (get-val user 'doc-status) "Active"))
        ;;TODO: How to get error messages in for users without access tokens.
        (when (string-equal (get-val user 'channel-user-type) "LinkedIn")
          (when (get-val user 'last-access-token)
            (linkedin-refresh-update user))))))

(defun linkedin-refresh-connection (channel-user)
  (when channel-user
    (linkedin-connections
     channel-user)))

(defun linkedin-refresh-connections ()
  (dolist (user (coerce (channel-users) 'list ))
      (when (and user (string-equal (get-val user 'doc-status) "Active"))
        ;;TODO: How to get error messages in for users without access tokens.
        (when (string-equal (get-val user 'channel-user-type) "LinkedIn")
          (when (get-val user 'last-access-token)
            (multiple-value-bind (connections error)
                  (linkedin-refresh-connection user)
             ;; (break "~A" connections)
              (unless error
                (unless (get-val user 'user-data)
                  (setf (get-val user 'user-data) (make-hash-table :test 'equal)))
                (setf (gethash "connections"
                               (get-val user 'user-data))
                      connections)
                (persist user))))))))

(defun linkedin-refresh-profile (channel-user)
  (when channel-user
    (linkedin-profile
     channel-user)))

(defun linkedin-refresh-profiles ()
  (dolist (user (coerce (channel-users) 'list ))
      (when (and user (string-equal (get-val user 'doc-status) "Active"))
        ;;TODO: How to get error messages in for users without access tokens.
        (when (string-equal (get-val user 'channel-user-type) "LinkedIn")
          (when (get-val user 'last-access-token)
            (multiple-value-bind (connections error)
                  (linkedin-refresh-profile user)
              (unless error
                (unless (get-val user 'user-data)
                  (setf (get-val user 'user-data) (make-hash-table :test 'equal)))
                
                (setf (gethash "profile"
                               (get-val user 'user-data))
                      connections)
                (persist user))))))))

