(in-package :dyb)

(defun linkedin-refresh-update (channel channel-user)
  (when channel-user
    (when (get-val channel-user 'last-access-token)
      (let (
            (result (linkedin-network-updates
                     (get-val channel 'app-id)
                     (get-val channel 'app-secret)
                     (get-val channel-user 'last-access-token) 
                     (get-val channel-user 'last-token-secret))))
        (parse-linkedin-updates 
         (get-val channel-user 'entity)
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