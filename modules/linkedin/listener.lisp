(in-package :ems)

(defun linkedin-refresh-update (channel service-user)
  (when service-user
    (when (get-val service-user 'last-access-token)
      (let (
            (result (linkedin-network-updates
                     (get-val channel 'app-id)
                     (get-val channel 'app-secret)
                     (get-val service-user 'last-access-token) 
                     (get-val service-user 'last-token-secret))))
        (parse-linkedin-updates 
         (json::decode-json-from-string (flexi-streams:octets-to-string result))
         'updates)))))

(defun linkedin-refresh-updates ()
  (let ((channel (get-social-channel "LinkedIn")))
    (dolist (user (coerce (service-users) 'list ))
      (when (and user (string-equal (get-val user 'doc-status) "Active"))
        ;;TODO: How to get error messages in for users without access tokens.
        (when (string-equal (get-val user 'service-user-type) "LinkedIn")
          (when (get-val user 'last-access-token)
            (linkedin-refresh-update channel user)))))))