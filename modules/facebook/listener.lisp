(in-package :dyb)

(defun facebook-refresh-feeds ()
  (dolist (user (coerce (channel-users) 'list ))
        
    (when (and user (string-equal (get-val user 'doc-status) "Active"))
      ;;TODO: How to get error messages in for users without access tokens.
      (when (string-equal (get-val user 'channel-user-type) "Facebook")
        (when (get-val user 'last-access-token)

          (multiple-value-bind (posts error)
              (facebook-feed user)
            (unless error
              (parse-facebook-posts (get-val user 'entity) posts 'facebook-feed)
              )))))))
