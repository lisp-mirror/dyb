(in-package :dyb)

(defun social-mention-refresh-searches ()
  (dolist (search (coerce (search-streams) 'list ))

    (if (string-equal (search-stream-type search) "social mention")
        (parse-social-mention
         (get-val search 'entity)
         (social-mention-search (get-val search 'search-stream))
         'social-mention-search  ))))