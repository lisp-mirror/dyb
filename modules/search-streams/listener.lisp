(in-package #:ems)

(defun social-mention-refresh-searches ()
  (dolist (search (coerce (search-streams) 'list ))

    (if (string-equal (search-stream-type search) "social mention")
        (parse-social-mention (social-mention-search (get-val search 'search-stream))
                              'social-mention-search  ))))