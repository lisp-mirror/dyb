(in-package :dyb)

(defun social-mention-refresh-searches ()
  (dolist (search (coerce (search-streams) 'list ))
    (fetch-social-mention (get-val search 'search-stream) :parse-p t)
    ))