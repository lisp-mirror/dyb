(in-package #:ems)

(defun fetch-social-mention (search &key parse-p)
  (multiple-value-bind (body)
      (drakma:http-request (format nil "http://api2.socialmention.com/search?q=~A&f=json&t=all&lang=en" search))
    (if parse-p
        (parse-social-mention (json:decode-json-from-string (babel:octets-to-string body)))
        (json:decode-json-from-string (babel:octets-to-string body)))))


(defun assocv (item alist)
  (rest (assoc item alist :test 'equal)))

(defun parse-social-mention (mentions)
  (let ((items (assoc ':items mentions)))
    (when items

      (dolist (mention (cdr items) )

        (let* ((men (get-generic-entry-by-post-id (assocv ':id mention)))
               (old-men (if men
                            (copy men))))
          (unless men
            (setf men
                  (make-instance 'generic-entry
                                 :key (assocv ':id mention)
                                 :pid (assocv ':id mention)
                                 :title (assocv ':title mention)
                                 :payload (make-instance 'social-mention
                                                         :mention-id (assocv ':id mention)
                                                 
                                                         :title (assocv ':title mention)
                                                         :description (assocv ':description mention)
                                                         :link (assocv ':link mention)
                                                         :time-stamp (assocv ':timestamp mention)
                                                         :language (assocv ':language mention)
                                                         :image (assocv ':image mention)
                                                         :embeded (assocv ':embeded mention)
                                                         :user (assocv ':user mention)
                                                         :user-image (assocv ':user_image mention)
                                                         :user-link (assocv ':user_link mention)
                                                         :domain (assocv ':domain mention)
                                                         :source (assocv ':source mention)
                                                         :favicon (assocv ':favicon mention)
                                                         :type (assocv ':type mention))
                                 :type "Social Mention"
                                 :interaction nil
                                 :created (assocv ':timestamp mention))))
          (if (and old-men (xid old-men))
              (persist men :old-object old-men)
              (persist men))
          )
        ))))

(defun update-social-mention-for-searches ()
  (dolist (search (coerce (search-streams) 'list ))
    (fetch-social-mention (get-val search 'search-stream) :parse-p t)
    ))

#|
(let* ((men (get-generic-entry-by-post-id (assocv ':id mention)))
               (old-men (if men
                            (copy men))))
          (unless men
            (setf men
                  (make-instance 'generic-entry
                                 :key (assocv ':id mention)
                                 :pid (assocv ':id mention)
                                 :title (assocv ':title mention)
                                 :payload (make-instance 'social-mention
                                                         :id (assocv ':id mention)
                                                         :title (assocv ':title mention)
                                                         :link (assocv ':link mention)
                                                         :time-stamp (assocv ':timestamp mention)
                                                         :language (assocv ':language mention)
                                                         :image (assocv ':image mention)
                                                         :embeded (assocv ':embeded mention)
                                                         :user (assocv ':user mention)
                                                         :user-image (assocv ':user_image mention)
                                                         :user-link (assocv ':user_link mention)
                                                         :domain (assocv ':domain mention)
                                                         :source (assocv ':source mention)
                                                         :favicon (assocv ':favicon mention)
                                                         :type (assocv ':type mention))
                                 :type "Social Mention"
                                 :interaction nil
                                 :created (assocv ':timestamp mention))))
          (if (and old-men (xid old-men))
              (persist men :old-object old-men)
              (persist men))
          )
|#