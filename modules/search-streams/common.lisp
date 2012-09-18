(in-package #:ems)

(defun parse-social-mention (mentions)
  (let ((items (assoc ':items mentions)))
    (when items
      (dolist (mention (cdr items) )
        (let* ((men (get-generic-entry-by-post-id (assoc ':id mention)))
               (old-men (if men
                            (copy men))))
          (unless men
            (setf men
                  (make-instance 'generic-entry
                                 :key (assoc ':id mention)
                                 :pid (assoc ':id mention)
                                 :title (assoc ':title mention)
                                 :payload (make-instance 'social-mention
                                                         :id (assoc ':id mention)
                                                         :title (assoc ':title mention)
                                                         :link (assoc ':link mention)
                                                         :time-stamp (assoc ':timestamp mention)
                                                         :language (assoc ':language mention)
                                                         :image (assoc ':image mention)
                                                         :embeded (assoc ':embeded mention)
                                                         :user (assoc ':user mention)
                                                         :user-image (assoc ':user_image mention)
                                                         :user-link (assoc ':user_link mention)
                                                         :domain (assoc ':domain mention)
                                                         :source (assoc ':source mention)
                                                         :favicon (assoc ':favicon mention)
                                                         :type (assoc ':type mention))
                                 :type "Social Mention"
                                 :interaction nil
                                 :created (assoc ':timestamp mention))))
          (if (and old-men (xid old-men))
              (persist men :old-object old-men)
              (persist men))
          )
        ))))

#|
(let* ((men (get-generic-entry-by-post-id (assoc ':id mention)))
               (old-men (if men
                            (copy men))))
          (unless men
            (setf men
                  (make-instance 'generic-entry
                                 :key (assoc ':id mention)
                                 :pid (assoc ':id mention)
                                 :title (assoc ':title mention)
                                 :payload (make-instance 'social-mention
                                                         :id (assoc ':id mention)
                                                         :title (assoc ':title mention)
                                                         :link (assoc ':link mention)
                                                         :time-stamp (assoc ':timestamp mention)
                                                         :language (assoc ':language mention)
                                                         :image (assoc ':image mention)
                                                         :embeded (assoc ':embeded mention)
                                                         :user (assoc ':user mention)
                                                         :user-image (assoc ':user_image mention)
                                                         :user-link (assoc ':user_link mention)
                                                         :domain (assoc ':domain mention)
                                                         :source (assoc ':source mention)
                                                         :favicon (assoc ':favicon mention)
                                                         :type (assoc ':type mention))
                                 :type "Social Mention"
                                 :interaction nil
                                 :created (assoc ':timestamp mention))))
          (if (and old-men (xid old-men))
              (persist men :old-object old-men)
              (persist men))
          )
|#