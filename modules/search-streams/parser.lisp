(in-package :dyb)

(defmethod raw-post-id (post (post-type (eql 'social-mention)))
  (gpv post :id))

(defmethod raw-post-text (post (post-type (eql 'social-mention)))
  (gpv post :description))

(defmethod raw-post-user (post (post-type (eql 'social-mention)))
  (gpv post :user))

(defmethod raw-post-user-name (post (post-type (eql 'social-mention)))
   (gpv post :user :user))

(defmethod raw-post-user-id (post (post-type (eql 'social-mention)))
   (gpv post :user--id))

(defun parse-social-mention (entity mentions stream-type)

  (dolist (mention (gpv mentions :items))
    (let ((dup
           (find-doc (generic-post-collection)
                     :test
                     (lambda (doc)
                       (equal (raw-post-id mention 'social-mention) 
                              (raw-post-id doc 'social-mention))))))
      (when dup
        ;;TODO: Update changed-date? 
        (setf (payload dup) mention)
        (persist dup))
      (unless dup
        (persist (make-generic-post entity 
                                    'social-mention
                                    mention
                                    stream-type
                                    (if (gpv mention :timestamp)
                                        (unix-time-to-universal 
                                         (gpv mention :timestamp))
                                        (get-universal-time)
                                        )
                                    ;;TODO: last-change-date
                                    ))
        ))))

