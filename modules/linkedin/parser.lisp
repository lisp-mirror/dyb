(in-package :dyb)

(defmethod raw-post-id (post (post-type (eql 'linkedin)))
  (gpv post :update-key))

(defmethod raw-post-text (post (post-type (eql 'linkedin)))
  (cond ((string-equal (gpv post :update-type) "SHAR")
         (gpv post :update-content :person :current-share :content :description))
        ((string-equal (gpv post :update-type) "CMPY")
         (gpv post :update-content :company-status-update :share :comment))))

(defmethod raw-post-user (post (post-type (eql 'linkedin)))
  (cond ((string-equal (gpv post :update-type) "SHAR")
         (gpv post :update-content :person :id))
        ((string-equal (gpv post :update-type) "CMPY")
         (gpv post :update-content :company :id))))

(defmethod raw-post-user-name (post (post-type (eql 'linkedin)))
  (cond ((string-equal (gpv post :update-type) "SHAR")
         (gpv post :update-content :person :first-name))
        ((string-equal (gpv post :update-type) "CMPY")
         (gpv post :update-content :company :name))))

(defmethod raw-post-user-id (post (post-type (eql 'linkedin)))
   (cond ((string-equal (gpv post :update-type) "SHAR")
         (gpv post :update-content :person :id))
        ((string-equal (gpv post :update-type) "CMPY")
         (gpv post :update-content :company :id))))

(defun parse-linkedin-updates (entity updates stream-type)

  (dolist (update (gpv updates :values))
    
    
    (let ((dup
           (find-doc (generic-post-collection)
                     :test
                     (lambda (doc)
                       (equal (raw-post-id update 'linkedin) 
                              (raw-post-id doc 'linkedin))))))
      (when dup
        ;;TODO: Update changed-date? 
        (setf (payload dup) update)
        (persist dup))
      (unless dup
        (persist (make-generic-post 
                  entity
                  'linkedin
                  update
                  stream-type
                  (unix-time-to-universal (truncate (/ (gpv update :timestamp) 1000)))
                  ;;TODO: last-change-date
                  ))
        ))))

