(in-package :dyb)

(defmethod raw-post-id (post (post-type (eql 'facebook)))
  (gpv post :id))

(defmethod raw-post-text (post (post-type (eql 'facebook)))
  (or (gpv post :message) (gpv post :story) ))

(defmethod raw-post-user (post (post-type (eql 'facebook)))
  (gpv post :from))

(defmethod raw-post-user-name (post (post-type (eql 'facebook)))
   (gpv post :from :name))

(defmethod raw-post-user-id (post (post-type (eql 'facebook)))
   (gpv post :from :id))


;;"2012-10-09T09:40:43+0000"

(defun parse-facebook-created-at (date)
  (when date
    (let* ((split  (split-string date #\T))
           (split-date (split-string (first split) #\-))
           (split-time (split-string
                        (first (split-string (second split) #\+))
                        #\:)))
      
      (encode-universal-time 
       (parse-trim-integer (third split-time))
       (parse-trim-integer (second split-time))
       (parse-trim-integer (first split-time))
       (parse-trim-integer (third split-date))
       (parse-trim-integer (second split-date)) 
       (parse-trim-integer (first split-date))))))

(defun parse-facebook-posts (posts stream-type)
  (dolist (post (gpv posts :data))

    (let ((dup
           (find-doc (generic-post-collection)
                     :test
                     (lambda (doc)
                       (when (equal (post-type doc) 'facebook)
                         (equal (raw-post-id post 'facebook) 
                                (raw-post-id doc 'facebook)))))))
      
      (when dup
        (setf (last-change-date dup) 
              (parse-facebook-created-at 
               (gpv post :updated--time)))
        (setf (payload dup) post)

        (persist dup))
      (unless dup
        (persist (make-generic-post 'facebook
                                    post
                                    stream-type
                                    (parse-facebook-created-at (gpv post :created--time))
                                    :last-change-date (parse-facebook-created-at 
                                                       (gpv post :updated--time))))))))