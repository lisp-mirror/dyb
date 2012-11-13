(in-package :dyb)

(defmethod raw-post-id (post (post-type (eql 'twitter)))
  (gpv post :id))

(defmethod raw-post-text (post (post-type (eql 'twitter)))
  (gpv post :text))

(defmethod raw-post-user (post (post-type (eql 'twitter)))
  (gpv post :user))

(defmethod raw-post-user-name (post (post-type (eql 'twitter)))
   (gpv post :user :screen--name))

(defmethod raw-post-user-id (post (post-type (eql 'twitter)))
   (gpv post :user :id--str))

(defun parse-twitter-created-at (date)
  (when date
    (let* ((split (split-string date #\Space))
           (split-time (split-string (fourth split) #\:)))
      
      (encode-universal-time 
       (parse-trim-integer (third split-time))
       (parse-trim-integer (second split-time))
       (parse-trim-integer (first split-time))
       (parse-trim-integer (third split))
       (month-number (second split)) 
       (parse-trim-integer (sixth split))))))

(defun parse-tweets (entity tweets stream-type)
  (if (not (gpv tweets :errors))
  
    (dolist (tweet tweets)

      (let ((dup
             (find-doc (generic-post-collection)
                       :test
                       (lambda (doc)
                         (equal (raw-post-id tweet 'twitter) (raw-post-id doc 'twitter))))))
        (when dup
          ;;TODO: Update changed-date? 
          (setf (payload dup) tweet)
          (persist dup))
        (unless dup
          (persist (make-generic-post 
                    entity
                    'twitter
                    tweet
                    stream-type
                    (parse-twitter-created-at (gpv tweet :created--at))
                    ;;TODO: last-change-date
                    ))
          ))
      )
    (gpv tweets :errors :message)))