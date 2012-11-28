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

(defun parse-facebook-posts (channel-user posts stream-type)
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
        (if (string-equal (get-val channel-user 'profile-type) "Page")
            (let ((insights (post-insights channel-user (raw-post-id post 'facebook))))
              (setf (gethash :insights (get-val dup 'post-data))
                    insights)))
        (persist dup))
      (unless dup
        
        (let ((doc (make-generic-post 
                             channel-user
                             'facebook
                             post
                             stream-type
                             (parse-facebook-created-at (gpv post :created--time))
                             :last-change-date (parse-facebook-created-at 
                                                (gpv post :updated--time))))
              )
          (if (string-equal (get-val channel-user 'profile-type) "Page")
              (let ((insights (post-insights channel-user (raw-post-id post 'facebook))))
                (setf (gethash :insights (get-val doc 'post-data))
                      insights)))
          (persist doc)
          )))))

(defun parse-facebook-insights (channel-user insights)
  (dolist (insight-raw (gpv insights :data))
    (let* ((end-time (parse-facebook-created-at (gpv insight-raw :end-time)))
           (insight (get-facebook-insight-by-name (gpv insight-raw :name)))
           (dup (get-facebook-insight-value channel-user insight end-time)))
      
      (unless insight
        (setf insight (persist (make-facebook-insight (gpv insight-raw :name)
                                                      (gpv insight-raw :title)
                                                      (gpv insight-raw :description)
                                                      (gpv insight-raw :period)))))
      (unless dup
        (persist (make-facebook-insight-value 
                  channel-user
                  insight
                  (gpv insight-raw :name)
                  end-time))))))