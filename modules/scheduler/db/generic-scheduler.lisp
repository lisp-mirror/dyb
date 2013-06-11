(in-package :dyb)

(defclass generic-action-log ()
  ((stamp :initarg :stamp)
   (label :initarg :label)
   (message :initarg :message))
  (:metaclass storable-versioned-class))

(defclass generic-action (doc)
  ((post-id :initarg :post-id
            :initform nil
            :accessor generic-entry-post-id)
   (post-type :initarg :post-type
              :initform nil
              :accessor post-type)
   (channel-user :initarg :channel-user
                 :initform nil
                 :accessor channel-user)
   (from-user-id :initarg :from-user-id
                 :initform nil
                 :accessor from-user-id)
   (to-user-id :initarg :to-user-id
               :initform nil
               :accessor to-user-id)
   (action-type :initarg :action-type
                :initform nil
                :accessor action-type)
   (action-content :initarg :action-content
                   :initform nil
                   :accessor action-content)
   (processed-content :initarg :processed-content
                   :initform nil
                   :accessor processed-content)
   (action-status :initarg :action-status
                  :initform nil
                  :accessor action-status
                  :documentation "Completed")
   (action-log :initarg :action-log
               :initform nil
               :accessor action-log)
   (image-url :initarg :image-url
              :initform nil
              :accessor image-url)
   (post-url :initarg :post-url
             :initform nil
             :accessor post-url)
   (short-url :initarg :short-url
              :initform nil
              :accessor short-url)
   (schedule-type :initarg :schedule-type
                   :initform nil
                   :accessor schedule-type)
   (scheduled-date :initarg :scheduled-date
                   :initform nil
                   :accessor scheduled-date))
  (:metaclass storable-versioned-class))


(defun generic-actions-collection ()
  (get-collection (system-db) "generic-actions"))

(defmethod doc-collection ((doc generic-action))
  (generic-actions-collection))

(defun generic-actions ()
  (docs (generic-actions-collection)))

(defun get-generic-action-by-id (id)
  (get-doc (generic-actions-collection) id
           :element 'id))

(defun get-generic-action-by-post-id (id)
  (get-doc (generic-actions-collection) id
           :element 'post-id))

(defun make-generic-action (channel-user post-id post-type from-user-id 
                            to-user-id action-type 
                            action-content processed-content schedule-type
                            scheduled-date &key image-url post-url short-url
                            action-status)
  (make-instance 'generic-action 
                 :post-id post-id
                 :post-type post-type
                 :channel-user channel-user
                 :from-user-id from-user-id
                 :to-user-id to-user-id
                 :action-type action-type
                 :action-content action-content
                 :processed-content processed-content
                 :image-url image-url
                 :post-url post-url
                 :short-url short-url
                 :schedule-type schedule-type
                 :scheduled-date scheduled-date
                 :action-status (or action-status "Pending")
                 ))

(defun add-generic-action (channel-user post-id post-type from-user-id to-user-id 
                           action-type action-content schedule-type scheduled-date 
                       &key image-url post-url short-url (shorten-urls-p t))
  (let ((dup (find-doc (generic-actions-collection)
                       :test (lambda (doc)
                               (and
                                (string-equal post-id (get-val doc 'post-id))
                                (string-equal post-type (get-val doc 'post-type))
                                (string-equal from-user-id (get-val doc 'from-user-id))
                                (string-equal to-user-id (get-val doc 'to-user-id))
                                (string-equal action-type (get-val doc 'action-type))
                                (string-equal action-content 
                                              (get-val doc 'action-content))
                                (string-equal (get-val doc 'action-status) "Pending"))))))
;(break "~a" dup)

    (if dup
        dup
        (persist (make-generic-action channel-user post-id post-type 
                                      from-user-id to-user-id action-type 
                                      action-content 
                                      (if (stringp action-content)
                                          (if shorten-urls-p
                                              (shortify-string action-content)
                                              action-content)
                                          action-content)
                                      schedule-type scheduled-date 
                                      :image-url image-url
                                      :post-url post-url
                                      :short-url short-url)))))

(defun post-action-abandoned-email (action)
  (send-system-mail  
   (frmt "[DYB]: ~a ~a abondened retires"
         (post-type action) (action-type action))
   (frmt "Post with id ~a failed 5 times to be delivered. ~a~%~%~A~%~%Last error log:~%~%~a"
         (id action)
         (current-date-time)
         (action-content action)
         (get-val (last (action-log action)) 'message))))

(defun add-generic-action-log (action label message status) 
  (cond ((>= (length (action-log action)) 5)
         (setf (action-status action) "Abandoned Retries")
         (post-action-abandoned-email action))
        (t
         (let ((log-entry (make-instance 'generic-action-log
                                         :stamp (get-universal-time)
                                         :label label
                                         :message message)))
           (setf (action-status action) status)
           (push log-entry (action-log action)))))
  (persist action))

(add-collection (system-db) "generic-actions" 
                :collection-class 'dyb-collection)
