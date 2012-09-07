(in-package #:ems)


(defclass generic-action (doc)
  ((pid :initarg :pid :accessor generic-entry-pid)
   (post-type :initarg :post-type)
   (from-user-id :initarg :from-user-id)
   (to-user-id :initarg :to-user-id)
   (action-type :initarg :action-type)
   (action :initarg :action)
   (action-status :initarg :action-status)
   (action-log :initarg :action-log)
   (scheduled-date :initarg :scheduled-date))
  (:metaclass storable-class))


(defun generic-actions-collection ()
  (get-collection (system-db) "generic-actions"))

(defun generic-actions ()
  (docs (generic-actions-collection)))

(defun get-generic-action-by-id (id)
  (get-doc (generic-actions-collection) id
           :element 'id))

(defun get-generic-action-by-post-id (id)
  (get-doc (generic-actions-collection) id
           :element 'pid))

(defun make-generic-action (pid post-type from-user to-user action-type 
                            action scheduled-date)
  (make-instance 'generic-action 
                 :pid pid
                 :post-type post-type
                 :from-user-id from-user
                 :to-user-id to-user
                 :action-type action-type
                 :action action
                 :scheduled-date scheduled-date
                 :action-status "Pending"
                 ))
(add-collection (system-db) "generic-actions" 
                :collection-class 'ems-collection
                :load-from-file-p t)


(defclass generic-entry (doc)
   (
	(pid :accessor generic-entry-pid :initarg :pid)
	(title :accessor generic-entry-title :initarg :title)
	(payload :accessor generic-entry-payload :initarg :payload)
	(type :accessor generic-entry-type :initarg :type)
	(interaction :accessor generic-entry-interaction :initarg :interaction)
	(created :accessor generic-entry-created :initarg :created))
	(:metaclass storable-class))

(defun get-msg-or-story (post)
	(let (
		(msg (get-val post 'message))
		(story (get-val post 'story))
		)
	(if msg msg story)))

(defun get-fb-activity (post)
	(let (
		(comment-count (get-val (get-val post 'comments) 'count))
		(likes-count (get-val (get-val post 'likes) 'count))
		)
	  (unless likes-count (setf likes-count 0))
	(cond ((zerop (+ comment-count likes-count)) "none")
			((= 1 (+ comment-count likes-count)) "low")
			((= 2 (+ comment-count likes-count)) "medium")
			(t "high"))))

(defun wrap-fb-post (fb-post)
  (when (get-val fb-post 'post-id)
      (let* ((post (get-generic-entry-by-post-id (get-val fb-post 'post-id)))
            (old-post (if post
                          (copy post))))
        (unless post
          (setf post (make-instance 'generic-entry
                                    :key (get-val fb-post 'post-id)
                                    :pid (get-val fb-post 'post-id)
                                    :title (get-msg-or-story fb-post)
                                    :payload fb-post
                                    :type "Facebook"
                                    :interaction (get-fb-activity fb-post)
                                    :created (get-val fb-post 'created-time))))
        (if (and old-post (xid old-post))
            (persist post :old-object old-post)
            (persist post)))))

; NIL protection
(defun get-tw-activity (count)
	(if count 
	(cond ((zerop count) "none")
			((= 1 count) "low")
			((= 2 count) "medium")
			(t "high"))
		"none"))
			
(defun wrap-tweet (tweet)
	(let (
		(pid (get-val tweet 'id-str))
		(title (get-val tweet 'text))
		(payload tweet)
		(type "tweeter")
		(interaction (get-tw-activity (get-val tweet 'retweet-count)))
		(created (get-val tweet 'created-at)))
		(if pid
				(make-instance 'generic-entry
					:key pid
					:pid pid
					:title title
					:payload payload
					:type type
					:interaction interaction
					:created created) ())))
						
(defun generic-entry-collection ()
  (get-collection (system-db) "generic-entry"))

(defun generic-entries ()
  (docs (generic-entry-collection)))

(defun get-generic-entry-by-id (id)
  (get-doc (generic-entry-collection) id
           :element 'id))

(defun get-generic-entry-by-post-id (id)
  (get-doc (generic-entry-collection) id
           :element 'pid))

(defmethod doc-collection ((doc generic-entry))
  (generic-entry-collection))

;(defun populate-post-db-from-json (post-list)
;    (dolist (post post-list) 
;      (persist-doc (make-post post))))

(defun populate-generic-db-from-post (post-list)
  
  (dolist (post post-list)
    
    
    (if (listp post)
        (if (get-post-id post)
            (let* ((old-post (get-post-by-post-id (get-post-id post)))
                   (new-post (if old-post
                                 old-post
                                 (make-post post))))

              (if (and old-post (xid old-post))
          
                  (persist new-post :old-object (copy old-post))
                  (persist new-post))

              (wrap-fb-post new-post)))
        post)))
      
(defun populate-generic-db-from-tweet (tweet-list)
    (dolist (tweet tweet-list) 
      (persist (wrap-tweet (make-tweet tweet)))))


(add-collection (system-db) "generic-entry" 
                :collection-class 'ems-collection
                :load-from-file-p t)
; to see what is serialized

(defun make-tw-wrapper-list (tweet-list)
	(let ((out ()))
	(dolist (tweet tweet-list) (setf out (append out (list (wrap-tweet (make-tweet tweet)))))) out ))

;The slot XID is unbound in the object #<GENERIC-ENTRY
;                                        {1009539393}>.
;   [Condition of type UNBOUND-SLOT]
;on single tweet it works on list doesn't!
(defun make-fb-wrapper-list (post-list)
	(let ((out ()))
	(dolist (post post-list) (setf out (append out (list (wrap-fb-post (make-post post)))))) out ))
