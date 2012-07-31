(in-package #:ems)

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

(defun wrap-fb-post (post)
	(let (
		(pid (get-val post 'post-id))
		(title (get-msg-or-story post))
		(payload post)
		(type "facebook")
		(interaction (get-fb-activity post))
		(created (get-val post 'created-time)))
	(if pid
		(make-instance 'generic-entry
						:key pid
						:pid pid
						:title title
						:payload payload
						:type type
						:interaction interaction
						:created created) ())))
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

(defun generic-entry ()
  (docs (generic-entry-collection)))

(defun get-generic-entry-by-id (id)
  (get-doc (generic-entry-collection) id
           :element 'id))

(defmethod persist-doc ((doc generic-entry) &key (force-stamp-p t))
  (store-doc (generic-entry-collection) doc :force-stamp-p force-stamp-p))

;(defun populate-post-db-from-json (post-list)
;    (dolist (post post-list) 
;      (persist-doc (make-post post))))

(defun populate-generic-db-from-post (post-list)
    (dolist (post post-list) 
      (persist-doc (wrap-fb-post (make-post post)))))
      
(defun populate-generic-db-from-tweet (tweet-list)
    (dolist (tweet tweet-list) 
      (persist-doc (wrap-tweet (make-tweet tweet)))))


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
