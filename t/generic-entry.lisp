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
	(cond ((zerop (+ comment-count likes-count)) "none")
			((= 1 (+ comment-count likes-count)) "low")
			((= 2 (+ comment-count likes-count)) "medium")
			(t "high"))))

(defun wrap-fb-post (post)
	(let (
		(pid (get-val (post) 'id))
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
		(pid (get-val tweet 'id))
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

(defun get-post-by-id (id)
  (get-doc (generic-entry-collection) id
           :element 'id))

(defmethod persist-doc ((doc post) &key (force-stamp-p t))
  (store-doc (generic-entry-collection) doc :force-stamp-p force-stamp-p))

(defun populate-post-db-from-json (post-list)
    (dolist (post post-list) 
      (persist-doc (make-post post))))


(add-collection (system-db) "generic-entry" 
                :collection-class 'ems-collection
                :load-from-file-p t)
