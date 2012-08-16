(in-package #:ems)

;; id helper

(defun get-post-id (post)
 (when (listp post)
   ;;TODO: Find a better way to check if post is not an error.  
   (unless (or (string-equal (format nil "~A" (first post)) "MESSAGE")
               (string-equal (format nil "~A" (first post)) "TYPE")
               (string-equal (format nil "~A" (first post)) "CODE")
               (string-equal (format nil "~A" (first post)) "ERROR--SUBCODE")) 
       (rest (assoc ':id  post)))))

;; from helpers

(defun get-from (post)
  (rest (assoc ':from post)))

(defun get-from-id (from)
  (rest (assoc ':id from)))

(defun get-from-name (from)
  (rest (assoc ':name from)))

;;(get-from-name (get-comment-from (first (get-comments-data (get-comments (nth 4 *post-list*))))))

(defun make-from (from-raw)
  (make-instance 'from :id (get-from-id from-raw) :name (get-from-name from-raw)))

;; to helpers

(defun get-to (post)
  (rest (assoc ':to post)))
(defun get-to-data (to-raw)
  (rest (assoc ':data to-raw)))

(defun get-to-name (to-raw)
  (rest (assoc ':name to-raw)))

(defun get-to-id (to-raw)
  (rest (assoc ':id to-raw)))

(defun make-to (to-raw)
  (make-instance 'to :id (get-to-id to-raw) :name (get-to-name to-raw)))

(defun make-to-list (to-data)
  (let(( out '()))
    (dolist (tos to-data) (setf out (append out (list (make-to tos))))) out))

;; message helper

(defun get-message (post)
  (rest (assoc ':message post)))

;; message-tags helpers

(defun get-message-tags (post)
  (rest (assoc ':message--tags post)))

(defun get-message-tag-id (message-tag)
  (rest (assoc ':id message-tag)))

(defun get-message-tag-name (message-tag)
  (rest (assoc ':name message-tag)))

(defun get-message-tag-offset (message-tag)
  (rest (assoc ':offset message-tag)))

(defun get-message-tag-length (message-tag)
  (rest (assoc ':length message-tag)))

(defun get-message-tag-type (message-tag)
  (rest (assoc ':type message-tag)))

(defun make-message-tag (message-tag)
  (make-instance 'message-tag :id (get-message-tag-id message-tag) 
                 :name (get-message-tag-name message-tag) 
                 :offset (get-message-tag-offset message-tag) 
                 :length (get-message-tag-length message-tag) 
                 :type (get-message-tag-type message-tag)))

(defun make-message-tag-list (message-tags)
  (let(( out '()))
    (dolist (st message-tags) 
      (setf out (append out (list (make-message-tag (first (rest st))))))) out))

;; picture helper

(defun get-picture (post)
  (rest (assoc ':picture post)))

;; link helper

(defun get-link (post)
  (rest (assoc ':link post)))

;; name helper

(defun get-name (post)
  (rest (assoc ':name post)))

;; caption helper

(defun get-caption (post)
  (rest (assoc ':caption post)))

;; description helper

(defun get-description (post)
  (rest (assoc ':description post)))

;; source helper

(defun get-source (post)
  (rest (assoc ':source post)))

;; properties helpers

(defun get-properties-list (post)
  (rest (assoc ':properties post)))

(defun get-property-name (property-raw)
  (rest (assoc ':name property-raw)))
(defun get-property-text (property-raw)
  (rest (assoc ':text property-raw)))

(defun make-property (property-raw)
  (make-instance 'property :name (get-property-name property-raw) 
                 :text (get-property-text property-raw)))

(defun make-property-list (properties-raw)
  (let(( out '()))
    (dolist (prop properties-raw) 
      (setf out (append out (list (make-property prop))))) out))

;; icon helper

(defun get-icon (post)
  (rest (assoc ':icon post)))

;; action helpers

(defun get-actions ( post )
  (rest (assoc ':actions post)))

(defun get-action-name ( action-element )
  (rest (assoc ':name action-element)))

(defun get-action-link ( action-element )
  (rest (assoc ':link action-element)))
;; inconsistent!!!
(defun make-action-list (post)
  (let(( out '()))
    (dolist (act (get-actions post)) 
      (setf out (append out 
                        (list (make-instance 'action 
                                             :name (get-action-name act) 
                                             :link (get-action-link act)))))) out ))

;; privacy helper it creates privacy NIL NIL if there is none

(defun get-privacy ( post )
  (rest (assoc ':privacy post)))

(defun get-privacy-description (post)
  (rest (assoc ':description (get-privacy post))))

(defun get-privacy-value (post)
  (rest (assoc ':value (get-privacy post))))

(defun make-privacy (post)
  (make-instance 'privacy :description (get-privacy-description post) 
                 :value (get-privacy-value post)))

;; type helper

(defun get-type (post)
  (rest (assoc ':type post)))

;; likes helpers ~~

(defun get-likes (post)
  (rest (assoc ':likes post)))

(defun get-likes-count (post)
  (rest (assoc ':count (get-likes post))))

(defun get-likes-data (post)
  (rest (assoc ':data (get-likes post))))

(defun get-like-id ( like )
  (rest (assoc ':id like)))

(defun get-like-name ( like )
  (rest (assoc ':name like)))

(defun make-like-list (post)
  (let(( out '()))
    (dolist (lk (get-likes-data post))
      (setf out (append out 
                        (list (make-instance 'like 
                                             :name (get-like-name lk) 
                                             :id (get-like-id lk)))))) out ))

(defun make-likes (post)
  (make-instance 'likes :data (make-like-list post) :count (get-likes-count post)))

;; place/location helpers

(defun get-place ( post )
  (rest (assoc ':place post)))

(defun get-place-id (place)
  (rest (assoc ':id place)))

(defun get-place-name (place)
  (rest (assoc ':name place)))

(defun get-place-location (place)
  (rest (assoc ':location place)))

(defun get-location-latitude (location)
  (rest (assoc ':latitude location)))

(defun get-location-longitude (location)
  (rest (assoc ':longitude location)))

(defun make-location (location)
  (make-instance 'location :latitude (get-location-latitude location) 
                 :longitude (get-location-longitude location)))

(defun make-place (place)
	   (make-instance 'place :id (get-place-id place) :name (get-place-name place) 
                          :location (make-location (get-place-location place))))

;; story helper

(defun get-story (post)
  (rest (assoc ':story  post)))

;; story-tags helpers

(defun get-story-tags (post)
  (rest (assoc  ':story--tags post)))

(defun get-story-tag-id (story-tag)
  (rest (assoc ':id story-tag)))

(defun get-story-tag-name (story-tag)
  (rest (assoc ':name story-tag)))

(defun get-story-tag-offset (story-tag)
  (rest (assoc ':offset story-tag)))

(defun get-story-tag-length (story-tag)
  (rest (assoc ':length story-tag)))

(defun get-story-tag-type (story-tag)
  (rest (assoc ':type story-tag)))

(defun make-story-tag (story-tag)
  (make-instance 'story-tag :id (get-story-tag-id story-tag) 
                 :name (get-story-tag-name story-tag) 
                 :offset (get-story-tag-offset story-tag) 
                 :length (get-story-tag-length story-tag) 
                 :type (get-story-tag-type story-tag)))

;;do not use as template it is hardcoded to avoid |offset|!!!! (first (make-story-tag-list (get-story-tags (nth 12 *post-list*))))
;;and I used it 1000 times as template
(defun make-story-tag-list (story-tags)
  (let(( out '()))
    (dolist (st story-tags) 
      (setf out (append out (list (make-story-tag (first (rest st))))))) out))

;; with-tags helpers

(defun get-with-tags ( post ) 
  (rest (assoc ':with--tags post)))

(defun get-with-data (post)
  (rest (assoc ':data (get-with-tags post))))

(defun get-with-tag-id ( tag-element )
	   (rest (assoc ':id tag-element)))

(defun get-with-tag-name ( tag-element )
  (rest (assoc ':name tag-element)))

(defun make-with-tag (tag-element)
  (make-instance 'with-tag :id (get-with-tag-id tag-element) 
                 :name (get-with-tag-name tag-element)))

(defun make-with-tag-list (with-tags)
  (let(( out '()))
	   (dolist (wt with-tags) 
             (setf out (append out (list (make-with-tag wt))))) out))

;; comments helpers

(defun get-comments (post)
  (rest (assoc ':comments post)))

(defun get-comments-data (comments)
  (rest (assoc ':data comments)))

(defun get-comments-count (comments)
  (rest (assoc ':count comments)))

(defun get-comment-id (comment-data)
  (rest (assoc ':id comment-data)))

(defun get-comment-from (comment-data)
  (rest (assoc ':from comment-data)))

(defun get-comment-message (comment-data)
  (rest (assoc ':message comment-data)))

(defun get-comment-likes (comment-data)
  (rest (assoc ':likes comment-data)))

(defun get-comment-created-time (comment-data)
  (rest (assoc ':created--time comment-data)))

(defun make-comment (comment)
  (make-instance 'comment :id (get-comment-id comment) 
                 :from (make-from (get-comment-from comment))
                 :message (get-comment-message comment) 
                 :created-time (get-comment-created-time comment )
                 :likes (get-comment-likes comment)))

(defun make-comment-list (comments)
  (let(( out '()))
    (dolist (ct comments) 
      (setf out (append out (list (make-comment ct))))) out))

(defun make-comments (comments)
  (make-instance 'comments :data (make-comment-list (get-comments-data comments)) 
                 :count (get-comments-count comments)))

;; object-id not tested!!!

(defun get-object-id (post)
        (rest (assoc ':object--id post)))

;; application helpers

(defun get-application ( post ) 
  (rest (assoc ':application post)))

(defun get-application-name ( post )
  (rest (assoc ':name (get-application post))))

(defun get-application-namespace ( post )
  (rest (assoc ':namespace (get-application post))))

(defun get-application-id ( post )
  (rest (assoc ':id (get-application post))))

(defun make-application ( post ) ; param is post in list form
  (make-instance 'application :id (get-application-id post) 
                 :name (get-application-name  post) 
                 :namespace (get-application-namespace post)))

;; created-time

(defun get-created-time ( post ) 
  (rest (assoc ':created--time post)))

;; updated-time

(defun get-updated-time ( post ) 
  (rest (assoc ':updated--time post)))

;; post loader


