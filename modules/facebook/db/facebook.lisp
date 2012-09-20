(in-package #:ems)

(defclass post (doc) 
  ((post-id :accessor post-id :initarg :post-id)
   (from :accessor post-from :initarg :from)
   (to :accessor post-to :initarg :to)
   (message :accessor post-message :initarg :message)
   (message-tags :accessor post-message-tags :initarg :message-tags)
   (picture :accessor post-picture :initarg :picture)
   (link :accessor post-link :initarg :link)
   (name :accessor post-name :initarg :name)
   (caption :accessor post-caption :initarg :caption)
   (description :accessor post-description :initarg :description)
   (source :accessor post-source :initarg :source)
   (properties :accessor post-properties :initarg :properties)
   (icon :accessor post-icon :initarg :icon)
   (actions :accessor post-actions :initarg :actions)
   (privacy :accessor post-privacy :initarg :privacy)
   (type :accessor post-type :initarg :type)
   (likes :accessor post-likes :initarg :likes)
   (place :accessor post-place :initarg :place)
   (story :accessor post-story :initarg :story)
   (story-tags :accessor post-story-tags :initarg :story-tags)
   (with-tags :accessor post-with-tags :initarg :with-tags)
   (comments :accessor post-comments :initarg :comments)
   (object-id :accessor post-object-id :initarg :object-id)
   (application :accessor post-application :initarg :application)
   (created-time :accessor post-created-time :initarg :created-time)
   (updated-time :accessor post-updated-time :initarg :updated-time))
  (:metaclass storable-class))



(defclass from () 
  ((id :accessor from-id :initarg :id) 
   (name :accessor from-name :initarg :name)
   (picture :accessor picture :initarg :picture)))

(defclass to () 
  ((id :accessor to-id :initarg :id) 
   (name :accessor to-name :initarg :name)
   (picture :accessor picture :initarg :picture)))

(defclass message-tag () 
  ((id :accessor message-tag-id :initarg :id)
   (name :accessor message-tag-name :initarg :name)
   (offset :accessor message-tag-offset :initarg :offset)
   (length :accessor message-tag-length :initarg :length)
   (type :accessor message-tag-type :initarg :type)))

;---------------------------
;(defclass properties() ;this is just list
;    ((name :accessor properties-name :initarg :name) 
;    (text :accessor properties-text :initarg :text)))
;---------------------------

(defclass property() 
  ((name :accessor property-name :initarg :name) 
   (text :accessor property-text :initarg :text)))

(defclass action() 
  ((name :accessor action-name :initarg :name) 
   (link :accessor action-link :initarg :link)))

(defclass privacy() 
  ((description :accessor privacy-description :initarg :description) 
   (value :accessor privacy-value :initarg :value)))

(defclass like () 
  ((id :accessor like-id :initarg :id) 
   (name :accessor like-name :initarg :name)))

(defclass likes () 
  ((data :accessor likes-data :initarg :data) 
   (count :accessor likes-count :initarg :count)))

(defclass place () 
  ((id :accessor place-id :initarg :id)
   (name :accessor place-name :initarg :name)
   (location :accessor place-location :initarg :location)))

(defclass location () 
  ((latitude :accessor location-latitude :initarg :latitude)
   (longitude :accessor location-longitude :initarg :longitude)))

(defclass story-tag () 
  ((id :accessor story-tag-id :initarg :id)
   (name :accessor story-tag-name :initarg :name)
   (offset :accessor story-tag-offset :initarg :offset)
   (length :accessor story-tag-length :initarg :length)
   (type :accessor story-tag-type :initarg :type)))

(defclass with-tag() 
  ((id :accessor with-tag-id :initarg :id) 
   (name :accessor with-tag-name :initarg :name)))

(defclass comments () 
  ((data :accessor comments-data :initarg :data) 
   (count :accessor comments-count :initarg :count)))

(defclass comment () 
  ((id :accessor comment-id :initarg :id)
   (from :type 'id-pair :accessor comment-from :initarg :from)
   (message :accessor comment-message :initarg :message)
   (created-time :accessor comment-created-time :initarg :created-time)
   (likes :accessor comment-likes :initarg :likes)))

(defclass application () 
  ((name :accessor application-name :initarg :name)
   (namespace :accessor application-namespace :initarg :namespace)
   (id :accessor application-id :initarg :id)))


(defun make-post (post-raw )
  (make-instance 'post 
                 :key (get-post-id post-raw)
                 
                 :post-id (get-post-id post-raw) 
                 :from (make-from (get-from post-raw)) 
                 :to (make-to-list (get-to-data (get-to post-raw))) 
                 :message (get-message post-raw) 
                 :message-tags (make-message-tag-list (get-message-tags post-raw)) 
                 :picture (get-picture post-raw) 
                 :link (get-link post-raw) 
                 :name (get-name post-raw) 
                 :caption (get-caption post-raw) 
                 :description (get-description post-raw) 
                 :source (get-source post-raw) 
                 :properties (make-property-list (get-properties-list post-raw)) 
                 :icon (get-icon post-raw) 
                 :actions (make-action-list post-raw) 
                 :privacy (make-privacy post-raw) 
                 :type (get-type post-raw) 
                 :likes (make-likes post-raw) 
                 :place (make-place (get-place post-raw)) 
                 :story (get-story post-raw) 
                 :story-tags (make-story-tag-list (get-story-tags post-raw)) 
                 :with-tags (make-with-tag-list (get-with-data post-raw)) 
                 :comments (make-comments (get-comments post-raw)) 
                 :object-id (get-object-id post-raw) 
                 :application (make-application post-raw) 
                 :created-time (get-created-time post-raw) 
                 :updated-time (get-updated-time post-raw)))

(defun posts-collection ()
  (get-collection (system-db) "posts"))

(defun posts ()
  (docs (posts-collection)))

(defmethod doc-collection ((doc post))
  (posts-collection))

(defun get-post-by-id (id)
  (get-doc (posts-collection) id
           :element 'id))

(defun get-post-by-post-id (id)
  (get-doc (posts-collection) id
           :element 'post-id))

(defun populate-post-db-from-json (post-list)
    (dolist (post post-list) 
      (persist (make-post post))))


(add-collection (system-db) "posts" 
                :collection-class 'ems-collection
                :load-from-file-p t)