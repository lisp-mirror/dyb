(in-package :dyb)

(defclass coord ()
  ((longitude :initarg :longitude
              :accessor longitude)
   (latitude :initarg :latitude
                :accessor latitude))
  (:metaclass storable-class))

(defclass coordinate ()
  ((coordinates :initarg :coordinates
                :accessor coordinates)
   (type :initarg :type
         :accessor coordinate-type))
  (:metaclass storable-class))

(defclass contributor ()
  ((id :initarg :id
       :accessor :id)
   (screen-name :initarg :screen-name
                :accessor screen-name))
  (:metaclass storable-class))

(defclass tweet ()
  ((annotations :initarg :annotations
                :accessor annotations)
   (contributors :initarg :contributors
                 :accessor contibutors)
   (coordinates :initarg :coordinates
                :accessor coordinates)
   (created-at :initarg :created-at
               :accessor created-at)
   (current-user-retweet-id :initarg :current-user-retweet-id
                            :accessor current-user-retweet-id)
   (entities :initarg :entities
             :accessor tweet-entities)
   
   )
  (:metaclass storable-class))


(defun xxx (channel)
  (let ((id-hash (make-hash-table :test 'equal)))
    (find-docs 'list
               (lambda (doc)
                 (when (string-equal (post-type doc) channel)
                   (let ((post (gethash (post-id doc) id-hash))
                         (duplicate nil))
                     (when post
                       
                       (setf duplicate t))
                     (unless post
                       (setf (gethash (post-id doc) id-hash) doc))
                     duplicate)
                   ))
               (generic-post-collection))))

(defun yyy (channel)
  (let ((id-hash (make-hash-table :test 'equal)))
    (find-docs 'list
               (lambda (doc)
                 (when (string-equal (post-type doc) channel)
                   doc
                   ))
               (generic-post-collection))))

(defun zzz (payload-source)
  (find-docs 'list
               (lambda (doc)
                 (when (string-equal (payload-source doc) payload-source)
                   doc
                   ))
               (generic-post-collection)))

(defun delete-webfantix-tweets ()
  (find-docs 'list
               (lambda (doc)
                 (when (string-equal (post-type doc) "Twitter")
                   (when (string-equal (get-val (channel-user doc) 'channel-user-name) "webfanatix")
                     (remove-doc doc)
                     
                     (setf doc nil)
                     t)
                   ))
               (generic-post-collection)))


(defun delete-duplicate-posts (channel)
  (let ((id-hash (make-hash-table :test 'equal)))
    (find-docs 'list
               (lambda (doc)
                 (when (string-equal (post-type doc) channel)
                   (let ((post (gethash (post-id doc) id-hash))
                         (duplicate nil))
                     (when post
                       (remove-doc doc)
                       (setf doc nil)
                       (setf duplicate t))
                     (unless post
                       (setf (gethash (post-id doc) id-hash) doc))
                     duplicate)
                   ))
               (generic-post-collection))))