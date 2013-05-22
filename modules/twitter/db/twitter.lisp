(in-package :dyb)
;;https://dev.twitter.com/docs/platform-objects/tweets

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
       :accessor id)
   (screen-name :initarg :screen-name
                :accessor screen-name))
  (:metaclass storable-class))

;;https://dev.twitter.com/docs/tweet-entities
(defclass media-size ()
  ((size :initarg :size
       :accessor size)
   (width :initarg :width
          :accessor width)
   (height :initarg :height
           :accessor height)
   (resize :initarg :resize
       :accessor resize))
  (:metaclass storable-class))

(defclass media-entity ()
  ((id :initarg :id
        :accessor id
        :key t)
   (media-url :initarg :media-url
              :accessor media-url)
   (media-url-https :initarg :media-url-https
              :accessor media-url-https)
   (url :initarg :url
              :accessor url)
   (display-url :initarg :display-url
              :accessor display-url)
   (expanded-url :initarg :expanded-url
              :accessor expanded-url)
   (sizes :initarg :sizes
              :accessor sizes)
   (type :initarg :type
              :accessor media-type)
   (indices :initarg :indices
              :accessor indices))
  
  (:metaclass storable-class))

(defclass url-entity ()
  ((url :initarg :url
              :accessor url)
   (display-url :initarg :display-url
                :accessor display-url)
   (expanded-url :initarg :expanded-url
                 :accessor expanded-url)
   (indices :initarg :indices
              :accessor indices))
  
  (:metaclass storable-class))

(defclass mentions-entity ()
  ((id :initarg :id
       :accessor id)
   (screen-name :initarg :screen-name
                :accessor screen-name)
   (name :initarg :name
       :accessor name)
   (indices :initarg :indices
              :accessor indices))
  (:metaclass storable-class))

(defclass hashtag-entity ()
  ((text :initarg :text
       :accessor text)
   (indices :initarg :indices
              :accessor indices))
  (:metaclass storable-class))

(defclass symbol-entity ()
  ((text :initarg :text
       :accessor text)
   (indices :initarg :indices
              :accessor indices))
  (:metaclass storable-class))

(defclass place ()
  ((id :initarg :id
       :accessor id
       :key t)
   (attributes :initarg :attributes
       :accessor attributes)
   (bounding-box :initarg :bounding-box
              :accessor bounding-box) ;;coordinates
   (country :initarg :country
       :accessor country)
   (country-code :initarg :country-code
       :accessor country-code)
   (full-name :initarg :full-name
       :accessor full-name)
   (name :initarg :name
       :accessor name)
   (place-type :initarg :place-type
       :accessor place-type)
   (url :initarg :url
        :accessor url))
  (:metaclass storable-class))

(defclass twitter-user ()
  ((id :initarg :id
       :accessor id
       :key t)
   (contributers-enabled :initarg :contributers-enabled
       :accessor contributers-enabled)
   (created-at :initarg :created-at
       :accessor created-at)
   (defualt-profile :initarg :defualt-profile
       :accessor defualt-profile)
   (defualt-profile-image :initarg :defualt-profile-image
       :accessor defualt-profile-image)
   (description :initarg :description
       :accessor description)
   (entities :initarg :entities
             :accessor user-entities)
   (favourites-count :initarg :favourites-count
       :accessor favourites-count)
   (follow-request-sent :initarg :description
       :accessor description)
   (following :initarg :following
              :accessor following)
   (followers-count :initarg :followers-count
       :accessor followers-count)
   (friends-count :initarg :friends-count
       :accessor friends-count)
   (geo-enabled :initarg :geo-enabled
              :accessor geo-enabled)
   (is-translator :initarg :is-translator
              :accessor is-translator)
   (lang :initarg :lang
       :accessor lang)
   (listed-count :initarg :listed-count
       :accessor listed-count)
   (location :initarg :location
       :accessor location)
   (name :initarg :name
       :accessor name)
   (profile-background-colour :initarg :profile-background-colour
       :accessor profile-background-colour)
   (profile-background-image-url :initarg :profile-background-image-url
       :accessor profile-background-image-url)
   (profile-background-image-url-https :initarg :profile-background-image-url-https
       :accessor profile-background-image-url-https)
   (profile-background-title :initarg :profile-background-title
       :accessor profile-background-title)
   (profile-banner-url :initarg :profile-banner-url
       :accessor profile-banner-url)
   (profile-image-url :initarg :profile-image-url
       :accessor profile-image-url)
   (profile-image-url-https :initarg :profile-image-url-https
       :accessor profile-image-url-https)
   (profile-link-colour :initarg :profile-link-colour
       :accessor profile-link-colour)
   (profile-sidebar-border-colour :initarg :profile-sidebar-border-colour
       :accessor profile-sidebar-border-colour)
   (profile-sidebar-fill-colour :initarg :profile-sidebar-fill-colour
       :accessor profile-sidebar-fill-colour)
   (profile-text-colour :initarg :profile-text-colour
       :accessor profile-text-colour)
   (profile-use-background-image :initarg :profile-use-background-image
       :accessor profile-use-background-image)
   (protected :initarg :protected
       :accessor protected)
   (screen-name :initarg :screen-name
                :accessor screen-name)
   (last-tweet :initarg :last-tweet
       :accessor last-tweet) ;;(list id date)
   (statuses-count :initarg :statuses-count
       :accessor statuses-count)
   (time-zone :initarg :time-zone
       :accessor twitter-user-time-zone)
   (url :initarg :url
       :accessor url)
   (utc-offset :initarg :utc-offset
       :accessor utc-offset)
   (verified :initarg :verified
       :accessor verified)
   (witheld-in-countries :initarg :witheld-in-countries
       :accessor witheld-in-countries)
   (witheld-scope :initarg :witheld-scope
       :accessor witheld-scope)
   )
  (:metaclass storable-class))

(defclass tweet ()
  ((id :initarg :id
       :accessor id
       :key t)
   (annotations :initarg :annotations
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
   (favourite-count :initarg :favourite-count
       :accessor favourite-count)
   (favourited :initarg :favourited
       :accessor favourited)
   (in-reply-to-screen-name :initarg :in-reply-to-screen-name
       :accessor in-reply-to-screen-name)
   (in-reply-to-status-id :initarg :in-reply-to-status-id
       :accessor in-reply-to-status-id)
   (in-reply-to-user-id :initarg :in-reply-to-user-id
       :accessor in-reply-to-user-id)
   
   (lang :initarg :lang
       :accessor lang)
   (places :initarg :places
       :accessor places)
   (possibly-sensitive :initarg :possibly-sensitive
       :accessor possibly-sensitive)
   (scopes :initarg :scopes
       :accessor scopes)
   (retweet-count :initarg :retweet-count
       :accessor retweet-count)
   (retweeted :initarg :retweeted
       :accessor retweeted)
   (source :initarg :source
       :accessor source)
   (text :initarg :source
       :accessor text)
   (truncated :initarg :truncated
       :accessor truncated)
   (user :initarg :user
       :accessor user)
   (witheld-copyright :initarg :witheld-copyright
       :accessor witheld-copyright)
   (witheld-in-countries :initarg :witheld-in-countries
       :accessor witheld-in-countries)
   (witheld-scope :initarg :witheld-scope
       :accessor witheld-scope)
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