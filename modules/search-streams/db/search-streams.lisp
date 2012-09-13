(in-package #:ems)

(defclass social-mention (doc)
  ((id :initarg :id
           :initform nil)
   (title :initarg :title
           :initform nil)
   (link :initarg :link)
   (time-stamp :initarg :time-stamp)
   (language :initarg :language)
   (image :initarg :image)
   (embeded :initarg :embeded)
   (user :initarg :user)
   (user-image :initarg :user-image)
   (user-link :initarg :user-link)
   (domain :initarg :domain)
   (source :initarg :source)
   (favicon :initarg :favicon)
   (type :initarg :type)
   
   )
  (:metaclass storable-class))


(defclass search-stream (doc)
  ((entity :initarg :entity
           :initform nil)
   (description :initarg :description
           :initform nil)
   (search-stream-type :initarg :search-stream-type)
   (search-stream :initarg :search-stream)
   (search-stream-status :initarg :search-stream-status)
   (search-stream-log :initarg :search-stream-log)
   )
  (:metaclass storable-class))

(defun search-streams-collection ()
  (get-collection (system-db) "search-streams"))

(defmethod doc-collection ((doc search-stream))
  (search-streams-collection))

(defun search-streams ()
  (docs (search-streams-collection)))

(defun get-search-stream-by-id (id)
  (get-doc (search-streams-collection) id
           :element 'id))

(defun get-search-stream-by-post-id (id)
  (get-doc (search-streams-collection) id
           :element 'pid))

(defun make-search-stream (entity description search-stream-type 
                            search-stream)
  (make-instance 'search-stream 
                 :key (list (xid entity) search-stream-type description)
                 :entity entity
                 :description description
                 :search-stream-type search-stream-type
                 :search-stream search-stream
              
                 :search-stream-status "Active"))

(add-collection (system-db) "search-streams" 
                :collection-class 'ems-collection
                :load-from-file-p t)