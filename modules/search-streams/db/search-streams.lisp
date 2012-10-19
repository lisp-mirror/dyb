(in-package #:ems)

(defclass search-stream (doc)
  (
   (entity :initarg :entity)
   (description :initarg :discription)
   (search-stream-type :initarg :search-stream-type
                       :accessor search-stream-type)
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
                 :key (list (xid entity) description)
                 :entity entity
                 :description description
                 :search-stream-type search-stream-type
                 :search-stream search-stream
              
                 :search-stream-status "Active"
                 ))
(add-collection (system-db) "search-streams" 
                :collection-class 'ems-collection
                :load-from-file-p t)