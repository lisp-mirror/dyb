(in-package :dyb)

(defclass channel-user (doc)
  ((entity :initarg :entity
                        :initform nil)
   (channel-user-name :initarg :channel-user-name
                :initform nil
                :accessor channel-user-name)
   (channel-user-type :initarg :channel-user-type
                :initform nil
                :documentation "Twitter, Facebook, LinkedIn.")
   (user-id :initarg :user-id
                :initform nil)
   (last-access-token :initarg :last-access-token
                      :initform nil)
   (last-token-secret :initarg :last-token-secret
                      :initform nil)
   (request-token :initarg :request-token
                      :initform nil)
   (request-secret :initarg :request-secret
                      :initform nil)
   (access-token-expiry-date :initarg :access-token-expiry-date))
  (:metaclass storable-class)
  (:default-initargs :doc-type "channel-user"))


(defun channel-users-collection ()
  (get-collection (system-db) "channel-users"))


(defmethod doc-collection ((doc channel-user))
  (channel-users-collection))


(defun channel-users ()
  (docs (channel-users-collection)))


(defun make-channel-user (entity channel-user-name channel-user-type user-id
                          &key last-access-token)
  (make-instance 'channel-user :key (list (get-val entity 'xid) channel-user-type channel-user-name)
                 :doc-type "channel-user"
                 :xid (next-xid (channel-users-collection))
                 :entity entity
                 :channel-user-name channel-user-name
                 :channel-user-type channel-user-type
                 :user-id user-id
                 :last-access-token last-access-token))

(defun get-channel-user (entity-id channel-user-type user-id)
  (get-doc (channel-users-collection)
            (list entity-id channel-user-type user-id)))

(defun get-channel-user-by-id (id)
  (get-doc (channel-users-collection) id
                       :element 'xid))

(defgeneric match-channel-users (doc channel-users))

(defmethod match-channel-users (doc channel-users)
  (find (get-val doc 'xid) channel-users))

(add-collection (system-db) "channel-users"
                :collection-class 'dyb-collection
                :load-from-file-p t)

(defun get-channel-user-by-auth-token (auth-token)
  (get-doc (channel-users-collection) auth-token :element 'request-token))
  
(defun get-channel-user-by-user-id (user-id)
  (get-doc (channel-users-collection) user-id :element 'user-id))

(defun get-channel-user-by-user-name (user-id)
  (get-doc (channel-users-collection) user-id :element 'channel-user-name))
  
