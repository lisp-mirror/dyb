(in-package :ems)

(defclass service-user (doc)
  ((entity :initarg :entity
                        :initform nil)
   (service-user-name :initarg :service-user-name 
                :initform nil
                :accessor service-user-name)
   (service-user-type :initarg :service-user-type
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
  (:default-initargs :doc-type "service-user"))


(defun service-users-collection ()
  (get-collection (system-db) "service-users"))


(defmethod doc-collection ((doc service-user))
  (service-users-collection))


(defun service-users ()
  (docs (service-users-collection)))


(defun make-service-user (entity service-user-name service-user-type user-id
                          &key last-access-token)
  (make-instance 'service-user :key (list (get-val entity 'xid) service-user-type service-user-name) 
                 :doc-type "service-user" 
                 :xid (next-xid (service-users-collection))
                 :entity entity
                 :service-user-name service-user-name
                 :service-user-type service-user-type
                 :user-id user-id
                 :last-access-token last-access-token))

(defun get-service-user (entity-id service-user-type user-id)
  (get-doc (service-users-collection) 
            (list entity-id service-user-type user-id)))

(defun get-service-user-by-id (id)
  (get-doc (service-users-collection) id
                       :element 'xid))

(defgeneric match-service-users (doc service-users))

(defmethod match-service-users (doc service-users)
  (find (get-val doc 'xid) service-users))

(add-collection (system-db) "service-users" 
                :collection-class 'ems-collection
                :load-from-file-p t)

(defun get-service-user-by-auth-token (auth-token)
  (get-doc (service-users-collection) auth-token :element 'request-token))
  
(defun get-service-user-by-user-id (user-id)
  (get-doc (service-users-collection) user-id :element 'user-id))

(defun get-service-user-by-user-name (user-id)
  (get-doc (service-users-collection) user-id :element 'service-user-name))
  
