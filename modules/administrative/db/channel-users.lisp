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
   (profile-type :initarg :profile-type
                :initform nil
                :documentation "User,Page")
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
   (access-token-expiry-date :initarg :access-token-expiry-date)
   (user-data :initarg :user-data
              :initform (make-hash-table :test 'equal))
   (verification-code :initarg :verification-code
                      :initform nil))
  (:metaclass storable-class)
  (:default-initargs :doc-type "channel-user"))


(defun channel-users-collection ()
  (get-collection (system-db) "channel-users"))


(defmethod doc-collection ((doc channel-user))
  (channel-users-collection))


(defun channel-users ()
  (docs (channel-users-collection)))


(defun make-channel-user (entity channel-user-name channel-user-type profile-type
                          user-id
                          &key last-access-token)
  (make-instance 'channel-user :key (list (get-val entity 'xid) channel-user-type channel-user-name)
                 :doc-type "channel-user"
                 :xid (next-xid (channel-users-collection))
                 :entity entity
                 :channel-user-name channel-user-name
                 :channel-user-type channel-user-type
                 :profile-type profile-type
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

(unless (channel-users-collection)
  (add-collection (system-db) "channel-users"
                  :collection-class 'dyb-collection
                  :load-from-file-p t))

(defun get-channel-user-by-auth-token (auth-token)
  (get-doc (channel-users-collection) auth-token :element 'request-token))
  
(defun get-channel-user-by-user-id (user-id)
  (find-doc 
   (channel-users-collection) 
   :test (lambda (doc)
           (string-equal (format nil "~A" user-id)
                         (format nil "~A" (get-val doc 'user-id))))))

(defun get-channel-user-by-user-name (user-id)
  (get-doc (channel-users-collection) user-id :element 'channel-user-name))

(defun get-channel-user-by-verification-code (code)
  (get-doc (channel-users-collection) code :element 'verification-code))
  
(defun get-channel-users-list (channel-name users)
  (loop for user across (channel-users)
       when (string-equal (get-val user 'channel-user-type) channel-name)
       ;when (find (get-val user 'user-id) users)
       when (match-context-entities user)
       collect (list (get-val user 'user-id)
                     (get-val user 'channel-user-name))))