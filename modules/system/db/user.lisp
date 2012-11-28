(in-package :dyb)

(defclass user-preference (doc)
  ((name :initarg :name
          :accessor name)
   (preference :initarg :preference
             :accessor preference))
  (:metaclass storable-class))

(defclass permissions (doc)
  ((name :initarg :name
         :initform nil
         :accessor name) 
   (permission-list :initarg :permission-list
                    :initform nil
                    :accessor permission-list))
  (:metaclass storable-class))

(defclass user (doc)
  ((email :initarg :email
          :accessor email)
   (password :initarg :password
             :accessor password)
   (salt :initarg :salt
         :accessor salt)
   (permissions :initarg :permissions
                :initform nil
                :accessor permissions) 
   (accessible-entities :initarg :accessible-entities
			:initform nil
			:accessor accessible-entities)
   (last-root :initarg :last-root
              :initform nil
              :accessor last-root)
   (last-context :initarg :last-context
                 :initform nil
                 :accessor last-context)
   (preferences :initarg :preferences
                :initform nil
                :accessor preferences)
   (super-user-p :initarg :super-user-p
                 :initform nil
                 :accessor super-user-p))
  (:metaclass storable-class))

(defun users-collection ()
  (get-collection (system-db) "users"))

(defun permissions-collection ()
  (get-collection (system-db) "permissions"))

(defun users ()
  (docs (users-collection)))

(defmethod doc-collection ((doc user))
  (users-collection))

(defvar *min-passwrod-length* 5)

(defun make-password (password)
  (let* ((salt (generate-salt))
         (password (hash-password password salt)))
    (values password salt)))

(defun make-user (email password &key permissions
                  accessible-entities
                  preferences
                  super-user-p)
  (multiple-value-bind (password salt)
      (make-password password)
    (make-instance 'user :key email :doc-type "user" 
                         
                         :email email
                         :password password
                         :salt salt
                         :permissions permissions
                         :accessible-entities accessible-entities
                         :preferences preferences
                         :super-user-p super-user-p)))

(defun change-user (user new-password
                    &key (superuser nil superuser-supplied))
  (when new-password
    (setf (values (password user) (salt user))
          (make-password new-password)))
  (when superuser-supplied
    ;; (setf (super-user-p user) superuser)
    ))

(defun get-user (email)
  (get-doc (users-collection) email :element 'email))

(defmethod doc-collection ((doc user))
  (users-collection))

(defmethod doc-collection ((doc permissions))
  (permissions-collection))

(defmethod match-entities ((doc user) entities)
  (intersection (get-val doc 'accessible-entities) entities))

(defun find-users (criteria)
  (if criteria
      (find-docs 'vector
                 criteria
                 (users-collection))
      (users)))

(unless (users-collection)
  (add-collection (system-db) "users" 
                  :collection-class 'dyb-collection 
                  :load-from-file-p t))

(unless (permissions-collection)
  (add-collection (system-db) "permissions" 
                  :collection-class 'dyb-collection 
                  :load-from-file-p t))


(unless (get-user "admin@dyb.co.za")
    ;;Don't remove this, but change the default password regularly
    (persist (make-user "admin@dyb.co.za" "admin"
                        :super-user-p t)))

(defun user-list ()
  (let ((u-list))
    (dolist (doc (coerce (users) 'list))
      ;;TODO: Match entities
      (dolist (entity (get-val doc 'accessible-entities))
        
        (if (find entity (context))
            (when (not (string-equal (get-val doc 'doc-status) "superseded"))
              (setf u-list (append u-list (list (list (get-val doc 'email) 
                                                      (get-val doc 'email)))))))))
    u-list))

