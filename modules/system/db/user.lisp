(in-package :dyb)

(defclass user-preference (doc)
  ((name :initarg :name
          :accessor name)
   (preference :initarg :preference
             :accessor preference))
  (:metaclass storable-versioned-class))

(defclass permission-template (doc)
  ((name :initarg :name
         :initform nil
         :accessor name) 
   (permission-list :initarg :permission-list
                    :initform nil
                    :accessor permission-list))
  (:metaclass storable-versioned-class))

(defclass user (doc)
  ((email :initarg :email
          :accessor email
          :key t)
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
  (:metaclass storable-versioned-class))

(defun users-collection ()
  (get-collection (system-db) "users"))

(defun permission-templates-collection ()
  (get-collection (system-db) "permission-templates"))

(defun users ()
  (docs (users-collection)))

(defmethod doc-collection ((doc user))
  (users-collection))

(defun make-user (email password &key permissions
                  accessible-entities
                  preferences
                  super-user-p)
  (multiple-value-bind (password salt)
      (make-password password)
    (make-instance 'user :email email
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

(defmethod doc-collection ((doc permission-template))
  (permission-templates-collection))

(defmethod match-entities ((doc user) entities)
  (intersection (get-val doc 'accessible-entities) entities))

(defun find-users (criteria)
  (if criteria
      (find-docs 'vector
                 criteria
                 (users-collection))
      (users)))

(add-collection (system-db) "users" 
                :collection-class 'dyb-collection)

(add-collection (system-db) "permission-templates" 
                :collection-class 'dyb-collection)


(unless (get-user "admin@dyb.co.za")
    ;;Don't remove this, but change the default password regularly
    (persist (make-user "admin@dyb.co.za" "admin"
                        :super-user-p t)))

(defun user-list ()
  (let ((u-list))
    (dolist (doc (coerce (users) 'list))
      (dolist (entity (get-val doc 'accessible-entities))
        
        (if (find entity (context))
            (when (not (string-equal (get-val doc 'doc-status) "superseded"))
              (setf u-list (append u-list (list (list (get-val doc 'email) 
                                                      (get-val doc 'email)))))))))
    u-list))

(defun context-users-list ()
  (let ((u-list))
    (dolist (doc (coerce (users) 'list))
      (dolist (entity (get-val doc 'accessible-entities))
        
        (if (find entity (context))
            (setf u-list (append u-list (list doc))))))
    u-list))
