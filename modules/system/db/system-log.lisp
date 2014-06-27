(in-package :dyb)

(defclass system-log ()
  ((source :initarg :source)
   (user :initarg :user)
   (status :initarg :status)
   (headers :initarg :headers)
   (remote-address :initarg :remote-address)
   (entities :initarg :entities)
   (message :initarg :message)
   (stamp :initarg :stamp))
  (:metaclass storable-versioned-class))

(defun system-logs-collection ()
  (get-collection (system-db) "system-log"))

(defun system-logs ()
  (docs (system-logs-collection)))

(defmethod doc-collection ((doc system-log))
  (system-logs-collection))

(defun log-entry (source status message)
  (persist (make-instance 'system-log 
                            :source source
                            :user (if (current-user)
                                      (email (current-user))
                                      (parameter "email"))
                            :status status
                            :headers (format nil "~A" (headers-in*))
                            :remote-address (remote-addr*)
                            :entities (context)
                            :message message
                            :stamp nil)))

(defun log-login (source email status message)
  (persist (make-instance 'system-log 
                            :source source
                            :user email
                            :status status
                            :headers (format nil "~A" (headers-in*))
                            :remote-address (remote-addr*)
                            :entities nil
                            :message message
                            :stamp nil)))

(add-collection (system-db) "system-log" 
                :collection-class 'collection)
