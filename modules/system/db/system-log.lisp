(in-package :ems)

(defclass system-log ()
  ((source :initarg :source)
   (user :initarg :user)
   (status :initarg :status)
   (headers :initarg :headers)
   (remote-address :initarg :remote-address)
   (entities :initarg :entities)
   (message :initarg :message)
   (stamp :initarg :stamp))
  (:metaclass storable-class)
  )

(defun system-logs-collection ()
  (get-collection (system-db) "system-log"))

(defun system-logs ()
  (docs (entity-types-collection)))

(defun log-entry (source status message)
  (store-doc (system-logs-collection)
             (make-instance 'system-log 
                            :source source
                            :user (if (current-user)
                                      (email (current-user))
                                      (parameter "email"))
                            :status status
                            :headers (format nil "~A" (headers-in*))
                            :remote-address (remote-addr*)
                            :entities (if (current-user) 
                                          (get-val (current-user) 'last-context))
                            :message message
                            :stamp nil)))

(defun log-login (source email status message)
  (store-doc (system-logs-collection)
             (make-instance 'system-log 
                            :source source
                            :user email
                            :status status
                            :headers (format nil "~A" (headers-in*))
                            :remote-address (remote-addr*)
                            :entities nil
                            :message message
                            :stamp nil)))


(add-collection (system-db) "system-log" 
                :collection-class 'collection
                :load-from-file-p nil)