(in-package :dyb)

(defclass generic-action (doc)
  ((pid :initarg :pid :accessor generic-entry-pid)
   (post-type :initarg :post-type)
   (from-user-id :initarg :from-user-id)
   (to-user-id :initarg :to-user-id)
   (action-type :initarg :action-type)
   (action-content :initarg :action-content)
   (action-status :initarg :action-status)
   (action-log :initarg :action-log)
   (image-url :initarg :image-url)
   (post-url :initarg :post-url)
   (short-url :initarg :short-url)
   (scheduled-date :initarg :scheduled-date))
  (:metaclass storable-class))


(defun generic-actions-collection ()
  (get-collection (system-db) "generic-actions"))

(defmethod doc-collection ((doc generic-action))
  (generic-actions-collection))

(defun generic-actions ()
  (docs (generic-actions-collection)))

(defun get-generic-action-by-id (id)
  (get-doc (generic-actions-collection) id
           :element 'id))

(defun get-generic-action-by-post-id (id)
  (get-doc (generic-actions-collection) id
           :element 'pid))

(defun make-generic-action (pid post-type from-user to-user action-type 
                            action-content scheduled-date &key image-url post-url short-url)
  (make-instance 'generic-action 
                 :pid pid
                 :post-type post-type
                 :from-user-id from-user
                 :to-user-id to-user
                 :action-type action-type
                 :action-content action-content
                 :image-url image-url
                 :post-url post-url
                 :short-url short-url
                 :scheduled-date scheduled-date
                 :action-status "Pending"
                 ))
(add-collection (system-db) "generic-actions" 
                :collection-class 'dyb-collection
                :load-from-file-p t)