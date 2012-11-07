(in-package :dyb)

(defclass task (doc)
  ((entity :initarg :entity)
   (task-description :initarg :task-description)
   (assigned-user :initarg :assigned-user)
   (assigning-user :initarg :assigning-user)
   (task-instructions :initarg :task-instructions)
   (task-status :initarg :task-status
                :documentation "Assigned,In Progress,Completed,Abandoned")
   (assigned-date :initarg :assigned-date)
   (scheduled-date :initarg :scheduled-date)
   (completed-date :initarg :completed-date))
  (:metaclass storable-class))


(defun tasks-collection ()
  (get-collection (system-db) "tasks"))

(defmethod doc-collection ((doc task))
  (tasks-collection))

(defun tasks ()
  (docs (tasks-collection)))

(defun get-task-by-id (id)
  (get-doc (tasks-collection) id
           :element 'xid))

(defun make-task (entity task-description assigning-user assigned-user 
                  scheduled-date &key task-instructions
                  (assigned-date (get-universal-time))
                  (task-status "Assigned")
                  completed-date)
  (make-instance 'task 
                 :key (list (xid entity) 
                            (xid assigned-user)
                            task-description)
                 :entity entity
                 :task-description task-description
                 :assigning-user assigning-user
                 :assigned-user assigned-user
                 :task-instructions task-instructions
                 :task-status task-status
                 :scheduled-date scheduled-date
                 :assigned-date assigned-date
                 :completed-date completed-date
                                 ))
(add-collection (system-db) "tasks" 
                :collection-class 'dyb-collection
                :load-from-file-p t)