(in-package :dyb)

(defclass entity-type (doc)
  ((entity-type-name :initarg :entity-type-name
                     :initform nil
                     :accessor entity-type-name)
   (description :initarg :description
                :initform nil
                :accessor description))
  (:metaclass storable-class))

(defun entity-types-collection ()
  (get-collection (system-db) "entity-types"))

(defun entity-types ()
  (docs (entity-types-collection)))

(defun get-entity-type (entity-type-name)
  (get-doc (entity-types-collection) entity-type-name :element 'entity-type-name))

(defun get-entity-type-by-id (entity-type-id)
  (get-doc (entity-types-collection) entity-type-id :element 'xid))

(defun make-entity-type (entity-type-name description)
  (make-instance 'entity-type :key entity-type-name :doc-type "entity-type" 
                
                 :entity-type-name entity-type-name
                 :description description))

(defmethod doc-collection ((doc entity-type))
  (entity-types-collection))


(add-collection (system-db) "entity-types" 
                :collection-class 'dyb-collection
                :load-from-file-p t)

(when (= (length (entity-types)) 0)
    (persist (make-entity-type "Client" "Client"))
    (persist (make-entity-type "Company" "Company" ))
    (persist (make-entity-type "Mine" "Mine" ))
    (persist (make-entity-type "Contractor" "Contractor" ))
    (persist (make-entity-type "Community" "Community" ))
    (persist (make-entity-type "Division" "Division" ))
    (persist (make-entity-type "Department" "Department" ))
    (persist (make-entity-type "Plant" "Plant" ))
    (persist (make-entity-type "Smelter" "Smelter" ))
    (persist (make-entity-type "Country" "Country" ))
    (persist (make-entity-type "Province/State" "Province/State" )))

(defclass entity (doc)
  ((entity-type :initarg :entity-type
                :initform nil
                :accessor entity-type)
   (entity-name :initarg :entity-name 
                :initform nil
                :accessor entity-name))
  (:metaclass storable-class))

(defun entities-collection ()
  (get-collection (system-db) "entities"))

(defun entities ()
  (docs (entities-collection)))

(defmethod doc-collection ((doc entity))
  (entities-collection))

(defun make-entity (entity-type entity-name)
  (make-instance 'entity :key entity-name :doc-type "entity" 
                 
                 :entity-type (get-entity-type entity-type) 
                 :entity-name entity-name))

(defun get-entity (entity-name)
  (get-doc (entities-collection) entity-name
                       :element 'entity-name))

(defun get-entity-by-id (id)
  (get-doc (entities-collection) id
                       :element 'xid))

(defgeneric match-entities (doc entities))

(defmethod match-entities (doc entities)
  (find (get-val doc 'xid) entities))

(defgeneric match-context-entities (doc)
  )

(defmethod match-context-entities (doc)
  (if (typep doc 'entity)
    (find (xid doc) (context))
    (if (typep (get-val doc 'entity) 'entity)
        (find (xid (get-val doc 'entity)) (context)))))


(defun entity-list-no-context ()
  (let ((e-list))
    (dolist (doc (coerce (entities) 'list))
      (if (not (string-equal (get-val doc 'doc-status) "superseded"))
          (setf e-list (append e-list (list (list (get-val doc 'xid) 
                                                  (get-val doc 'entity-name)))))))
    e-list))

(defun entity-list ()
  (let ((e-list))
    (dolist (doc (coerce (entities) 'list))
      (if (and (not (string-equal (get-val doc 'doc-status) "superseded"))
               (find (xid doc) (context)))
          (setf e-list (append e-list (list (list (get-val doc 'xid) 
                                                  (get-val doc 'entity-name)))))))
    e-list))

(defun admin-entity-list ()
  (let ((e-list))
    (dolist (doc (coerce (entities) 'list))
      (if (not (string-equal (get-val doc 'doc-status) "superseded"))
          (setf e-list (append e-list (list (list (get-val doc 'xid) 
                                                  (get-val doc 'entity-name)))))))
    e-list))

(defun entity-mine-list ()
  (let ((e-list))
    (dolist (doc (coerce (entities) 'list))
      (if (and (not (string-equal (get-val doc 'doc-status) "superseded"))
               (find (xid doc) (context)))
          (if (string-equal (get-val (entity-type doc) 'entity-type-name)  "Mine")
              (setf e-list (append e-list (list (list (get-val doc 'xid) 
                                                      (get-val doc 'entity-name))))))))
    e-list))

(defun entity-type-list ()
  (let ((e-list))
    (dolist (doc (coerce (entity-types) 'list))
      (if (not (string-equal (get-val doc 'doc-status) "superseded"))
          (setf e-list (append e-list (list (list (get-val doc 'xid) 
                                                  (get-val doc 'entity-type-name)))))))
    e-list))

(defun print-context ()
  (let ((e-list))
    (dolist (id (coerce (context) 'list))
      (setf e-list (append e-list (list (format nil "[~A]" (get-val (get-entity-by-id id) 'entity-name ))))))
    e-list))

(add-collection (system-db) "entities" 
                :collection-class 'dyb-collection
                :load-from-file-p t)





