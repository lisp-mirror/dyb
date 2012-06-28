(in-package :ems)

(defclass entity-type (doc)
  ((entity-type-name :initarg :entity-type-name
         :accessor entity-type-name)
   (description :initarg :description
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
                 :xid (next-xid (entity-types-collection))
                 :entity-type-name entity-type-name
                 :description description))

(defmethod persist-doc ((doc entity-type) &key (force-stamp-p t))
  (store-doc (entity-types-collection) doc :force-stamp-p force-stamp-p))


(add-collection (system-db) "entity-types" 
                :collection-class 'ems-collection
                :load-from-file-p t)

(persist-doc (make-entity-type "Client" "Client"))
(persist-doc (make-entity-type "Company" "Company" ))
(persist-doc (make-entity-type "Building" "Building" ))
(persist-doc (make-entity-type "Complex" "Complex" ))
(persist-doc (make-entity-type "Country" "Country" ))
(persist-doc (make-entity-type "Province/State" "Province/State" ))
(persist-doc (make-entity-type "Meter" "Meter" ))
(persist-doc (make-entity-type "Unit" "Unit" ))

(defclass entity (doc)
  ((entity-type :initarg :entity-type 
                :accessor entity-type)
   (entity-name :initarg :entity-name 
                 :initform nil
                 :accessor entity-name))
  (:metaclass storable-class))

(defun entities-collection ()
  (get-collection (system-db) "entities"))

(defun entities ()
  (docs (entities-collection)))

(defmethod persist-doc ((doc entity) &key (force-stamp-p t))
  (store-doc (entities-collection) doc :force-stamp-p force-stamp-p))


(defun make-entity (entity-type entity-name)
  (make-instance 'entity 
                 :key entity-name 
                 :doc-type "entity" 
                 :xid (next-xid (entities-collection))
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
  (if (get-val doc 'entity)
      (find (xid (get-val doc 'entity)) (last-context (current-user)))))

(defun entity-list ()
  (let ((e-list))
    (dolist (doc (coerce (entities) 'list))
      (if (not (string-equal (get-val doc 'doc-status) "superseded"))
          (setf e-list (append e-list (list (list (get-val doc 'xid) 
                                                  (get-val doc 'entity-name)))))))
    e-list))

(defun entity-type-list ()
  (let ((e-list))
    (dolist (doc (coerce (entity-types) 'list))
      (if (not (string-equal (get-val doc 'doc-status) "superseded"))
          (setf e-list (append e-list (list (list (get-val doc 'xid) 
                                                  (get-val doc 'entity-type-name)))))))
    e-list))

(add-collection (system-db) "entities" 
                :collection-class 'ems-collection
                :load-from-file-p t)





