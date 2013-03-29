(in-package :dyb)

(defclass entity-relationship (doc)
  ((root :initarg :root
         :initform nil
         :accessor root
         :key t)
   (parent :initarg :parent
           :initform nil
           :accessor parent
           :key t)
   (entity :initarg :entity
           :initform nil
           :accessor entity
           :key t)
   (children :initarg :children 
             :initform ()
             :accessor children))
  (:metaclass storable-versioned-class))

(defun entity-relationships-collection ()
  (get-collection (system-db) "entity-relationships"))

(defun entity-relationships ()
  (docs (entity-relationships-collection)))

(defmethod doc-collection ((doc entity-relationship))
  (entity-relationships-collection))

(defgeneric add-relationship-child (parent child))

(defun find-duplicate-entity-relationship (doc docs-list &key (element 'key))
  (dolist (dup docs-list)
    (let ((dup-entity (get-val dup 'entity))
          (entity (get-val doc 'entity)))
      (if (equal (get-val dup-entity element) (get-val entity element))
          (return-from find-duplicate-entity-relationship dup)))))


;;TODO: Should we not do a replace instead???
(defmethod add-relationship-child ((parent-relationship entity-relationship) child)
  ;;If child already exists dont-add duplicate
  (unless (find-duplicate-entity-relationship 
           child (get-val parent-relationship 'children) 
           :element 'entity-name)
    (setf (get-val parent-relationship 'children) 
          (append (get-val parent-relationship 'children) 
                  (if (listp child)
                      child
                      (list child))))))

(defun map-relationship-tree (relationship func)
  (labels ((rec (relationship)
               (funcall func relationship)
               (if (children relationship)
                   (dolist (entity-rel (children relationship))
                     (rec entity-rel)))))
      (rec relationship)))

(defun add-relationship-tree-child (root parent-eid child-relationship)
  (map-relationship-tree
   root
   (lambda (parent-relationship)
     (when (equal parent-eid (xid (entity parent-relationship)))
       (unless (find-duplicate-entity-relationship
                child-relationship (get-val parent-relationship 'children) :element 'entity-name)
         (setf (root child-relationship) root)
         (setf (parent child-relationship) parent-relationship)
         (setf (children parent-relationship) 
               (append (children parent-relationship) (list child-relationship)))
         (append (children parent-relationship) (list child-relationship)))
       ))))



(defun get-relationship-tree-item-by-id (root relationship-id)
  (map-relationship-tree
   root
   (lambda (rel)
     (when (equal relationship-id (xid rel))
       (return-from get-relationship-tree-item-by-id
         rel)))))

(defun get-relationship-tree-item (root entity-id)
  (map-relationship-tree
   root
   (lambda (rel)
     
     (when (equal entity-id (xid (entity rel)))
       (return-from get-relationship-tree-item
         rel)))))

(defun remove-relationship-tree-child (root child)
  (map-relationship-tree
   root
   (lambda (rel)
     (when (equal (xid (parent child)) (xid (entity rel)))
       (setf (children rel) 
             (delete child (children rel)))))))

(defun make-entity-relationship (root parent-relationship entity children)
  ;(unless root (break "fuck ~A" entity))
  (let ((doc (make-instance 'entity-relationship
                           ;; :xid (next-xid (entity-relationships-collection))
                            
                            :doc-type "entity-relationship"
                            :root (if root (xid (entity root)))
                            :entity entity
                            :parent (if parent-relationship
                                        (entity parent-relationship))
                            :children children)))
    ;;(setf (xdb2::id doc) (next-id (entity-relationships-collection)))
    doc))

(defun get-entity-relationships-ordered (root-id entity-ids)
  (let ((rels ()))
    (map-relationship-tree
     (get-doc (entity-relationships-collection)
               root-id 
               :element 'root)
     (lambda (rel)
       (when (find (xid (entity rel)) entity-ids)
         (setf rels (append rels (list rel) )))))))


(defmethod match-entities ((doc entity-relationship) entity-ids)
  (find (xid (get-val doc 'entity)) entity-ids))


(defun find-root-entities (&optional user)
  (if user 
      (if  (get-val user 'super-user-p)
           (docs (entity-relationships-collection))
           (find-docs 'vector
                      (lambda (doc)
                        (if (string-equal (get-val doc 'doc-status) "active")
                            (match-entities doc (get-val user 'accessible-entities))))
                      (entity-relationships-collection)))
      (docs (entity-relationships-collection))))

(add-collection (system-db) "entity-relationships" 
                :collection-class 'dyb-collection)

(defun get-root (eid) 
  (find-doc 
   (entity-relationships-collection) 
    :test (lambda (doc)
            (if (string-equal (get-val doc 'doc-status) "active")
                (equal eid (xid (entity doc)))))))


;;TODO: Remove this once initial db is up and running.
(defun make-test-tree ()
  
  (let* ((root
          (make-entity-relationship nil nil (persist (make-entity "Client" "Demo Client" )) nil))
         (a (make-entity-relationship root root (persist (make-entity "Mine" "Demo Mine 1" )) nil))
         (b (make-entity-relationship root root (persist (make-entity "Mine" "Demo Mine 2" )) nil))
         (c (make-entity-relationship root root (persist (make-entity "Mine" "Demo Mine 3" )) nil))
         (d (make-entity-relationship root c (persist (make-entity "Contractor" "Contractor 1" )) nil))
         (e (make-entity-relationship root d (persist (make-entity "Community" "Community 1" )) nil))
         (f (make-entity-relationship root root (persist (make-entity "Mine" "Demo Mine 4" )) nil))
         (g (make-entity-relationship root f (persist (make-entity "Contractor" "Contractor 2" )) nil))
         (h (make-entity-relationship root f (persist (make-entity "Contractor" "Contractor 3" )) nil))
         (i (make-entity-relationship root f (persist (make-entity "Community" "Community 2" )) nil))
         (j (make-entity-relationship root f (persist (make-entity "Plant" "Plant 1" )) nil)))

    (setf (children root) (list a b c f))
    (setf (children c) (list d e))
    (setf (children f) (list g h i j))
    root))


;(defparameter tree (make-test-tree))
;(map-relationship-tree tree (lambda (parent) (format t "~A~%" (eid parent))))
;(add-relationship-tree-child tree 10 (make-entity-relationship nil nil 101 nil))
;(remove-relationship-tree-child tree (get-relationship-tree-item tree 101))


;;(persist (make-test-tree))



(defun quicklook-relationships ()
  (with-output-to-string (stream)
      (dolist (tree (coerce (entity-relationships) 'list))
        (map-relationship-tree tree 
                               (lambda (relationship)
                                 (format stream "~%~A --id ~A --xid ~A --parent ~A --entity ~A --doc-status ~A --version ~A"
                                         (if (parent relationship) 
                                             "----> "
                                             "")
                                         (xdb2::id relationship)
                                         (xid relationship)
                                         (if (parent relationship)
                                             (get-val (parent relationship) 'entity-name)) 
                                         (if (entity relationship)
                                             (get-val (entity relationship) 'entity-name)) 
                                         (doc-status relationship)
                                         (version relationship)))))
      stream)
  )


#|(map-relationship-tree (get-root 114) 
                               (lambda (relationship)
                                 (format t "~% ~A ~A --version ~A" (parent relationship) (eid relationship) (version relationship
                                                                                                           ))))|#
