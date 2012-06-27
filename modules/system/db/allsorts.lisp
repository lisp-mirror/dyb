(in-package :ems)

(defclass allsort (doc import-detail)
  ((sort :initarg :sort 
         :initform nil)
   (sort-value :initarg :sort-value 
               :initform nil)
   (sort-order :initarg :sort-order 
               :initform nil)   
   (alternate-sort-order :initarg :alternate-sort-order 
                         :initform nil)
   (aa-sort-order :initarg :aa-sort-order 
                  :initform nil)
   (description :initarg :description 
                :initform nil)
   (extended-description :initarg :extended-description 
                         :initform nil))
  (:metaclass storable-class))

(defun allsorts-collection ()
  (get-collection (system-db) "allsorts"))

(defun allsorts ()
  (docs (allsorts-collection)))

(defmethod persist-doc ((doc allsort) &key (force-stamp-p t))
  (store-doc (allsorts-collection) doc :force-stamp-p force-stamp-p))

(defun make-allsort (sort sort-value &key
                     sort-order alternate-sort-order
                     aa-sort-order description
                     extended-description)
  (make-instance 'allsort 
                 :key (list sort sort-value) 
                 :doc-type "allsort" 
                 :xid (next-xid (allsorts-collection))
                 :sort sort
                 :sort-order sort-order
                 :sort-value sort-value
                 :alternate-sort-order alternate-sort-order
                 :aa-sort-order aa-sort-order
                 :description description
                 :extended-description extended-description))

(defun get-allsort (sort value)
  (get-doc (allsorts-collection) 
           (list sort value)))

(defun find-allsorts (sort)
  (find-docs 'list
             (lambda (doc)
               (string-equal (get-val doc 'sort) sort))
             (allsorts-collection)))

(defun find-allsorts-for-select (sort)
  (let ((docs (find-docs 'list
                         (lambda (doc)
                           (and (string-equal (get-val doc 'sort) sort)
                                (string-not-equal (get-val doc 'doc-status) "superseded")))
                         (allsorts-collection)))
        (data))
    (dolist (doc docs)
      (setf data (append data (list (list (get-val doc 'sort-value)
                                          (get-val doc 'description))))))
    data))

(defun find-allsorts-for-validation (sort)
  (let ((docs (find-docs 'list
                         (lambda (doc)
                           (and (string-equal (get-val doc 'sort) sort)
                                (string-not-equal (get-val doc 'doc-status) "superseded")))
                         (allsorts-collection)))
        (data))
    (dolist (doc docs)
      (setf data (append data (list (get-val doc 'sort-value)))))
    data))

(defun get-allsort-by-id (id)
  (get-doc (allsorts-collection) id
                       :element 'xid))

(defgeneric match-allsorts (doc allsorts))

(defmethod match-allsorts (doc allsorts)
  (find (get-val doc 'xid) allsorts))

(add-collection (system-db) "allsorts" 
                :collection-class 'ems-collection
                :load-from-file-p t)

(persist-doc (make-allsort "Core Mining" "Yes" :description "Yes"))
(persist-doc (make-allsort "Core Mining" "No" :description "No"))
(persist-doc (make-allsort "User Preferences" "Last Context" :description "Last Context"))
(persist-doc (make-allsort "Action Status" "Started" :description "Started"))
(persist-doc (make-allsort "Action Status" "Not Started" :description "Not Started"))
(persist-doc (make-allsort "Action Status" "Completed" :description "Completed"))
(persist-doc (make-allsort "Action Status" "Aborted" :description "Aborted"))
(persist-doc (make-allsort "Address Type" "Postal" :description "Postal"))
(persist-doc (make-allsort "Address Type" "Physical" :description "Physical"))

