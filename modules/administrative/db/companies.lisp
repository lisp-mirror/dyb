(in-package :ems)

(defclass company (doc)
  ((entity :initarg :entity 
           :initform nil)
   (registration-no :initarg :registration-no 
                 :initform nil
                 :accessor registration-no)
   (vat-no :initarg :vat-no 
           :initform nil
           :accessor vat-no)
   (addresses 
    :initarg :addresses
    :initform nil
    :documentation "List of addresses.")
   (contacts 
    :initarg :contacts
    :initform nil
    :documentation "List of contacts."))
  (:metaclass storable-class))

(defun companies-collection ()
  (get-collection (system-db) "companies"))

(defun companies ()
  (docs (companies-collection)))

(defmethod persist-doc ((doc company) &key (force-stamp-p t))
  (store-doc (companies-collection) doc :force-stamp-p force-stamp-p))


(defun make-company (entity &key registration-no vat-no contacts addresses)
  (make-instance 'company :key (xid entity) :doc-type "company" 
                 :xid (next-xid (companies-collection))
                 :entity entity
                 :registration-no registration-no
                 :vat-no vat-no
                 :contacts contacts
                 :addresses addresses))

(defun get-company (company-name)
  (find-doc (companies-collection)
            :test (lambda (doc)
                    (and (string-equal (get-val doc 'doc-status) "Superceded")
                         (string-equal (get-val (get-val doc 'entity) 'entity-name) 
                                       company-name)))))

(defun get-company-by-id (id)
  (get-doc (companies-collection) id
                       :element 'xid))

(defgeneric match-companies (doc companies))

(defmethod match-companies (doc companies)
  (find (get-val doc 'xid) companies))

(add-collection (system-db) "companies" 
                :collection-class 'ems-collection
                :load-from-file-p t)









