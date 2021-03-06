(in-package :dyb)

(defclass company (doc)
  ((entity :initarg :entity)
   (company-name :initarg :company-name 
                 :initform nil
                 :accessor company-name
                 :key t)
   (contact-person :initarg :contact-person)
   (contact-number :initarg :contact-number))
  (:metaclass storable-versioned-class))

(defun companies-collection ()
  (get-collection (system-db) "companies"))

(defun companies ()
  (docs (companies-collection)))

(defmethod doc-collection ((doc company))
  (companies-collection))

(defun make-company (company-name)
  (make-instance 'company
                 :company-name company-name))

(defun get-company (company-name)
  (get-doc (companies-collection) company-name
                       :element 'company-name))

(defun get-company-by-id (id)
  (get-doc (companies-collection) id
                       :element 'xid))

(defgeneric match-companies (doc companies))

(defmethod match-companies (doc companies)
  (find (get-val doc 'xid) companies))

(add-collection (system-db) "companies")









