(in-package :dyb)

(defclass xpack-import-mapping ()
  ((import-slot :initarg :import-slot)
   (file-column :initarg :file-column)))

(defclass xpack-import (doc )
  ((verification-set :initarg :verification-set :initform nil)
   (import-type :initarg :import-type 
                  :initform nil)
   (original-file-name :initarg :original-file-name 
                       :initform nil)
   (file-name :initarg :file-name :initform nil)
   (import-status :initarg :import-status
                  :initform nil)
   (mapping :initarg :mapping 
                      :initform nil))
  (:metaclass storable-class))

(defun xpack-imports-collection ()
  (get-collection (system-db) "xpack-imports"))

(defun xpack-imports ()
  (docs (xpack-imports-collection)))

(defmethod doc-collection ((doc xpack-import))
  (xpack-imports-collection))

(defun make-xpack-import (verification-set import-type original-file-name file-name
                      &key mapping)
  (make-instance 'xpack-import :key file-name 
                 :doc-type "xpack-import" 
            
                 :verification-set verification-set
                 :import-type import-type
                 :original-file-name original-file-name
                 :file-name file-name
                 :import-status "File Uploaded"
                 :mapping mapping))

(defun get-xpack-import (verification-set import-type original-file-name)
  (get-doc (xpack-imports-collection) (list verification-set import-type original-file-name)))

(defun get-xpack-import-by-id (id)
  (get-doc (xpack-imports-collection) id
                       :element 'xid))

(defgeneric match-xpack-imports (doc xpack-imports))

(defmethod match-xpack-imports (doc xpack-imports)
  (find (get-val doc 'xid) xpack-imports))

(add-collection (system-db) "xpack-imports" 
                :collection-class 'ems-collection
                :load-from-file-p t)

