(in-package :ems)

(defclass ems-collection (collection)
  ())

(defgeneric max-xid (collection))

(defmethod max-xid ((col ems-collection))
  (max-val col :element 'xid))

(defgeneric next-xid (collection))

(defmethod next-xid ((col ems-collection))
  (next-sequence (list (xdb2::name col) 'xid)))

(defclass date-doc ()
  ((start-date :initarg :start-date
               :initform nil
               :accessor start-date)
   (end-date :initarg :end-date
               :initform nil
               :accessor end-date))
  (:metaclass storable-class))

(defclass import-detail ()
  ((import-id :initarg :import-id
              :initform nil
              :accessor import-id))
  (:metaclass storable-class))

(defclass documentx ()
  ((key :initarg :key
        :accessor key)
   (doc-type :initarg :doc-type
         :initform nil
         :accessor doc-type))
  (:metaclass storable-class))

(defclass doc (documentx)
  ((xid :initarg :xid
        :initform nil
        :accessor xid)
   (version :initarg :version
            :initform 1
            :accessor version
            :storep nil)
   (old-versions :initarg :old-versions
                 :initform nil
                 :accessor old-versions
                 :storep nil)
   (collection :initarg :collection
               :initform nil
               :accessor collection
               :storep nil)
   (top-level :initarg :top-level
              :initform nil
              :accessor top-level)
   (stamp-date :initarg :stamp-date
               :initform nil
               :accessor stamp-date)
   (effective-date :initarg :effective-date
                   :initform nil
                   :accessor effective-date)
   (doc-status :initarg :doc-status 
               :initform "active"
               :accessor doc-status
               :documentation "Active, deleted, imported, invalidated, superseded.")
   (user :initarg :user 
         :initform nil
         :accessor user)
   (log-action :initarg :log-action 
               :initform nil
               :accessor log-action
               :documentation "Inserted, updated, deleted, rolledback."))
  (:metaclass storable-class))

(defgeneric doc-collection (doc))

(defvar *inhibit-change-marking* nil)

(defun supersede (object old-object)
  (let ((*inhibit-change-marking* t))
    (setf (doc-status old-object) "superseded"
          (effective-date old-object) (stamp-date object))
    (push old-object (old-versions object))))

(defmethod load-from-file ((collection ems-collection) file)
  (when (probe-file file)
    (load-data collection file
               (lambda (object &key copy)
                 (let ((*inhibit-change-marking* t))
                  (cond ((not (typep object 'doc))
                         (when (not (typep object 'storable-object))
                           (vector-push-extend object (docs collection))))
                        (copy
                         (supersede object copy))
                        ((top-level object)
                         (setf (collection object) collection)
                         (vector-push-extend object (docs collection)))
                        (t
                         (setf (collection object) collection)))))
               (lambda (object)
                 (alexandria:deletef (docs collection) object)))))

(defmethod remove-doc ((object storable-object))
  (let ((collection (doc-collection object)))
    (alexandria:deletef (docs collection) object)
    (delete-doc collection object)))

(defmethod update-doc ((object storable-object) &key (set-time t))
  (when set-time
    (setf (stamp-date object) (get-universal-time)))
  (serialize-doc (collection object) object))

(defmethod persist ((doc storable-object) &key old-object (set-time t)
                                               (top-level t))
  (declare (ignore old-object))
  (let ((collection (doc-collection doc)))    
    (when (not (get-val doc 'xdb2::id)) 
      (setf (version doc) 0
            (xid doc) (or (xid doc)
                          (next-xid collection))
            (doc-status doc) (or (doc-status doc) "active")
            (log-action doc) (or (log-action doc) "inserted")
           
            (top-level doc) top-level)
      (when top-level
        (vector-push-extend doc (docs collection))))
    (setf (collection doc) collection)
    ;;Doing or because conversion is not run where session is available.
    (setf (user doc) (or (and (current-user)
                              (email (current-user)))
                         "admin@ems.co.za"))
    (update-doc doc :set-time set-time))
  doc)

(defmethod (setf slot-value-using-class)
    (new-value (class storable-class) (object doc) slotd)
  (when (and (not *inhibit-change-marking*)
             (slot-boundp-using-class class object slotd)
             (written object))
    (let ((current-value (slot-value-using-class class object slotd))
          (*inhibit-change-marking* t))
      (unless (equal new-value current-value)
        (setf (written object) nil)
        (supersede object (copy object))
        (incf (version object)))))
  (call-next-method))

;;;

(defgeneric match-any (doc criteria))

(defmethod match-any ((doc doc) criteria)
  (dolist (slot (class-slots (class-of doc)))
    (let ((slot-name (slot-definition-name slot)))
      (cond ((listp criteria)
             (if (find (get-val doc slot-name) criteria)
               (return-from match-any doc)))
            (t
             (if (equal (get-val doc slot-name) criteria)
               (return-from match-any doc)))))))

;;TODO: Add start and end date check
(defmethod duplicate-doc-p ((doc date-doc) test-doc)
  (or (eq doc test-doc)
      (equal (get-val doc 'key) (get-val test-doc 'key))))

;;TODO: Add stamp date check to duplicate check.
(defmethod duplicate-doc-p ((doc doc) test-doc)
  (or (eq doc test-doc)
      (equal (get-val doc 'key) (get-val test-doc 'key))))



;;TODO: Make sure the docs are sorted in stamp oder before searching! 
;;We want to find the most relevant duplicate document not just any duplicate document.
(defmethod find-duplicate-doc ((collection ems-collection) doc 
                               &key function
                               (ignore-superseded-p t))
  (let ((test (or function #'duplicate-doc-p))
        (result-doc))
    (map-docs
     nil
     (lambda (docx)
       (if ignore-superseded-p
           (unless (string-equal (get-val doc 'doc-status) "superseded")
               (when (funcall test doc docx)
             (setf result-doc docx)))
           (when (funcall test doc docx)
             (setf result-doc docx))))
     collection)
    result-doc))


(defgeneric invalidate-doc (doc &key))

(defmethod invalidate-doc ((doc doc) &key)
  (setf (doc-status doc) "Invalidated")
  (persist doc))

;;Find the last doc that matches to get the lastest version
(defmethod get-doc ((collection ems-collection) value  
                    &key (element 'key) (test #'equal) (ignore-superseded-p t))
  (map-docs
     nil
     (lambda (doc)
       (if ignore-superseded-p
           (unless (string-equal (get-val doc 'doc-status) "superseded")
                   (when (funcall test (get-val doc element) value)
                     (return-from get-doc doc)))
           (when (funcall test (get-val doc element) value)
                     (return-from get-doc doc)) ))
     collection))

;;Find the last doc that matches to get the lastest version
(defmethod find-doc ((collection ems-collection) &key test (ignore-superseded-p t))
  (let ((found-doc))
    (if test
        (map-docs
         nil
         (lambda (doc)
           (if ignore-superseded-p
               (unless (string-equal (get-val doc 'doc-status) "superseded")
                   (when (funcall test doc)
                     (setf found-doc doc)))
               (when (funcall test doc)
                     (setf found-doc doc))))
         collection))
    found-doc))

(defun quicklook-docs (docs)
  (dolist (doc (coerce docs 'list))
          (format t "~%--id ~A --written ~A --key ~A --version ~A --xid ~A --log-action ~A --status ~A --stamp ~A" 
                  (get-val doc 'xdb2::id) (get-val doc 'xdb2::written) (key doc) (version doc) (xid doc) (log-action doc) (doc-status doc) (stamp-date doc))))

(defun activep (doc)
  (and (typep doc 'doc)
       (equal (doc-status doc) "active")))
