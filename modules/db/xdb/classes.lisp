(in-package :dyb)

(defclass dyb-collection (collection)
  ())

(defclass storable (storable-versioned-class)
  ())

(defgeneric max-xid (collection))

(defmethod max-xid ((col dyb-collection))
  (max-val col :element 'xid))

(defgeneric next-xid (collection))

(defmethod next-xid ((col dyb-collection))
  (next-sequence (list (xdb2::name col) 'xid)))

(defclass date-doc ()
  ((start-date :initarg :start-date
               :initform nil
               :accessor start-date)
   (end-date :initarg :end-date
               :initform nil
               :accessor end-date)))

(defclass doc ()
  ((xid :initarg :xid
        :initform nil
        :accessor xid)
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
  (:metaclass storable))

(defmethod persist :before ((doc doc) &key)
  (let ((collection (doc-collection doc)))
    (when (not (get-val doc 'xdb2::id)) 
      (setf (xid doc) (or (xid doc)
                          (next-xid collection))
            (doc-status doc) (or (doc-status doc) "active")
            (log-action doc) (or (log-action doc) "inserted")))
    ;;Doing or because conversion is not run where session is available.
    (setf (user doc) (or (and (current-user)
                              (email (current-user)))
                         (user doc)
                         "admin@dyb.co.za"))))

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
(defmethod find-duplicate-doc ((collection dyb-collection) doc 
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
(defmethod get-doc ((collection dyb-collection) value  
                    &key element (test #'equal) (ignore-superseded-p t))
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
(defmethod find-doc ((collection dyb-collection) &key test (ignore-superseded-p t))
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
