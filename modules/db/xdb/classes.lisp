(in-package :ems)

(defclass ems-collection (collection)
())

(defgeneric max-xid (collection))

(defmethod max-xid ((col ems-collection))
  (max-val col :element 'xid))

(defgeneric next-xid (collection))

(defmethod next-xid ((col ems-collection))
  (next-sequence (list (xdb2::name col) 'xid)))


(defmethod next-id ((col ems-collection))
  (next-sequence (list (xdb2::name col) 'id)))

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
       :accessor xid)
   (version :initarg :version
             :initform 1
             :accessor version)
   (stamp-date :initarg :stamp-date
               :initform nil
               :accessor stamp-date)
   (effective-date :initarg :stamp-date
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

(defgeneric copy-instance (instance &rest initargs))

(defmethod copy-instance ((doc doc) &rest initargs)
  (apply #'copy-object doc 'standard-class initargs))


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

(defmethod add-doc ((collection ems-collection) (doc doc) 
                    &key (duplicate-doc-p-func 'duplicate-doc-p)
                    (force-stamp-p t)
                    (ignore-superseded-p t))
  (when (and doc collection)    
    (when force-stamp-p
      (setf (stamp-date doc) (get-universal-time)))

    (unless (get-val doc 'stamp-date)
      (setf (stamp-date doc) (get-universal-time)))

    (let ((dup (and duplicate-doc-p-func
                    (find-duplicate-doc collection doc
                                        :function duplicate-doc-p-func
                                        :ignore-superseded-p ignore-superseded-p))))
      (when dup
        
        ;;This is to enable versioning of doc.
        (setf (version doc) (+ (get-val dup 'version)  1))
        ;;Keep the application-id the same accross versions.
        (setf (xid doc) (xid dup))
        (setf (get-val doc 'xdb2::id) nil)
        (setf (log-action doc) "updated")
        (setf (doc-status dup) "superseded")
        
        )
      
      (when (or (not (get-val doc 'version)) (not (get-val doc 'xid)))
          (setf (get-val doc 'version) 1)
          (setf (get-val doc 'xdb2::id) nil)
          (setf (get-val doc 'xdb2::written) nil)
          (setf (xid doc) (next-xid collection))
          (setf (log-action doc) "inserted"))

      (unless (get-val doc 'user)
        
        (setf (get-val doc 'user) (if (current-user)
                                      (get-val (current-user) 'email))))
      
      (vector-push-extend doc (docs collection)))
    doc))


(defmethod store-doc ((collection ems-collection) (doc doc)
                      &key (duplicate-doc-p-func 'duplicate-doc-p)
                      (force-stamp-p t)
                      (ignore-superseded-p t))
  (when (and doc collection :duplicate-doc-p-func duplicate-doc-p-func)
    (when (and doc collection)    
    (when force-stamp-p
      (setf (stamp-date doc) (get-universal-time)))

    (unless (get-val doc 'stamp-date)
      (setf (stamp-date doc) (get-universal-time)))

    (let ((dup (and duplicate-doc-p-func
                    (find-duplicate-doc collection doc
                                        :function duplicate-doc-p-func
                                        :ignore-superseded-p ignore-superseded-p))))
      (when dup
       ; (when (eq doc dup)
      ;    (break "shit ? ~a" doc)
       ;     (setf doc (copy doc)))

        ;;This is to enable versioning of doc.
        (setf (version doc) (+ (get-val dup 'version)  1))
        ;;Keep the application-id the same accross versions.
        (setf (xid doc) (xid dup))
        (setf (get-val doc 'xdb2::id) nil)
        (setf (get-val doc 'xdb2::written) nil)
        (setf (log-action doc) "updated")

        (setf (doc-status dup) "superseded")
        ;(setf (get-val dup 'xdb2::written) nil)          
        (serialize-doc collection dup)
        )
      
      (when (or (not (get-val doc 'version)) (not (get-val doc 'xid)))
          (setf (get-val doc 'version) 1)
          (setf (get-val doc 'xdb2::id) nil)
          (setf (get-val doc 'xdb2::written) nil)
          (setf (xid doc) (next-xid collection))
          (setf (xdb2::id doc) (next-id collection))
          (setf (doc-status doc) "active")
          (setf (log-action doc) "inserted"))

      (unless (get-val doc 'user)
        
        (setf (get-val doc 'user) (if (current-user)
                                      (get-val (current-user) 'email))))
      
      (vector-push-extend doc (docs collection)))
    (serialize-doc  collection doc))
    ))


(defgeneric persist-doc (doc &key force-stamp-p))
(defgeneric invalidate-doc (doc &key))

(defmethod invalidate-doc ((doc doc) &key)
  (setf (doc-status doc) "Invalidated")
  (persist-doc doc))

(defgeneric delete-doc (doc &key))
(defgeneric remove-doc (doc &key))


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
                     (setf found-doc doc) )))
         collection))
    found-doc))

(defun quicklook-docs (docs)
  (dolist (doc (coerce docs 'list))
          (format t "~%--id ~A --written ~A --key ~A --version ~A --xid ~A --log-action ~A --status ~A --stamp ~A" 
                  (get-val doc 'xdb2::id) (get-val doc 'xdb2::written) (get-val doc 'key) (version doc) (xid doc) (log-action doc) (doc-status doc) (stamp-date doc))))


(defun activep (doc)
  (and (typep doc 'doc)
       (equal (doc-status doc) "active")))