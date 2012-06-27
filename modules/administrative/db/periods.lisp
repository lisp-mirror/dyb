(in-package :ems)

(defclass period (doc)
  ((entity-relationship :initarg :entity-relationship
                        :initform nil)
   (period-name :initarg :period-name 
                :initform nil
                :accessor period-name)
   (period-type :initarg :period-type
                :initform nil
                :documentation "Years, Year, Quater, Month, Week")
   (description :initarg :description
                :initform nil)   
   (start-date :initarg :start-date
               :initform nil
               :accessor start-date)
   (end-date :initarg :end-date
             :initform nil
             :accessor end-date)
   (status :initarg :status :initform nil
           :documentation "Open, Closed"))
  (:metaclass storable-class))

(defun periods-collection ()
  (get-collection (system-db) "periods"))

(defun periods ()
  (docs (periods-collection)))

(defmethod persist-doc ((doc period) &key (force-stamp-p t))
  (store-doc (periods-collection) doc :force-stamp-p force-stamp-p))


(defun make-period (entity-relationship
                    period-name period-type description 
                    start-date end-date status)
  (make-instance 'period :key (list (get-val entity-relationship 'xid) period-name) 
                 :doc-type "period" 
                 :xid (next-xid (periods-collection))
                 :entity-relationship entity-relationship
                 :period-name period-name
                 :period-type period-type
                 :description description
                 :start-date start-date
                 :end-date end-date
                 :status status))

(defun get-period (entity-relationship-id period-name)
  (get-doc (periods-collection) 
            (list entity-relationship-id period-name)))

(defun get-period-by-id (id)
  (get-doc (periods-collection) id
                       :element 'xid))

(defgeneric match-periods (doc periods))

(defmethod match-periods (doc periods)
  (find (get-val doc 'xid) periods))

(add-collection (system-db) "periods" 
                :collection-class 'ems-collection
                :load-from-file-p t)

