(in-package :dyb)

(defclass period (doc)
  ((entity :initarg :entity
           :initform nil
           :accessor entity
           :key t)
   (period-name :initarg :period-name 
                :initform nil
                :accessor period-name
                :key t)
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
  (:metaclass storable-versioned-class))

(defun periods-collection ()
  (get-collection (system-db) "periods"))

(defun periods ()
  (docs (periods-collection)))

(defmethod doc-collection ((doc period))
  (periods-collection))

(defun make-period (entity
                    period-name period-type description 
                    start-date end-date status)
  (make-instance 'period
                 :entity entity
                 :period-name period-name
                 :period-type period-type
                 :description description
                 :start-date start-date
                 :end-date end-date
                 :status status))

(defun get-period (entity-id period-name)
  (get-doc (periods-collection)   
           (list (if (stringp entity-id)
                     (parse-integer entity-id)
                     entity-id)
                 (format nil "~A" period-name))))

(defun get-period-by-id (id)
  (get-doc (periods-collection) id
           :element 'xid))

(defgeneric match-periods (doc periods))

(defmethod match-periods (doc periods)
  (find (get-val doc 'xid) periods))

(add-collection (system-db) "periods" 
                :collection-class 'dyb-collection)

