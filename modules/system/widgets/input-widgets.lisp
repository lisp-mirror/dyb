(in-package :dyb)

(defclass chained-select (input-widget)
  ((select-names :initarg :select-names
		 :initform ()
		 :accessor select-names)
   (selects :initform ()
            :accessor selects)))

(defclass chained-select-sub-select (select ajax-widget)
  ((chained-select :initarg :chained-select
                   :initform nil
                   :accessor chained-select)))

(defmethod initialize-instance :after ((widget chained-select)
				       &key select-names)
  (setf (selects widget)
        (loop for name in select-names
              for downcased = (string-downcase name)
              collect (make-widget 'chained-select-sub-select
                                   :name downcased
                                   :chained-select widget))))

(defmethod render ((widget chained-select-sub-select) &key)
  (let* ((parent (chained-select widget))
         (selects (selects parent))
         (member (member widget selects))
         (next-select (cadr member))
         (values (retrieve-values parent
                                  widget
                                  (mapcar #'value (ldiff selects member)))))
    (setf (items widget) values
          ;(value widget) (car values)
          )
    (when next-select
      (defer-js (frmt "$('#~a select').trigger('change')" (name widget)))
      (defer-js (frmt "$('#~a select').change(function(){~a;})"
                      (name widget)
                      (js-render next-select (frmt "[~s, $('#~a select').val()]"
                                                   (name widget)
                                                   (name widget))))))
    (call-next-method)))

(defgeneric retrieve-values (chained-select select values))

(defmethod action-handler ((select chained-select))
  (setf (value select)
            (mapcar #'value (selects select)))

  )

(defclass country-town-select (chained-select)
  ()
  (:metaclass widget-class)
  (:default-initargs :select-names '(country province town)))

(defun get-all-countries ()
  (map 'list #'country (remove-duplicates (country-towns)
                                          :key #'country :test #'equal)))

(defun get-provinces (country)
  (map 'list #'province
       (remove-duplicates (remove country (country-towns)
                                  :key #'country :test-not #'equal)
                          :key #'province :test #'equal)))

(defun get-towns (country province)
  (map 'list #'town
       (remove-if-not (lambda (x)
                        (equal (country x) country)
                        (equal (province x) province))
                      (country-towns))))

(defmethod retrieve-values ((chain-select country-town-select) select values)
  
  (destructuring-bind (&optional country province) values
   (let ((position (position select (selects chain-select))))
     (case position
       (0 (get-all-countries))
       (1 (get-provinces country))
       (2 (get-towns country province))))))

(defclass job-title-select (chained-select)
  ()
  (:metaclass widget-class)
  (:default-initargs :select-names '(entity job-title)))


(defun get-all-entities ()
  (map 'list #'entity-name (remove-duplicates (entities)
                                          :key #'entity-name :test #'equal)))

(defun get-job-titles (entity)
  (map 'list #'entity
       (remove-if-not
        (lambda (doc)
          (if (get-val doc 'entity)
              (equal (xid (get-val doc 'entity)) entity)))
        (job-titles))))

(defmethod retrieve-values ((chain-select job-title-select) select values)
  (destructuring-bind (&optional entity) values
   (let ((position (position select (selects chain-select))))
     (case position
       (0 (get-all-entities))
       (1 (get-job-titles entity))))))

(defclass entity-select (chained-select)
  ()
  (:metaclass widget-class)
  (:default-initargs :select-names '(xid entity-name)))

(defun get-all-entitiesx ()
  (map 'list (lambda (doc)
               (if (not (string-equal (get-val doc 'doc-status) "superseded"))
                   (list (get-val doc 'xid) 
                         (get-val doc 'entity-name))))
       (entities)))


(defclass channel-user-select (chained-select)
  ()
  (:metaclass widget-class)
  (:default-initargs :select-names '(service channel-user)))



(defun get-channel-users (service)
  (loop for doc across (docs (channel-users-collection))
     when (and (match-context-entities doc)
               (string-equal (get-val doc 'channel-user-type) service)
               (get-val doc 'channel-user-name))
       
     collect (list (get-val doc 'user-id)
                   (get-val doc 'channel-user-name))))

(defmethod retrieve-values ((chain-select channel-user-select) select values)
  (destructuring-bind (&optional service) values
   (let ((position (position select (selects chain-select))))
;;(break "~A ~A ~A" service (get-channels-list) (get-channel-users "Twitter"))
     (case position
       (0 (get-channels-list))
       (1 (get-channel-users service))))))


(defun get-project-description (eid classification-type)
  (cond ((string-equal classification-type "HDSA In Management")
         (find-allsorts-for-select "HDSA Target"))
        ((string-equal classification-type "Career Progression Plans")
         (find-allsorts-for-select "Occupational Level"))
        ((string-equal classification-type "Workforce Ramp Up")
         (find-allsorts-for-select "Occupational Level"))
        ((string-equal classification-type "Preferential Procurement - HDSA")
         (find-allsorts-for-select "Procurement Service Rendered"))
        ((string-equal classification-type "Preferential Procurement - Local")
         (find-allsorts-for-select "Preferential Procurement - Local"))		  
        ((string-equal classification-type "Women In Mining")
         (find-allsorts-for-select "Women In Mining Target"))
        ((string-equal classification-type "Local Recruitment")
         (find-allsorts-for-select "Labour Sending Areas"))
        ((string-equal classification-type "Mentoring")
         (when (entity-eid-p eid)
           (get-mentoring-target-value-list (parse-integer eid))))
        ((string-equal classification-type "Employment Equity")
         (when (entity-eid-p eid) 
           (get-employment-equity-target-value-list (parse-integer eid))))
        ((find classification-type 
               '("ABET" "Learnerships" "Core Business Skills Training" "Portable Skills Training" "Bursaries" "Internships" "ABET Community" "Portable Skills Training (Non Mining)") 
               :test #'string-equal)
         (when (entity-eid-p eid)
           (course-name-value-list classification-type (parse-integer eid))))))

(defun entity-eid-p (eid)  
  (when  eid  
    (if (string-equal (subseq eid 0 1) "(") ;handle this crep entity (1 Agnes)
      nil
      T)))

(defclass target-description-select (chained-select)
  ((classification-type :initarg :classification-type))
  (:metaclass widget-class)
  (:default-initargs :select-names '(entity target-description)))

(defmethod retrieve-values ((chain-select target-description-select) select values)
  (destructuring-bind (&optional entity) values
   (let ((position (position select (selects chain-select))))
     (case position
       (0 (get-all-entitiesx))
       (1 (get-project-description entity (get-val chain-select 'classification-type)))))))
