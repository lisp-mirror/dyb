(in-package :dyb)

(defclass input-widget (widget html-element)
  ((value :initarg :value 
          :initform nil
          :accessor value)
   (on-click :initarg :on-click
              :initform nil
              :accessor on-click
              :documentation
              "JS to run on click")
   (on-change :initarg :on-change
              :initform nil
              :accessor on-change
              :documentation
              "JS to run on change")
   (submit-on-change :initarg :submit-on-change
		     :initform nil
		     :accessor submit-on-change
		     :documentation
		     "id of the form to submit")
   (disabled :initarg :disabled
	     :initform nil
	     :accessor disabledp)))

(defmethod action-handler ((widget input-widget))
  (let ((parameter (parameter (name widget))))
    (when parameter
      (setf (value widget) parameter))))

(defun on-change-function (widget)
  (if (submit-on-change widget)
      (format nil "document.getElementById(\"~a\").submit();"
              (submit-on-change widget))
      (on-change widget)))

(defclass text (input-widget)
  ())

(defmethod render ((text text) &key)
  (with-html
    (:input :type "text"
            :name (name text)
            :id (name text)
            :class (css-class text)
            :style (style text)
            :value (escape (value text)))))

(defclass text-area (input-widget)
  ())

(defmethod render ((text-area text-area) &key)
  (with-html-output  (*standard-output* nil :indent nil)
    (:textarea
     :name (widgy-name text-area "value")
     (esc (value text-area)))))

(defclass checkbox (input-widget)
  ((description :initarg :description
                :initform nil
                :accessor description)))

(defmethod render ((checkbox checkbox) &key)
  (with-slots (on-change on-click) checkbox
    (with-html
      ;; When checkbox isn't checked, the browser doesn't send anything,
      ;; so we need an additional parameter which takes precedence
      ;; in this case.
      (:input :type "hidden"
             :name (widgy-name checkbox "value")
              :value (if (value checkbox)
                         "on" 
                         "off"))
      (:label :style (style checkbox)
       (:input :type "checkbox"
               :name (widgy-name checkbox "value")
               :class (css-class checkbox)
               :checked (when (value checkbox)
                          "checked")
               :onchange (when (or on-change on-click)
                          (format nil "~@{~@[~a~]~^;~}"
                                  on-click (on-change-function checkbox)))
               :disabled (when (disabledp checkbox)
                           "disabled"))
       (when (description checkbox)
         (esc (description checkbox)))))))

(defmethod action-handler ((checkbox checkbox))
  (let ((value (value checkbox)))
    (when (stringp value)
      (setf (value checkbox)
            (or (string= value "on") (string= value "true") )))))

(defclass checkbox-list (input-widget)
  ((items :initarg :items
          :initform ()
          :accessor items)
   (checkboxes :initform ()
               :accessor checkboxes)
   (orientation :initarg :orientation
		:initform :vertical
		:accessor orientation)
   (check-all :initarg :check-all
              :initform t
              :accessor check-all))
  (:metaclass widget-class))

(defun sub-name (widget name)
  (format nil "~a-~a" (name widget) name))

(defun %create-checkboxes (checkbox-list
                           items
                           &key change
                                (parent 0)
                                (level 0))
  (loop for (nil description selected . sub) in items
        for i from 0
        for checkbox = (make-widget 'checkbox
                                    :name (sub-name checkbox-list
                                                    (fmt "~a-~a-~a"
                                                         parent level i))
                                    :description description
                                    :value selected
                                    :on-change (on-change checkbox-list)
                                    :submit-on-change
                                    (submit-on-change checkbox-list)
                                    :on-click (when (check-all checkbox-list)
                                                "updateCheckAll(this)")
                                    :style (and (plusp level)
                                                (fmt "margin-left: ~apx;" (* 10 level))))
        when change
        do (setf (value checkbox) selected
                 (description checkbox) description)
        do (setf (style checkbox)
                 (and (plusp level)
                      (fmt "padding-left: ~apx;" (* 10 level))))
        collect (list* checkbox
                       (%create-checkboxes checkbox-list
                                           sub
                                           :change change
                                           :parent (1+ i)
                                           :level (1+ level)))))

(defun create-checkboxes (checkbox-list
                          &key change)
  (prog1
      (setf
       (checkboxes checkbox-list)
       (%create-checkboxes checkbox-list (items checkbox-list)
                           :change change))
    (setf (value checkbox-list)
	  (checkbox-list-value checkbox-list))))

(defun update-checkbox-list-items (checkbox-list)
  (labels ((%update (checkboxes items)
             (loop for (checkbox . check-sub) in checkboxes
                   for (object text nil . items-sub) in items
                   collect (list* object text (value checkbox)
                                  (%update check-sub items-sub)))))
    (setf (items checkbox-list)
          (%update (checkboxes checkbox-list)
                   (items checkbox-list)))))

(defun checkbox-list-value (checkbox-list)
  (labels ((%value (checkboxes items)
             (loop for (checkbox . check-sub) in checkboxes
                   for (object text value . items-sub) in items
                   when value
                   collect (if check-sub
                               (list* object (%value check-sub items-sub))
                               object))))
    (%value (checkboxes checkbox-list)
            (items checkbox-list))))

(defmethod (setf items) :after (items (object checkbox-list))
  (create-checkboxes object :change t))

(defun make-check-all-button (checkbox-list)
  (when (check-all checkbox-list)
    (let ((box (make-widget 'checkbox
                            :description "Check All"
                            :name (sub-name checkbox-list "all")
                            :on-click (fmt "checkAll(this)" ())
                            :on-change (on-change checkbox-list)
                            :submit-on-change
                            (submit-on-change checkbox-list))))
      (setf (value box)
            (every #'third (items checkbox-list)))
      (with-html
        (render box)
        (when (eq (orientation checkbox-list) :vertical)
          (htm (:br)))))))

(defun lay-checkbox-list (checkbox-list
                          checkboxes
                          &key (level 0))
  (with-html
    (loop for (checkbox . sub) in checkboxes
          do
          (render checkbox)
          (lay-checkbox-list checkbox-list sub
                             :level (1+ level)))))

(defmethod render ((checkbox-list checkbox-list) &key)
  (with-html
    (:div
     :class (ecase (orientation checkbox-list)
              (:vertical "cb-list")
              (:horizontal "horizontal-cb-list"))
     :id (name checkbox-list)
     (make-check-all-button checkbox-list)
     (lay-checkbox-list checkbox-list
                        (create-checkboxes checkbox-list)))))

(defmethod action-handler ((checkbox-list checkbox-list))
  (update-checkbox-list-items checkbox-list)
  (setf (value checkbox-list)
	(checkbox-list-value checkbox-list)))


(defclass select (input-widget)
  ((items :initarg :items
          :initform ()
          :accessor items)
   (first-item :initarg :first-item
               :initform nil
               :accessor first-item)
   (blank-allowed :initarg :blank-allowed
                  :initform t
                  :accessor blank-allowed)))

(defun item-description (item)
  (if (consp item)
      (second item)
      item))

(defun item-value (item)
  (if (consp item)
      (car item)
      item))

(defun display-select-warning (select)
  (unless (or (invalid-value-p (value select))
              (not (items select)))
    (with-html
      (:img :src "/appimgvfr-red-icon.png" 
            :alt ""
            :title (format nil "Illegal value \"~A\"! ~
Please select a legal one"
                           (value select))))))

(defun item-equal (item value)
  (or (textually-equal-p (item-value item)
                         value)
      (textually-equal-p (item-description item)
                         value)))

(defmethod render ((select select) &key)
  (let ((current-value (value select))
        (first-item (first-item select))
        selected-item)
    (with-html
      (:select
       :name (name select)
       :id (name select)
       :class (css-class select)
       :style (style select)
       :onchange (on-change-function select)
       (when first-item
         (htm (:option :selected
                       (and (item-equal first-item current-value)
                            (setf selected-item t))
                       (esc first-item))))
       (when (and (blank-allowed select)
                  (invalid-value-p current-value))
         (htm         
          (:option 
           :style "background-color:pink;"
           :selected t)))
       (loop for item in (items select)
             do
             (htm (:option :selected
                           (and (item-equal item current-value)
                                (setf selected-item t))
                           :value (item-value item)
                           (princ-esc (item-description item)))))
       (when (and (not selected-item)
                  (not (invalid-value-p current-value)))
         (htm         
          (:option 
           :style "background-color:pink;"
           :selected t
           :value (escape current-value)
           (princ-esc current-value)))))
      (unless selected-item
        (display-select-warning select)))))
;;;

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
      (defer-js (fmt "$('#~a select').trigger('change')" (name widget)))
      (defer-js (fmt "$('#~a select').change(function(){~a;})"
                     (name widget)
                     (js-render next-select (fmt "[~s, $('#~a select').val()]"
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
       when (match-context-entities doc)
     when (if (string-equal (get-val doc 'channel-user-type) service)              
              (if (get-val doc 'channel-user-name)
                  (get-val doc 'channel-user-name)
                  ))
     collect (list (get-val doc 'user-id)
                   (get-val doc 'channel-user-name)))

  )

(defmethod retrieve-values ((chain-select channel-user-select) select values)
  (destructuring-bind (&optional service) values
   (let ((position (position select (selects chain-select))))
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
