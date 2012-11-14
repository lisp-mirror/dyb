(in-package :dyb)

(defvar *public-dir* "/var/www/dyb.co.za/public/")

(defparameter *extract-dir* "/var/www/dyb.co.za/extracts")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *indent-code* t))

(defvar *version* "0.1.00.05")

(defparameter *salt-length* 10)

(defun textually-equal-p (a b)
  (string-equal (princ-to-string a)
                (princ-to-string b)))

(defun current-user ()
  (if (boundp 'hunchentoot::*session*)
      (session-value 'user)))

(defun context-entities ()
  (if (stringp (session-value 'context-entities))
      (safe-read-from-string (session-value 'context-entities))
      (session-value 'context-entities)))

(defun context-reporting-period ()
  (session-value 'context-reporting-period))

(defun context-root-entity ()
  (session-value 'context-root-entity))

(defmacro with-html-to-string ((&key prologue (indent '*indent-code*)) &body body)
  `(with-html-output-to-string (*standard-output* nil :indent ,indent
                                                  :prologue ,prologue)
     ,@body))

(defmacro with-html (&body body)
  `(with-html-output (*standard-output* nil :indent *indent-code*)
     ,@body))

(defmacro with-html-string (&body body)
  `(with-html-output-to-string (*standard-output* nil :indent *indent-code*)
     ,@body))

(defun field-permission (data-element)
  (declare (ignore data-element))
  t)


(defun ensure-render (ensure body)
  (and ensure
       body))

(defun render-edit-field (name value
                          &key data-element required 
                          (type "text") 
                          data (blank-allowed t)
                          min
                          max
                          width)

  (ensure-render
   (field-permission (or data-element name))
   (with-html
     (case type
       (:span
        (htm (:input
              :style (if width
                         (format nil "width:~A;" width)
                         (format nil "width:~A;" "300px"))
              :type "text"
                     :name name
                     :disabled t
                     :readonly t
                     :value (escape value))))
       (:select
        (render-select (or data-element name) data value
                       :blank-allowed blank-allowed))
       (:textarea
        (htm (:textarea
              :style (if width
                         (format nil "width:~A;" width)
                         (format nil "width:~A;" "300px"))
              :class (if required "required")
              :name name
              :cols 85 :rows 5
                        (str (escape value)))))
       (:password
        (htm (:input
              :style (if width
                         (format nil "width:~A;" width)
                         (format nil "width:~A;" "300px"))
              :type "password"
              :class (if required "required")
              :name name
              :value (escape value))))
       (:date
        (htm (:input :type "text"
                     :style (if width
                                (format nil "width:~A;" width)
                                (format nil "width:~A;" "300px"))
                     :class "datepicker"
                     :name name
                     :value (escape value)))
        (defer-js "$('.datepicker').datepicker({dateFormat: 'dd M yy'})"))
       (t
        (htm
         (:div (:input :type type
                       :style (if width
                                  (format nil "width:~A;" width)
                                  (format nil "width:~A;" "300px"))
                       :class (if required "required")
                       :name name
                       :min min
                       :max max
                       :value (escape value)))
         (:div :style "display:none;"
               :name (format nil "validate-~A" name)
               :id (format nil "validate-~A" name)
               (:img :src "/appimg/q-icon.png"))))))))

(defun translate-possible-date-slot (slot-name value)
  (cond ((or (string-equal slot-name "start-date")
             (string-equal slot-name "end-date")
             (string-equal slot-name "date-of-engagement")
             (string-equal slot-name "date-of-termination")
             (string-equal slot-name "date-of-birth")
             (string-equal slot-name "lof-start-date")
             (string-equal slot-name "lof-end-date")
             (string-equal slot-name "feedback-date")
             (string-equal slot-name "meeting-date"))
         (string-to-date value))
        (t value)))

(defun synq-edit-data (object)
  (let ((parameters (append (get-parameters *request*)
                            (post-parameters *request*))))

    (when (typep object 'standard-object)

            (loop for (key . value) in parameters
               for slot = (find-slot key object)
               when slot
               do (update-slot object slot value)))))

(defun find-duplicate-doc-list (doc docs-list &key (element 'key))
  (dolist (dup docs-list)
    (if (equal (get-val doc element) (get-val dup element))
        (return-from find-duplicate-doc-list dup))))


(defun year (date)
  (values (decode-date date)))

(defun month (date)
  (nth-value 1 (decode-date date)))

(defun day (date)
  (nth-value 2 (decode-date date)))

(defun format-datex (date)
  (if (integerp date)
      (format-universal-date date)
      date))

(defun parse-date (date)
  (multiple-value-bind (year month date) (decode-date date)
    (when (and year month date)
      (encode-universal-time 0 0 0 date month year))))



(defparameter *time-zone* 0)

(defun period-date-to-universal (date-string)  
  (if date-string
      (let ((split-date (split-string date-string #\-)))        
        (encode-universal-time  
         0 0 0   
         (parse-integer (third split-date)) 
         (parse-integer (second split-date)) 
         (if (> (parse-integer (first split-date)) 1900) 
             (parse-integer (first split-date))
             1901)))
      date-string))

(defun current-date-time ()
  (multiple-value-call #'format-date-time (date-calc:today-and-now)))

(defun current-date ()
  (multiple-value-call #'format-date (date-calc:today)))

(defun integer-paremeter (name)
  (let ((parameter (parameter name)))
    (when (stringp parameter)
      (parse-integer parameter :junk-allowed t))))

(defun parse-trim-integer (string)
  (parse-integer (string-trim '(#\Space #\Tab #\Newline) string) :junk-allowed t))

(defun degrees-radians (degrees)
  (* degrees pi 1/180))

(defun radians-degrees (radians)
  (* (/ radians pi) 180))

(defun check-gps-coord (coord)
  (if (numberp coord)
      coord
      0))

(defun distance-between-points (lat1 lon1 lat2 lon2)
  (distance-between-points-rad
   (degrees-radians (check-gps-coord  lat1)) (degrees-radians (check-gps-coord lon1))
   (degrees-radians (check-gps-coord lat2)) (degrees-radians (check-gps-coord lon2))))

(defun distance-between-points-rad (lat1 lon1 lat2 lon2)
  (flet ((haversin (phi)
           (expt (sin (/ phi 2)) 2)))
    (let* ((earth-radius 6371)
           (a (+ (haversin (- lat2 lat1))
                 (* (haversin (- lon2 lon1))
                    (cos lat1)
                    (cos lat2))))
           (c (* 2 (atan (sqrt a)
                         (sqrt (- 1 a))))))
      (* earth-radius c))))

#|(defun distance-between-objects (object1 object2)
  (distance-between-points (latitude object1) (longitude object1)
                           (latitude object2) (longitude object2)))
|#

(defun render-allsort-select (sort name value)
  (let ((allsort (make-widget 'select
			      :name name
			      :items (find-allsorts-for-select sort))))
    (setf (value allsort) value)
    (render allsort)))

(defun render-select (name items value &key first-value blank-allowed)
  (let ((select (make-widget 'select
                             :name name)))
    (setf (blank-allowed select) blank-allowed)
    (setf (first-item select) first-value)
    (setf (items select) items)
    (when value
     (setf (value select) value))
    (render select)
    select))

(defun build-validation-array (validation-list)
  (if validation-list
      (json:encode-json-to-string
       (loop for (element type required) in validation-list
          collect `((element . ,element)
                    (type . ,type)
                    (required . ,(equalp required
                                         "required")))))))

#|(defun partition (predicate list)
  (loop for i in list
	when (funcall predicate i)
	collect i into part1
	else collect i into part2
	finally (return (values part1 part2))))
|#

(defun escape (x)
  (escape-string
   (typecase x
     (string x)
     (null nil)
     (t (princ-to-string x)))))

(defun princ-esc (x)
  (princ (escape x)))

(defun render-to-string (widget &rest args)
  (with-html-string
    (apply #'render widget args)))

(defun gps-cord-formatter (value)
  (format nil "~17$" value))



(defun decode-date-string (date)
  (ppcre:register-groups-bind
      (day month year) ("(\\d+)\\s+(\\w+)\\s+(\\d+)" date)
    (when day
      (values (parse-integer year)
              (month-number month)
              (parse-integer day)))))

(defun decode-time-string(time)
  (let ((time-match "(\\d{2}):(\\d{2}):(\\d{2})"))
    (if (cl-ppcre:scan time-match time )
        (cl-ppcre:register-groups-bind 
            ((#'parse-integer hour min sec)) 
            (time-match time)
          (values sec min hour)))))

(defun decode-time-string-check (time)
  (multiple-value-bind (second minute hour)
        (decode-time-string time)
    (cond ((> hour 24)
           (values nil "Hour can not be more than 24."))
          ((or (> minute 60) (> second 60))
           (values nil "Minutes and seconds can not be more than 60."))
          ((or  (< second 0) (< minute 0) (< hour 0))
           (values nil "No time value can be less than 0."))
          ((and (= hour 24) (or (> minute 0) (> second 0)))
           (values nil "Time can not be more than 24 hours."))
          (t (values second minute hour)))))



(defun decode-date (date)
  (etypecase date
    (simple-date:date
     (simple-date:decode-date date))
    (string
     (decode-date-string date))
    (integer
     (multiple-value-bind (a b c day month year)
         (decode-universal-time date)
       (declare (ignore a b c))
       (values year month day)))))

(defun make-query-string (alist)
  (with-output-to-string (query)
    (loop for (name . value) in alist
          for separator = "?" then "&amp;"
          do
          (format query "~a~a=~a"
                  separator
                  (url-encode name)
                  (url-encode (princ-to-string value))))))


(defmacro with-parameters (parameters &body body)
  "with-parameters ((variable-name \"parameter-name\") | variable-name) body"
  `(let ,(loop for parameter in parameters
               for (variable parameter-name) =
               (if (consp parameter)
                   parameter
                   (list parameter
                         (string-downcase parameter)))
               collect `(,variable (parameter ,parameter-name)))
     ,@body))


(defun make-icon (name &key (size 16)
                            (title ""))
  (let ((size (or size 16)))
    (with-html
      (:img :src (format nil "/img/icons/packs/fugue/~ax~a/~a.png"
                         size size name)
            :alt title
            :title title
            :width size :height size))))

(defun box-header (text &key icon (icon-size 16)
                             (class "header")
                             collapsible
                             body)
  (with-html
    (:div :class class
          (when icon
            (make-icon icon
                       :size icon-size))
          (:h3 (esc text))
          (when collapsible
            (htm (:div :class "collapse")))
          (str body))))

(defun js-link (&rest code)
  (fmt "javascript:{~{~a~^,~}}" code))

(defun sub-name (widget name)
  (fmt "~a-~a" (name widget) name))

(defun defer-js (code)
  (push code (getf *widget-parameters* :javascript-defer)))

(defun defer-js-function (code)
  (push code (getf *widget-parameters* :javascript-defer-function)))

(defun deferred-js ()
  (let ((defer (getf *widget-parameters* :javascript-defer))
        (function (getf *widget-parameters* :javascript-defer-function)))
    (when (or defer function)
      (format nil "$(document).ready(function(){~{~a;~}});~
~{~a~}"
              defer
              function))))

(defun check-vals (docs element)
  (dolist (doc (coerce docs 'list))
    (format t "~%~A" (get-val doc element))))

(defun print-entity-name (doc)
  (if doc
      (if (string-equal (get-val doc 'doc-type) "entity")
          (get-val doc 'entity-name)
          (if (get-val doc 'entity)
              (get-val (get-val doc 'entity) 'entity-name)))))

(defun print-entity-namex (doc)
  (format nil "~A" (xid doc)))

(defun print-entity-namexx (doc)
  (get-val doc 'doc-status))

(defun print-meeting (doc)   
  (if doc
      (if (string-equal (get-val doc 'doc-type) "project-meeting")
          (format nil "~A (~A)" 
                  (get-val doc 'description) 
                  (format-universal-date
                   (get-val doc 'meeting-date))))))

(defun print-attendance-xx-entity (doc)  
  ;(break "~A" doc)
  (if doc
      (get-val (get-val doc 'entity) 'entity-name)))

(defun print-address-type (doc)  
  ;(break "~A" doc)
  (if doc
      (get-val doc 'address-type)))

(defun print-countryx (doc)  
  ;(break "~A" doc)
  (if doc
      (get-val (get-val doc 'country-town) 'country)))

(defun print-provincex (doc)
;(break "~A" doc)
  (if doc
      (get-val (get-val doc 'country-town) 'province)))

(defun print-townx (doc)
;(break "~A" doc)
  (if doc
      (get-val (get-val doc 'country-town) 'town)))

(defun print-country-town-xid (doc)  
  (if doc
      (if (string-equal (get-val doc 'doc-type) "country-town")
          (get-val doc 'xid))))
(defun print-country (doc)  
  (if doc
      (if (string-equal (get-val doc 'doc-type) "country-town")
          (get-val doc 'country))))

(defun print-province (doc)
  (if doc
      (if (string-equal (get-val doc 'doc-type) "country-town")
          (get-val doc 'province))))

(defun print-town (doc)
  (if doc
      (if (string-equal (get-val doc 'doc-type) "country-town")
          (get-val doc 'town))))



(defun print-course-name (doc)
  (if doc
      (if (string-equal (get-val doc 'doc-type) "training-intervention")
          (get-val doc 'course-name)
           )))

(defun print-category (doc)
  (if doc
      (if (string-equal (get-val doc 'doc-type) "training-intervention")
          (get-val doc 'category)
           )))

(defun print-industry-number (doc)
  (if doc
      (if (string-equal (get-val doc 'doc-type) "biographical")
          (get-val doc 'employee-number)
           )))

(defun print-supplier-name (doc)
  (if doc
      (if (string-equal (get-val doc 'doc-type) "supplier")
          (get-val doc 'supplier-name)
          (if (get-val doc 'supplier)
              (get-val (get-val doc 'supplier) 'supplier-name)))))

(defun filter-active (objects)
  (remove-if-not #'activep objects))

(defun context ()
  (and (current-user)
       (multiple-value-bind (context found)
           (session-value 'context)
         (if found
             context
             (setf (context) (last-context (current-user)))))))

(defun (setf context) (entities)
  (and (current-user)
       (setf (session-value 'context) entities)))
