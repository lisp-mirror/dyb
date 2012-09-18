(in-package :ems)

(defvar *public-dir* "/var/www/ems.co.za/public/")

(defparameter *extract-dir* "/var/www/ems.co.za/extracts")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *indent-code* t))

(defvar *version* "0.2.00.06")

(defparameter *salt-length* 10)

(defparameter *count-year* 9999)

(defun find-dfs (x tree &key (test #'eql) key)
  (let ((key (or key #'identity)))
    (labels ((do-find (x tree)
               (loop for i in tree
                     when (funcall test x (funcall key i))
                     do (return-from find-dfs i)
                     when (consp i)
                     do (do-find x i))))
      (do-find x tree))))

(defun fmt (control-string &rest args)
  (apply #'format nil control-string args))

(defun format-float (n)
  (fmt "~,2f" n))

(defun current-user ()
  (if (boundp 'hunchentoot::*session*)
      (session-value 'user)))

(defun %intern (value)
  (intern (format nil "~:@(~a~)" value)
          #.*package*))

(defun intern-key (value)
  (intern (format nil "~:@(~a~)" value) "KEYWORD"))

(defmethod get-val ((doc standard-object) element &key data-type)
  (declare (ignore data-type))
  (when doc
    (slot-val doc (if (stringp element)
                        (%intern element)
                        element))))



(defmethod (setf get-val) (new-value (doc standard-object) element &key data-type)
  (declare (ignore data-type))
  (setf (slot-value doc
                    (if (stringp element)
                        (%intern element)
                        element))
        new-value))



(defun nbl-p (value)
  (and value
       (not (equal value ""))))

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

(defun slot-val (instance slot-name)
  (if (and instance
           (slot-boundp instance slot-name))
      (slot-value instance slot-name)))

(defun format-money (value &key (include-comma t))
  (typecase value
    (null "")
    (number
     (multiple-value-bind (quot rem) (truncate value)
       (format nil "~@?~0,2f"
               (if include-comma "~:d" "~d")
               quot rem)))
    (t
     (princ-to-string value))))

(defun render-edit-field (name value
                          &key data-element required 
                          (type "text") 
                          data (blank-allowed t)
                          min
                          max)

  (ensure-render
   (field-permission (or data-element name))
   (with-html
     (case type
       (:span
        (htm (:input :type "text"
                     :name name
                     :disabled t
                     :readonly t
                     :value (escape value))))
       (:select
        (render-select (or data-element name) data value
                       :blank-allowed blank-allowed))
       (:textarea
        (htm (:textarea :class (if required "required")
                        :name name
                        :cols 85 :rows 5
                        (str (escape value)))))
       (:password
        (htm (:input :type "password"
                     :class (if required "required")
                     :name name
                     :value (escape value))))
       (:date
        (htm (:input :type "text"
                     :class (if required
                                "date-pick required"
                                "date-pick")
                     :name name
                     :value (escape value))))
       (t
        (htm
         (:div (:input :type type
                       :class (if required "required")
                       :name name
                       :min min
                       :max max
                       :value (escape value)))
         (:div :style "display:none;"
               :name (format nil "validate-~A" name)
               :id (format nil "validate-~A" name)
               (:img :src "/appimgq-icon.png"))))))))


(defun find-direct-slot-definition (class slot-name)
  (labels ((find-slot (class)
             (or (find slot-name (class-direct-slots class)
                       :key #'slot-definition-name)
                 (some #'find-slot (class-direct-superclasses class)))))
    (find-slot class)))

(defun find-slot-writer (class slot-name)
  (let ((slot (find-direct-slot-definition class slot-name)))
    (and slot
         (car (slot-definition-writers slot)))))

(defun get-slot-setf-method (object slot-name)
  (let ((writer (find-slot-writer (class-of object)
                                  slot-name)))
    (when writer
      (fdefinition writer))))

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
        (t value)
        ))

(defun update-slot (instance slot-name value)
  (let ((method (get-slot-setf-method instance slot-name))
        (val (translate-possible-date-slot slot-name value)))
    (if method
        (funcall method val instance)
        (setf (slot-value instance slot-name) val))))

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

(defun update-master-child-doc (master-doc doc master-slot &key key (element 'key))
  (let ((old-master-doc (copy-new master-doc))
        (old-doc (copy-new doc)))
    (synq-edit-data doc)
    (if (slot-exists-p doc 'key)
        (setf (key doc) key))
    (setf (get-val master-doc master-slot)
          (append (get-val master-doc master-slot) (list doc)))
    (if old-doc
        (update-doc doc old-doc)
        (persist doc))
    (if old-master-doc
        (update-doc master-doc old-master-doc)
        (persist master-doc))))

(defun persist-from-grid-edit (doc key)
  (let ((old-doc (copy doc)))
    (synq-edit-data doc)
    (setf (key doc) key)

    (if (get-val doc 'xid)
            (persist doc)
            (persist doc :old-object old-doc))

    (persist doc)
    ))

(defun persist-from-child-grid-edit (doc key)
  (let ((new-doc (copy doc)))
    (setf (key new-doc) key)
    (persist new-doc)
    ))

(defmacro render-row-edit (form-name object &key validation-list save-disabled-p body  )
  `(let* ((form
           (make-widget 'grid-edit-form
                        :name ,(format nil "~A-edit-form" form-name))))
     (setf (data form) (list ,object))
     (setf (edit-form grid) form)
     (setf (save-disabled-p form) ,save-disabled-p)

     (setf (validation-list form) ,validation-list)


     (with-html
       (:div :class "grid_6"
             (:div :class "box"
                   (:div :class "header"
                         (:img :src "img/icons/packs/fugue/16x16/application-form.png" :alt "" :width "16" :height "16")
                         (:h3 "")
                         (:span)
                         )

                   (render form
                                :body
                                (with-html-to-string ()
                                  (str
                                   ,body))))))
     ))

(defun format-coord (value)
  (format nil "~17$" (if value
                         value
                         0)))

(defvar *short-months*
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defvar *long-months*
  #("January" "February" "March" "April" "May" "June" "July" "August" "September"
    "October" "November" "December"))

 (defun short-month-name (n)
  (when (array-in-bounds-p *short-months* (1- n))
    (aref *short-months* (1- n))))

(defun long-month-name (n)
  (when (array-in-bounds-p *long-months* (1- n))
    (aref *long-months* (1- n))))

(defun format-universal-date (universal-date)  
  (if (stringp universal-date)
      universal-date
      (multiple-value-bind (a b c day month year)
          (decode-universal-time (or universal-date (get-universal-time)))
        (declare (ignore a b c))
        (build-date year month day))))

(defun build-date (year month day)
  (format nil "~d ~a ~d"  day (short-month-name month) year))

(defun build-date-time (year month day hour min sec
                        &optional timezone)
  (declare (ignore timezone))
  (format nil "~d ~a ~d ~@{~2,'0d~^:~}"
          day (short-month-name month) year hour min sec))

(defun year (date)
  (values (decode-date date)))

(defun month (date)
  (nth-value 1 (decode-date date)))

(defun day (date)
  (nth-value 2 (decode-date date)))

(defun format-date (date)
  (if (integerp date)
      (format-universal-date date)
      date))

(defun parse-date (date)
  (multiple-value-bind (year month date) (decode-date date)
    (when (and year month date)
      (encode-universal-time 0 0 0 date month year))))

(defun split-string (string char)
    "Returns a list of substrings of string
divided by char."
    (loop for i = 0 then (1+ j)
          as j = (position char string :start i)
          collect (subseq string i j)
          while j))

(defparameter *time-zone* 0)

(defun string-to-date (date-string)
  (if (stringp date-string)
      (let ((split-date (split-string date-string #\space)))
        (encode-universal-time  
         0 0 0 
         (parse-integer (first split-date)) 
         (+ 1 (or 
               (position (second split-date) *short-months* :test 'string-equal)
               (position (second split-date) *long-months* :test 'string-equal))) 
         (if (> (parse-integer (third split-date)) 1900) 
             (parse-integer (third split-date))
             1901)
         ))
      date-string))

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

(defun date-to-universal (date-string)  
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

(defun format-short-date (date)
  (let ((split-date (split-string (format-date date) #\space)))
    (format nil "~a ~a ~a"
            (first split-date)
            (string-capitalize (subseq (second split-date) 0 3))
            (third split-date))))

(defun format-time-stamp (stamp)
  (if (typep stamp 'simple-date:timestamp)
      (multiple-value-call #'build-date-time
        (simple-date:decode-timestamp stamp))
      stamp))

(defun previous-day ()
  (multiple-value-call #'build-date (date-calc:yesterday)))

(defun current-date-time ()
  (multiple-value-call #'build-date-time (date-calc:today-and-now)))

(defun current-date ()
  (multiple-value-call #'build-date (date-calc:today)))

(defun integer-paremeter (name)
  (let ((parameter (parameter name)))
    (when (stringp parameter)
      (parse-integer parameter :junk-allowed t))))

(defun ensure-parse-integer (x)
  (typecase x
    (string
     (multiple-value-bind (integer position)
         (parse-integer x :junk-allowed t)
       (when (= (length x) position)
         integer)))
    (integer x)))

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

(defun distance-between-objects (object1 object2)
  (distance-between-points (latitude object1) (longitude object1)
                           (latitude object2) (longitude object2)))

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

(defun partition (predicate list)
  (loop for i in list
	when (funcall predicate i)
	collect i into part1
	else collect i into part2
	finally (return (values part1 part2))))

(defun escape (x)
  (escape-string
   (typecase x
     (string x)
     (null nil)
     (t (princ-to-string x)))))

(defun princ-esc (x)
  (princ (escape x)))

(defun strip-leading-zeros (string)
  (if (every #'digit-char-p string)
      (if (position #\0 string :test #'char/=)
          (subseq string (position #\0 string :test #'char/=)))
      string))

(defun render-to-string (widget &rest args)
  (with-html-string
    (apply #'render widget args)))

(defun gps-cord-formatter (value)
  (format nil "~17$" value))

(defun month-number (month)
  (let ((position (or (position month *short-months*
                                :test #'equalp)
                      (position month *long-months*
                                :test #'equalp))))
    (when position
      (1+ position))))

(defun decode-date-string (date)
  (ppcre:register-groups-bind
      (day month year) ("(\\d+)\\s+(\\w+)\\s+(\\d+)" date)
    (when day
      (values (parse-integer year)
              (month-number month)
              (parse-integer day)))))

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

(defun days-between-dates (date1 date2)
  (multiple-value-call #'date-calc:delta-days
    (decode-date date1)
    (decode-date date2)))

(defun convert-days-to-years (start-date end-date)
  (/ (days-between-dates start-date end-date) 365))

(defun ensure-render (ensure body)
  (and ensure
       body))

(defun field-permission (data-element)
  (declare (ignore data-element))
  t)


(defun ffr (field alist-row &optional retain-case)
 (cdr
  (assoc
   (if (not (symbolp field))
       (intern (if retain-case
		   field
		   (string-upcase field)) "KEYWORD")
       field)
   alist-row :test #'string-equal)))

(defun safe-read-from-string (string)
  (let (*read-eval*)
    (ignore-errors (read-from-string string))))

(defun numeric-string-p (string)
  (numberp (safe-read-from-string string)))

(defun make-query-string (alist)
  (with-output-to-string (query)
    (loop for (name . value) in alist
          for separator = "?" then "&amp;"
          do
          (format query "~a~a=~a"
                  separator
                  (url-encode name)
                  (url-encode (princ-to-string value))))))

(defun textually-equal-p (a b)
  (string-equal (princ-to-string a)
                (princ-to-string b)))

(defun invalid-value-p (value)
  (or (equal value :null)
      (string-equal (format nil "~A" nil)
                    (if (stringp value)
                        value
                        (format nil "~A" value)))
      (not (nbl-p value))))


;;; Is this really what is intended?
;;; it can be rewrritten as
;;; (or (string-not-equal (slot-val link 'alt) "delete")
;;;     (string-equal (slot-val row-object 'reporting-period-status-desc) "Open"))
;;; That means that the first condition doesn't matter,
;;; since it will return T by the default clause.
(defun edit-delete-check (link row-object)
  (cond ((string-equal (slot-val link 'alt) "edit")
         t)
        ((string-equal (slot-val link 'alt) "delete")
         (if (string-equal (slot-val row-object 'record-status) "Active")
             t))
        (t
         t)))

(defun deep-copy-dao-effective (dao)
  (let ((new (make-instance
              (class-name (class-of dao)))))
    (dolist (slot (sb-mop::class-slots (class-of dao)))
      (setf (slot-value new (slot-definition-name slot))
            (slot-value dao (slot-definition-name slot))))
    new))



(defun effective-date-validation (object)
  (< (days-between-dates (slot-value object 'effective-std)
                         (slot-value object 'effective-end)) 0))




(defun format-money-return-value (id value)
  (if (or  (nbl-p id)
           (nbl-p value))
      (format-money value
                    :include-comma nil)
      0.0))

#|
(defun entities-names (entities-ids)
  (query (:select 'id 'entity-value
                  :from 'entities
                  :where (:and
                          (:in 'id (:set entities-ids))
                          (:ilike 'record-status  "Active")))))

|#

(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos)))

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

(defun remove-whitespace (string)
  (string-trim '(#\Space #\Newline #\Tab #\Return) string))

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
  (format nil "$(document).ready(function(){~{~a;~}});~
~{~a~}"
          (getf *widget-parameters* :javascript-defer)
          (getf *widget-parameters* :javascript-defer-function )))

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

(defun copy-new (doc)
  (and (slot-val doc 'xid)
       (copy doc)))

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
