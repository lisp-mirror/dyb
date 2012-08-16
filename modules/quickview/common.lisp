(in-package :ems-quickview)

(defvar *quarter-colors*
  #("F9F9FF" "E9E9FF" "D9D9FF" "C9C9FF"))

(defvar *reporting-period* nil)

(defun add-months (universal months)
  (local-time:timestamp-to-universal
   (local-time:timestamp+ (local-time:universal-to-timestamp universal)
                          months :month)))

(defun year-difference (universal1 universal2)
  (local-time:timestamp-whole-year-difference
   (local-time:universal-to-timestamp universal1)
   (local-time:universal-to-timestamp universal2)))

(defun period-quarters (&key inclusive)
  (loop for i from 1 to 4
        for start = (start-date *reporting-period*) then end
        for end = (add-months start 3)
        for color across *quarter-colors*
        for name = (format nil "Q~a" i)
        collect (cons (make-instance 'reporting-period
                                     :name name
                                     :start-date (if inclusive
                                                     (start-date *reporting-period*)
                                                     start)
                                     :end-date end)
                      (make-header name
                                   :color color
                                   :sort i))))

(defun filter-quarters (doc quarters)
  (with-slots (date-of-engagement date-of-termination) doc
    (loop for (quarter . header) in quarters
          when (and (<= date-of-engagement (end-date quarter))
                    (>= date-of-termination (start-date quarter)))
          collect header)))

(defun filter-quarters-start-end (doc quarters)
  (with-slots (start-date end-date) doc
    (loop for (quarter . header) in quarters
          when (and (<= start-date (end-date quarter))
                    (>= end-date (start-date quarter)))
          collect header)))

(defun assoc-value (key alist &optional default)
  (let ((cons (assoc key alist :test #'equal)))
    (cond ((not cons)
           default)
          ((consp (cdr cons))
           (cadr cons))
          (t
           (cdr cons)))))

(defun remove-plist-duplicates (plist)
  (let (seen)
    (loop for (key value) on plist by #'cddr
          unless (member key seen)
          collect key
          and collect value
          and do (push key seen))))

(defun leave-in-plist (plist keys)
  (loop for (key value) on plist by #'cddr
        when (member key keys)
        collect key
        and collect value))

(defun format-symbol (symbol)
  (ppcre:regex-replace-all #\- (string-capitalize symbol)
                           " "))

(defun patersonify (level)
  (if (and (stringp level)
           (= (length level) 1)
           (find (char level 0) "ABCDEF" :test #'char-equal))
      (fmt "Paterson Level ~:@(~a~)" level)
      level))

(defun find-address (object type)
  (find-if (lambda (x)
             (equal (slot-value x 'address-type) type))
           (slot-value object 'addresses)))

(defun physical-address (object)
  (find-address object "Physical Address"))

(defun postal-address (object)
  (find-address object "Postal Address"))

(defun check-entity (doc &optional (entities *quickview-entities*))
  (if (slot-exists-p doc 'entity)
      (let* ((entity (slot-value doc 'entity))
             (entity-id (and entity
                             (xid entity))))
        (member entity-id entities))
      t))

(defun filter-entities (objects)
  (remove-if-not #'check-entity objects))

(defun find-country-town (country province town)
  (loop for doc across (country-towns)
        when (and (activep doc)
                  (equal (town doc) town)
                  (equal (province doc) province)
                  (equal (country doc) country))
        return doc))

(defun bogus-result-p (x)
  (member x '(nil "false" :null "" "n/a"
              "NIL")
          :test #'equalp))

(defun mine-name (mine)
  (entity-name (entity mine)))

(defvar *number-coma* nil)

(defun format-number (value)
  (typecase value
    (null "")
    (number
     (cond (*number-coma*
            (ems::format-money value))
           ((integerp value)
            (format nil "~a" value))
           (t
            (format nil "~,2f" (float value)))))
    (string
     value)
    (t
     (princ-to-string value))))

(defun period-version (doc &optional (period *reporting-period*))
  (cond ((not (typep doc 'doc))
         doc)
        ((or ;; (null (stamp-date doc))
          (null period)
          (and (<= (stamp-date doc) (end-date period))
               (null (effective-date doc))))
         doc)
        (t
         (let ((start-date (start-date period))
               (end-date (end-date period))
               latest-date
               latest-doc)
           (flet ((check-version (doc)
                    (with-slots (effective-date stamp-date) doc
                      (when (and (<= stamp-date end-date)
                                 (>= effective-date start-date)
                                 (or (null latest-date)
                                     (> effective-date latest-date)))
                        (setf latest-date effective-date
                              latest-doc doc)))))
             (check-version doc)
             (mapc #'check-version (old-versions doc))           
             latest-doc)))))

(defun town-local-municipalities (town &optional period)
  (loop for municip in (municipalities town)
        for local = (local-municipality (period-version municip period))
        unless (bogus-result-p local) 
        collect local))

(defun filter-period (objects)
  (loop for object being the elements of objects
        when (period-version object)
        collect it))

(defun project-entity-data-value (project-entity key)
  (loop for data in (data project-entity)
        when (equalp (data-name data) key)
        return (data-value data)))

(defun project-entity-children (type project-entity &key (check-entity t))
  (let ((children (children project-entity)))
    (loop for child-doc in children
          for child = (period-version child-doc)
          when (and child
                    (or (not check-entity)
                        (check-entity child))
                    (equal (project-entity-type child) type))
          collect child)))

(defun project-entity-targets (project-entity)
  (let ((children (children project-entity)))
    (loop for child-doc in children
          for child = (period-version child-doc)
          when (and child
                    (check-entity child)
                    (equal (project-entity-type child) "Undertaking Target"))
          collect (project-entity-data-value child "Target"))))

(defun project-entity-all-targets (project-entity)
  (let ((children (children project-entity)))
    (loop for child-doc in children
          for child = (period-version child-doc)
          when (and child
                    (check-entity child)
                    (equal (project-entity-type child) "Undertaking Target"))
          collect (list (project-entity-data-value child "Target")
                        (description child)))))

(defun get-targets (category)
  (loop for entity-doc being the elements of (project-entities)
        for entity = (period-version entity-doc)
        when (and entity
                  (check-entity entity)
                  (equal category (project-entity-data-value entity "Category")))
        append (project-entity-all-targets entity)))

(defun split-target (target)
  (ppcre:split " *- *" target))

(defun calc-percent (whole part)
  (if (plusp whole)
      (* (/ part whole) 100)
      0))

(defun count-docs (test docs)
  (loop for latest-doc being the elements of docs
        for doc = (period-version latest-doc)
        count (and doc
                   (check-entity doc)
                   (funcall test doc))))
