(in-package :ems-quickview)

(defclass quickview-form (quickview-widget)
  ((drill-down-page :initarg :drill-down-page
                    :initform "/ems/biographical"
                    :accessor drill-down-page)))

(defclass form-table ()
  ((vertical :initarg :vertical
             :initform nil
             :accessor vertical)
   (horizontal :initarg :horizontal
               :initform nil
               :accessor horizontal)
   (rows :initarg :rows
         :initform nil
         :accessor rows)
   (columns :initarg :columns
            :initform nil
            :accessor columns)))

(defclass table-cell ()
  ((text :initarg :text
         :initform nil
         :accessor text)
   (row-span :initarg :row-span
             :initform 1
             :accessor row-span)
   (col-span :initarg :col-span
             :initform 1
             :accessor col-span)
   (bg-color :initarg :bg-color
             :initform nil
             :accessor bg-color)
   (vertical-text :initarg :vertical-text
                  :initform nil
                  :accessor vertical-text)))

(defclass cell-path ()
  ((iterator :initarg :iterator
             :initform nil
             :accessor iterator)
   (tests :initarg :tests
          :initform nil
          :accessor tests)
   (results :initarg :results
            :initform nil
            :accessor results)))

(defmethod print-object ((header table-cell) stream)
  (print-unreadable-object (header stream :type t :identity t)
    (format stream "~@[~a~]" (text header))))

(defclass form-header (table-cell)
  ((children :initarg :children
             :initform ()
             :accessor children)
   (index :initarg :index
          :initform nil
          :accessor index)
   (sort-order :initarg :sort-order
               :initform nil
               :accessor sort-order)
   (parent :initarg :parent
           :initform nil
           :accessor parent)))

(defun make-header (text &key sort color)
  (make-instance 'form-header :text text
                              :sort-order sort
                              :bg-color color))

(defclass total-header (form-header)
  ())

(defclass target-header (form-header)
  ())

(defclass target-percent-header (form-header)
  ((target :initarg :target
           :initform 0
           :accessor target)
   (actual :initarg :actual
           :initform 0
           :accessor actual)))

(defstruct container
  children)

(defmethod children ((container container))
  (container-children container))

(defmethod (setf children) (value (container container))
  (setf (container-children container) value))

(defun headers-depth (rows)
  (typecase rows
    (form-header
     (headers-depth (children rows)))
    (t
     (loop for header in rows
           maximize (1+ (headers-depth header))))))

(defun headers-width (headers)
  (typecase headers
    (string
     0)
    (t
     (loop for header in headers
           maximize (1+ (headers-width header))))))

(defun adjust-col-spans (rows)
  (if (listp rows)
      (mapc #'adjust-col-spans rows)
      (setf (col-span rows)
            (if (children rows)
                (reduce #'+ (adjust-col-spans (children rows)) :key #'col-span)
                1))))

(defun adjust-row-spans (columns)
  (if (listp columns)
      (mapc #'adjust-row-spans columns)
      (setf (row-span columns)
            (if (terminal-header-p columns)
                1
                (reduce #'+ (adjust-row-spans (children columns)) :key #'row-span)))))

(defun adjust-vertical-row-spans (verticals)
  (let ((max-depth (headers-depth verticals)))
    (labels ((recurse (row level)
               (cond ((not (terminal-header-p row))
                      (loop for row in (children row)
                            do (recurse row (1+ level))))
                     ((< level max-depth)
                      (setf (row-span row) (- max-depth level))))))
      (loop for row in verticals
            do (recurse row 0)))))

(defun assign-indexes (rows)
  (let ((index -1))
    (labels ((recurse (header)
               (cond ((children header)
                      (setf (index header) (1+ index))
                      (map nil #'recurse (children header)))
                     (t
                      (setf (index header) (incf index))))))
      (map nil #'recurse rows))))

(defun test-path (object path)
  (let ((results (results path)))
    (flet ((sub-test (function)
             (loop for value = (setf (values value function) (funcall function object))
                   for result = (pop results)
                   always (typecase value
                            (cons
                             (member result value :test #'equal))
                            (t (equal result value)))
                   while function)))
      (destructuring-bind (vertical . horizontal) (tests path)
        (and (sub-test vertical)
             (or (not horizontal)
                 (sub-test horizontal)))))))

(defmethod quickview-details ((quickview quickview-form) path)
  (let (result)
    (funcall (iterator path)
             (lambda (doc)
               (when (test-path doc path)
                 (push doc result))))
    result))

;;;

(defun calculate-totals (table)
  (with-slots (rows horizontal) table
    (let* ((width (reduce #'+ horizontal :key #'col-span))
           (result (make-array width :initial-element 0)))
      (loop for row in rows
            do
            (loop for (header value) in (children row)
                  do (typecase header
                       (target-percent-header
                        (unless (plusp (aref result (index header)))
                          (setf (aref result (index header))
                                (calc-percent (target header)
                                              (actual header)))))
                       (t
                        (incf (aref result (index header))
                              (if (numberp value)
                                  value
                                  0))))))
      result)))

(defvar *totals-colors*
  '("ffffd6"
    "ccffff"
    "ccccff"
    "ccccff"
    "ccccff"))

(defun sum-totals (vertical total-header target-header start end
                   &key target
                        (add-total t))
  (let (has-targets)
    (loop for row in vertical
          for target-value = 0
          for sum = 0
          do
          (loop for (header value) in (children row)
                do
                (cond ((not (numberp value)))
                      ((or (typep total-header 'target-header)
                           (not (typep header 'target-header)))
                       (when (<= start (index header) end)
                         (incf sum value)))
                      ((or (eq target t)
                           (equal (text header)
                                  target))
                       (incf target-value value))))
          (when add-total
           (push (list total-header (or sum 0))
                 (children row)))
          (incf (actual target-header) sum)
          (when (plusp target-value)
            (incf (target target-header) target-value)
            (push (list target-header (calc-percent target-value sum))
                  (children row))
            (setf has-targets t)))
    has-targets))

(defun insert-after (item after list)
  (let* ((list (copy-list list))
         (after (member after list)))
    (setf (cdr after)
          (cons item (cdr after)))
    list))

(defun add-totals (table &key (add-grand-total t))
  (with-slots (rows horizontal) table
    (let ((level 0))
      (labels ((add (h-rows)
                 (incf level)
                 (cond ((listp h-rows)
                        (map nil #'add h-rows))
                       (t
                        (let* ((add-total (> (col-span h-rows) 1))
                               (depth (headers-depth h-rows))
                               (header (and add-total
                                            (make-instance (if (typep h-rows 'target-header)
                                                               'target-header
                                                               'total-header)
                                                           :text "Total"
                                                           :bg-color (elt *totals-colors* level)
                                                           :row-span depth
                                                           :index -1)))
                               (percent (make-instance 'target-percent-header
                                                       :text "%"
                                                       :bg-color *target-color*
                                                       :row-span (max depth 1)
                                                       :index -1))
                               (has-targets (sum-totals rows header percent
                                                        (index h-rows)
                                                        (+ (index h-rows) (col-span h-rows) -1)
                                                        :add-total add-total
                                                        :target (and (not (typep h-rows 'target-header))
                                                                     (text h-rows))))
                               (parent (parent h-rows)))
                          (add (children h-rows))
                          (cond (add-total
                                 (setf (children h-rows)
                                       (append (children h-rows)
                                               (and add-total
                                                    (list header))
                                               (and has-targets
                                                    (list percent)))))
                                (has-targets
                                 (setf (row-span percent)
                                       (row-span h-rows))
                                 (setf (children parent)
                                       (insert-after percent h-rows
                                                     (children parent))))))))
                 (decf level)))
        (if add-grand-total
            (let* ((depth (headers-depth horizontal))
                   (grand-total (make-instance 'total-header
                                               :text "Total"
                                               :row-span depth
                                               :index -1))
                   (percent (make-instance 'target-percent-header
                                           :text "%"
                                           :bg-color *target-color*
                                           :row-span depth
                                           :index -1))
                   (has-targets
                     (sum-totals rows grand-total percent
                                 0 most-positive-fixnum
                                 :target t)))
              (map nil #'add horizontal)
              (setf horizontal
                    (append horizontal
                            (if has-targets
                                (list grand-total percent)
                                (list grand-total)))))
            (map nil #'add horizontal))))
    (adjust-col-spans horizontal)
    (assign-indexes horizontal)
    table))

(defun form-headers (rows-type rows &key row-type-col-span)
  (when rows
    (add (row))
    (when rows-type
      (loop with depth = (headers-depth rows)
            for rows-type in (alexandria:ensure-list rows-type)
            do
            (add
             (header rows-type
                     :row-span depth
                     :col-span (or row-type-col-span 1)
                     :h-align :left))))
    (form-headers
     nil
     (loop for header in rows
           do
           (add
            (header
             (text header)
             :vertical-text (vertical-text header)
             :bg-color (bg-color header)
             :col-span (col-span header)
             :row-span (row-span header)
             :h-align :left))
           when (typep header 'form-header)
           append (children header)))))

(defun form-totals (rows &key (total-string "Total")
                              col-span)
  (add (row (header total-string :col-span (or col-span 1))))
  (loop for row across rows
        do (add (header (format-number row)))))

(defun form-fill-row (row)
  (let ((sorted (sort (copy-seq (children row)) #'<
                      :key (lambda (x) (index (car x))))))
    (loop for index = -1 then (index cell)
          for (cell value path) in sorted
          do
          (loop repeat (1- (- (index cell) index))
                do (add (cell "")))
          (add (cell (if path
                         (qv-link (format-number value) path)
                         (format-number value)))))))

(defun terminal-header-p (header)
  (not (typep (car (children header)) 'form-header)))

(defun lay-rows (rows)
  (labels ((recurse (rows top)
             (loop for first-row = (not top) then nil
                   for row in rows
                   for terminal = (terminal-header-p row)
                   do
                   (unless first-row
                     (add (row)))
                   (add (header (text row)
                                :row-span (row-span row)
                                :bg-color (bg-color row)))
                   (if terminal
                       (form-fill-row row)
                       (recurse (children row) nil)))))
    (recurse rows t)))

(defun form-rows (rows-type table)
  (with-slots (vertical horizontal) table
    (form-headers rows-type horizontal
                  :row-type-col-span 1)
    (lay-rows vertical)))

(defun render-form (rows-type data
                    &key (add-totals t))
  (let ((totals (and data add-totals
                     (calculate-totals data))))
    (with-document (:orientation :landscape)
      (with-slots (rows vertical horizontal) data
        (if (and vertical horizontal)
            (with-table ()
              (let* ((non-perm (find "Non-permanent employees" vertical
                                     :key #'text :test #'equal)))
                (when non-perm
                  (setf vertical (remove non-perm vertical))
                  (setf rows (remove non-perm rows)))
                (form-rows rows-type data)
                (when non-perm
                  (when add-totals
                    (form-totals (calculate-totals data)
                                 :total-string "Total permanent"))
                  (lay-rows (list non-perm)))
                (when add-totals
                  (form-totals totals :col-span (if (consp rows-type)
                                                    (length rows-type)
                                                    1)))))
            (add (paragraph "No results")))))))

;;;

(defun test-period (doc start end)
  (or (null start)
      (null end)
      (with-slots (effective-date stamp-date) doc
        (and stamp-date
             (<= stamp-date end)
             (or (not effective-date)
                 (>= effective-date start))))))

(defun data-iterator (collection type
                      &optional test)
  (let* ((period *reporting-period*)
         (entities *quickview-entities*))
    (lambda (function)
      (loop for latest-doc across collection
            for doc = (period-version latest-doc period)
            when (and (typep doc type)
                      (check-entity doc entities)
                      (or (not test)
                          (funcall test doc)))
            do (funcall function doc)))))

(defun iterate-data (collection type function)
  (funcall (data-iterator collection type)
           function))

(defun %sort-headers (headers test)
  (if (and headers (sort-order (car headers)))
      (sort headers #'<
            :key (lambda (x)
                   (or (sort-order x)
                       most-positive-fixnum)))
      (sort headers test
            :key (lambda (x)
                   (princ-to-string (text x))))))

(defun sort-headers (headers test)
  (let ((sorted (%sort-headers headers test)))
    (loop for header in sorted
          unless (terminal-header-p header)
          do (setf (children header)
                   (sort-headers (children header) test)))
    sorted))

(defun make-table (v-headers h-headers rows columns
                   &key (add-totals t)
                        (add-grand-total t)
                        sort
                        sort-vertical-test
                        sort-horizontal-test)
  (let ((v-headers (if sort
                       (sort-headers v-headers
                                     (or sort-vertical-test
                                         #'string<))
                       v-headers))
        (h-headers (if sort
                       (sort-headers h-headers
                                     (or sort-horizontal-test
                                         #'string<))
                       h-headers)))
    (adjust-col-spans h-headers)
    (adjust-row-spans v-headers)
    (assign-indexes h-headers)
    (adjust-vertical-row-spans h-headers)
    (let ((table (make-instance 'form-table
                  :vertical v-headers
                  :horizontal h-headers
                  :rows rows
                  :columns columns)))
      (if (and add-totals (> (length h-headers) 1))
          (add-totals table
                      :add-grand-total add-grand-total)
          table))))

(defmethod text ((string string))
  string)

(defmethod text ((number number))
  number)

(defun find-existing-header (text parent)
  (find (text (text text)) (children parent)
        :key (lambda (x) (text (text x))) :test #'equalp))

(defun add-header (value parent answers)
  (let (new)
    (let ((header (find-existing-header value parent)))
      (unless header
        (setf header
              (typecase value
                (form-header
                 (copy-object value))
                (t
                 (make-instance (if (typep parent 'form-header)
                                    (class-of parent)
                                    'form-header)
                                :text value
                                :parent parent)))
              new t)
        (push header (children parent)))
      (when answers
        (push value (children answers)))
      (values header new))))

(defun cell-summ (doc v-header h-header iterator tests answers
                  &key (amount 1)
                       (add-path t))
  (declare (ignore doc))
  (let ((cell (assoc h-header (children v-header))))
    (if cell
        (incf (cadr cell) amount)
        (prog1 amount
          (push (list* h-header amount
                       (and add-path
                            (list (make-instance 'cell-path
                                                 :tests tests
                                                 :results (reverse (children answers))
                                                 :iterator iterator))))
                (children v-header))))))

(defun cell-change (value v-header h-header)
  (let ((cell (assoc h-header (children v-header))))
    (if cell
        (setf (cadr cell) value)
        (push (list h-header value)
              (children v-header)))))

(defun add-header-value (header-text value h-header v-header
                         iterator tests answers
                         &key add-path)
  (let ((new-header (add-header header-text h-header answers)))
    (push (list* new-header value
                 (and add-path
                      (list
                       (make-instance 'cell-path
                                      :tests tests
                                      :results (reverse (children answers))
                                      :iterator iterator))))
          (children v-header))))

(defun add-header-value-new (header-text value h-header v-header
                             iterator tests answers)
  (pushnew (list (add-header header-text h-header answers) value)
           (children v-header) :key #'car))

(defun add-header-sum (header-text value h-header v-header
                       iterator tests answers
                       &key add-path)
  (let ((new-header (add-header header-text h-header answers)))
    (cell-summ nil v-header new-header iterator tests answers
               :amount value
               :add-path add-path)))

(defun change-header (header-text value h-header v-header)
  (cell-change value v-header (add-header header-text h-header nil)))

(defun find-header (doc test
                    headers rows/columns answers
                    final-function)
  (let (new
        (result headers))
    (when test
      (loop for current-test = test
            for value = (setf (values value test) (funcall test doc))
            when (bogus-result-p value) do (return-from find-header)
            do
            (cond ((consp value)
                   (loop with answers-copy = (copy-list (children answers))
                         for x in value
                         do
                         (find-header doc test (add-header x result answers)
                                      rows/columns answers
                                      final-function)
                         (setf (children answers) answers-copy))
                   (return-from find-header))
                  (t
                   (setf (values result new) (add-header value result answers))))
            while test))
    (when new
      (push result (children rows/columns)))
    (funcall final-function result)))

(defun table-builder (iterator verticals horizontals
                      &key (add-totals t)
                           (add-grand-total t)
                           (final-function #'cell-summ)
                           (sort t)
                           target-iterator
                           target-verticals
                           target-horizontals
                           (target-final-function #'cell-summ)
                           sort-vertical-test
                           sort-horizontal-test)
  (let ((v-headers (make-container))
        (h-headers (make-container))
        (rows (make-container))
        (columns (make-container))
        (tests (cons verticals horizontals)))
    (funcall
     iterator
     (lambda (x)
       (let ((answers (make-container)))
         (find-header x verticals v-headers rows answers
                      (lambda (v-header)
                        (find-header
                         x horizontals h-headers columns answers
                         (lambda (h-header)
                           (funcall final-function x
                                    v-header h-header iterator tests answers))))))))
    (when target-iterator
      (funcall
       target-iterator
       (lambda (x)
         (let ((answers (make-container)))
           (find-header x target-verticals v-headers rows answers
                        (lambda (v-header)
                          (find-header
                           x target-horizontals h-headers columns answers
                           (lambda (h-header)
                             (funcall target-final-function x
                                      v-header h-header iterator tests answers)))))))))
    (make-table (children v-headers)
                (children h-headers)
                (children rows)
                (children columns)
                :add-totals add-totals
                :add-grand-total add-grand-total
                :sort sort
                :sort-vertical-test sort-vertical-test
                :sort-horizontal-test sort-horizontal-test)))

(defun bio-iterator (&optional test &key include-terminated)
  (let ((end-date (end-date *reporting-period*)))
    (data-iterator (biographicals) 'biographical
                   (if include-terminated
                       test
                       (lambda (x)
                         (and (or (null test)
                                  (funcall test x))
                              (with-slots (date-of-engagement date-of-termination)
                                  x
                                (and date-of-engagement
                                     (<= date-of-engagement end-date)
                                     (or include-terminated
                                         (null date-of-termination)
                                         (> date-of-termination end-date))))))))))
(defun target-iterator (target)
  (let ((targets (get-targets target))
        (period *reporting-period*)
        (entities *quickview-entities*))
   (lambda (function)
     (loop for latest-doc being the elements of targets
           for doc = (period-version latest-doc period)
           when (and doc (check-entity doc entities))
           do (funcall function doc)))))

(defvar *target-color* "00cccc")

(defun target-horizontal (&key (header "Target") (key #'cdr))
  (let ((target-header (make-instance 'target-header
                                      :text header
                                      :sort-order -1
                                      :bg-color *target-color*)))
    (lambda (x)
      (let ((targets (funcall key (split-target (cadr x)))))
        (labels ((get-header (x)
                   (declare (ignore x))
                   (values (pop targets)
                           (and targets
                                #'get-header))))
          (values target-header (and targets
                                     #'get-header)))))))

(defun target-final ()
  (lambda (doc v-header h-header iterator tests answers)
    (let ((target (car doc)))
      (cell-summ doc v-header h-header iterator tests answers
                 :amount (if (numberp target)
                             target
                             0)
                 :add-path nil))))

(defun bio-data (verticals horizontals
                 &optional test &key include-terminated)
  (table-builder (bio-iterator test :include-terminated include-terminated)
                 verticals horizontals))

(defun designatedp (person)
  (not (or (not (equal (slot-value person 'nationality) "South African"))
           (and (equal (slot-value person 'gender) "Male")
                (equal (slot-value person 'race) "White")))))

(defun designated (person)
  (if (designatedp person)
      "Designated"
      "Non Designated"))

(defmacro branch (result &body body)
  `(values ,result
           ,@(if body
                `((lambda (x)
                    (declare (ignorable x))
                    ,@body)))))

(defparameter *bio-table*
  (lambda (x)
    (with-slots (nationality gender race) x
      (branch (designated x)
        (if (equal nationality "South African")
            (branch gender
              (branch race))
            (branch "Foreign Nationals"
              (branch gender)))))))

(defparameter *bio-table*
  (let ((designated-header (make-header "Designated" :sort 1))
        (non-designated-header (make-header "Non-Designated" :sort 0)))
    (lambda (x)
      (with-slots (nationality gender race) x
        (let ((designated (designatedp x)))
          (if designated
              (branch designated-header
                (branch gender
                  (branch race)))
              (branch non-designated-header
                (if (equal nationality "South African")
                    (branch "White Male")
                    (branch "Foreign Nationals"
                      (branch gender))))))))))

(defun permanent-test (function)
  (lambda (x)
    (if (equal (slot-value x 'employment-type) "Permanent")
        (funcall function x)
        "Non-permanent employees")))
