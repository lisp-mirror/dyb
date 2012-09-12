(in-package :ems-quickview)

(defvar *quickview-entities* ())
(defvar *current-quickview* nil)
(defvar *quickview-page* nil)
(defvar *reporting-periods* nil)

(defclass quickview-page (page)
  ()
  (:metaclass widget-class))

(defclass local-entity-selector (input-widget)
  ((cb-list :initform nil
            :accessor cb-list)
   (entities :initarg :entities
             :initform nil
             :accessor entities)))

(defun entity-with-names ()
  (loop for xid in (context)
        collect (list xid (entity-name (get-entity-by-id xid)))))

(defmethod render ((widget local-entity-selector) &key page)
  (let* ((all-entities (entity-with-names))
         (selected (context))
         (cb-list
           (make-widget 'checkbox-list
                        :name (fmt "~a-cb-list" (name widget))
                        :orientation :horizontal
                        :on-change (js-render-form-values page
                                                          "local-entities-select-form"))))
    (unless (equal all-entities (entities widget))
      (setf (items cb-list)
            (loop for (id name) in all-entities
                  collect (list id name
                                (and (find id selected) t))))
      (setf (entities widget) all-entities))
    (if selected
        (with-html
          (:div :class "local-entities-select"
                (:b "Selected entities:")
                (:form :action ""
                       :method "post"
                       :id "local-entities-select-form"
                       (render cb-list))))
        (with-html
          (:a :href "/ems/context"
              (:h3 "Set context"))))
    (setf (cb-list widget) cb-list)
    (setf (value widget) (value cb-list))))

(defmethod action-handler ((widget local-entity-selector))
  (setf (value widget)
        (value (cb-list widget))))

(defclass quickview-widget (widget)
  ((title :initarg :title
          :initform nil
          :accessor title)
   (entities :initarg :entities
             :initform nil
             :accessor entities)
   (period :initarg :period
           :initform nil
           :accessor period)
   (period-type :initarg :period-type
                :initform :range
                :type (member :range :predefined)
                :accessor period-type)))

(defclass quickview-page (ajax-widget)
  ((quickview :initarg :quickview
              :initform nil
              :accessor quickview)
   (links :initarg :links
          :initform nil
          :accessor links)
   (last-link-id :initarg :last-link-id
                 :initform -1
                 :accessor last-link-id))
  (:metaclass widget-class))

(defun clear-links (widget)
  (setf (links widget) nil
        (last-link-id widget) -1))

(defmethod render :around ((widget quickview-widget) &key)
  (let ((*current-quickview* widget))
    (call-next-method)))

(defun format-entities ()
  (cons
   "Entities: "
   (loop with first = t
         for xid in *quickview-entities*
         for name = (entity-name (get-entity-by-id xid))
         if first do (setf first nil)
         else collect ", "
         collect name)))

(defun add-pdf-header (document &key entities-header
                                     title)
  (setf (document-header document)
        (paragraph ""
          :elements
          (list*
           title
           (image "/appimgx-packs-logo-small.jpg")
           "Reporting period: "
           (and *reporting-period*
                (format nil "~a - ~a"
                        (format-date (start-date *reporting-period*))
                        (format-date (end-date *reporting-period*))))
           (new-line)
           (when entities-header
             (format-entities)))))
  (propagate-doc-header document))

(defun serve-pdf (document &key (entities-header t)
                                title)
  (let ((path (make-pathname
               :name (string-left-trim
                      "-" (ppcre:regex-replace-all "/+" (script-name*) "-"))
               :type "pdf"
               :directory
               (append (pathname-directory *public-dir*) '("pdf")))))
    (ensure-directories-exist path)    
    (add-pdf-header document
                    :entities-header entities-header
                    :title title)
    (with-pdf (path)
      (render-element document :pdf))
    (redirect (conc "/pdf/" (file-namestring path)))))

(defun make-quickview-renderer (class)
  (let ((quickview (make-widget class)))
    (lambda ()
      (let ((*reporting-period* (getf *reporting-periods*
                                      (if (eql (period-type quickview) :both)
                                          :range
                                          (period-type quickview)))))
        (setf (entities quickview) *quickview-entities*)
        (setf (period quickview) *reporting-period*)
        (render quickview)))))

(defun render-quickview (class &rest args)
  (let* ((quickview (make-widget class))
         (page (make-widget 'page :title (title quickview))))
    (render page
            :body
            (apply #'render-to-string (make-widget 'quickview-page
                                                   :quickview quickview)
                   args))))

(defun render-quickview-inline (class)
  (funcall (make-quickview-renderer class)))

(defmethod render ((page quickview-page)
                   &key (entity-selector t)
                        (pdf-link t))
  (clear-links page)
  (let* ((entity-selector (and entity-selector
                               (make-widget 'local-entity-selector
                                            :name (fmt "~a-entity-select"
                                                       (name page)))))
         (quickview (quickview page))
         (period-type (period-type quickview))
         (range (when (member period-type '(:range :both))
                  (make-widget 'reporting-period-selector)))
         (predefined (when (member period-type '(:predefined :both))
                       (make-widget 'reporting-quarter-selector)) )
         (type (get-parameter "type"))
         (type (cond ((equal type "pdf") :pdf)
                     (t :html)))
         (*quickview-page* page))
    (with-html
      (when entity-selector
        (render entity-selector :page page))
      (let ((*quickview-entities*
              (if entity-selector
                  (value entity-selector)
                  (context))))
        (when *quickview-entities*
          (htm
           (ecase period-type
             (:range (render range))
             (:predefined (render predefined))
             (:both
              (render range)
              (render predefined)))
           (when pdf-link
             (htm
              (:a :style "margin-left: 20px"
                  :href
                  (format nil "~a?type=pdf" (script-name*))
                  :target "_blank"
                  "PDF"))))
          (let* ((range (and range (value range)))
                 (predefined (and predefined (value predefined)))
                 (*reporting-periods*
                   (list :range range
                         :predefined predefined))
                 (*reporting-period* (or range predefined))
                 (body (make-quickview-renderer (class-of quickview)))
                 (document (funcall body)))
            (cond ((null document))
                  ((null *quickview-entities*))
                  ((eql type :pdf)
                   (serve-pdf document
                              :entities-header entity-selector
                              :title (title quickview)))
                  (t
                   (let ((drill-down (make-widget 'drill-down
                                                  :page page)))
                     (with-html
                       (:div :class "grid_12"
                             (render-element document :html)
                             (render drill-down))))))))))))

;;;

(defclass reporting-period ()
  ((entity :initarg :entity
           :initform nil
           :accessor entity-name)
   (name :initarg :name
         :initform nil
         :accessor name)
   (start-date :initarg :start-date
               :initform nil
               :accessor start-date)
   (end-date :initarg :end-date
             :initform nil
             :accessor end-date)))

(defmethod print-object ((period reporting-period) stream)
  (print-unreadable-object (period stream :type t :identity t)
    (format stream "~a - ~a"
            (format-date (start-date period))
            (format-date (end-date period)))))

(defclass reporting-period-selector (input-widget)
  ((start :initarg :start
          :initform nil
          :accessor start)
   (end :initarg :end
        :initform nil
        :accessor end)))

(defun year-start (date)
  (multiple-value-bind (year month day)
      (decode-date date)
    (declare (ignore day month))
    (encode-universal-time 0 0 0 1 1 year)))

(defun year-end (date)
  (multiple-value-bind (year month day)
      (decode-date date)
    (declare (ignore day month))
    (encode-universal-time 0 0 0 31 12 year)))

(defun selected-date ()
  (session-value 'selected-date))

(defun (setf selected-date) (value)
  (setf (session-value 'selected-date) value))

(defun greatest-universal-date (date)
  (multiple-value-bind (year month day) (decode-date date)
    (let* ((ts (local-time:encode-timestamp 0 0 0 0 day month year))
           (next-day (local-time:timestamp+ ts 1 :day)))
      (1- (local-time:timestamp-to-universal next-day)))))

(defmethod render ((widget reporting-period-selector) &key)
  (let ((time (get-universal-time))
        (selected (selected-date))
        start-date
        end-date)
    (if selected
        (setf start-date (format-date (car selected))
              end-date (format-date (cdr selected)))
        (setf start-date (format-date (year-start time))
              end-date (format-date (year-end time))))
    (with-html
      (:form :action ""
             :class "period-select validate"
             :name "reporting-period-form"
             :method "post"
             (:input :type "text"
                     :class "date-pick required"
                     :name "start-date"
                     :value start-date)
             (:input :type "text"
                     :class "date-pick required"
                     :name "end-date"
                     :value end-date)
             (:input :type "submit"                     
                     :name "action"
                     :value "Change")))
    (unless selected
      (setf selected
            (setf (selected-date) (cons (year-start time)
                                        (greatest-universal-date (year-end time))))))
    (setf (value widget)
          (make-instance 'reporting-period
                         :start-date (car selected)
                         :end-date (cdr selected)))))

(defmethod action-handler ((widget reporting-period-selector))
  (with-parameters (start-date end-date)
    (let* ((start-date (and start-date (parse-date start-date)))
           (end-date (and end-date (parse-date end-date)))
           (end-date (and end-date
                          (greatest-universal-date end-date))))
      (when (and start-date end-date)
        (setf (selected-date) (cons start-date end-date))
        (setf (value widget)
              (make-instance 'reporting-period
                             :start-date start-date
                             :end-date end-date))))))
;;;

(defclass reporting-quarter-selector (input-widget)
  ((periods :initarg :periods
            :initform nil
            :accessor periods-list)
   (select :initarg :select
            :initform nil
            :accessor select)))

(defun make-reporting-period-name (period)
  (fmt "~a - ~a" (entity-name (entity period))
       (period-name period)))

(defun find-reporting-period (name widget)
  (find name (periods-list widget)
        :key #'make-reporting-period-name
        :test #'equal))

(defmethod render ((widget reporting-quarter-selector) &key)
  (let ((periods (filter-entities (filter-active (periods))))
        (select (make-widget 'select
                             :name "periods")))
    (setf (on-change select)
          (js-render *quickview-page*
                     (js-value select)))
    (setf (items select) (map 'list #'make-reporting-period-name periods))
    (setf (select widget) select
          (periods-list widget) periods)
    (when (or (not (value select))
              (not (find-reporting-period (value select) widget)))
      (setf (value select) (car (items select))))
    (when (and (or (not (value widget))
                   (not (find-reporting-period (value (select widget)) widget)))
               (plusp (length periods)))
      (setf (value widget) (elt periods 0)))
    (with-html
      (:div :class "period-select"
            (if periods
                (htm
                 (:label "Reporting period: ")
                 (render select))
                (htm
                 (:strong :style "color: red;" "No reporting periods")))))))

(defmethod action-handler ((widget reporting-quarter-selector))
  (setf (value widget) (find-reporting-period (value (select widget)) widget)))

;;;

(defclass mine-selector (input-widget)
  ((mines :initarg :mines
          :initform nil
          :accessor mine-list)
   (select :initarg :select
           :initform nil
           :accessor select)))

(defun find-mine (name widget)
  (find name (mine-list widget) :test #'equal :key #'mine-name))

(defun mine-name (mine)
  (and (typep mine 'mine)
       (entity mine)
       (entity-name (entity mine))))

(defmethod render ((widget mine-selector) &key)
  (let ((mines (filter-entities (mines)))
        (select (make-widget 'select :name "mines")))
    (setf (on-change select) (js-render *quickview-page* (js-value select))
          (items select) (map 'list #'mine-name mines)
          (select widget) select
          (mine-list widget) mines)
    (when (or (not (value select))
              (not (find-mine (value select) widget)))
      (setf (value select) (car (items select))))
    (cond ((zerop (length mines))
           (setf (value widget) nil)
           (setf (value select) nil))
          ((and (or (not (value widget))
                    (not (find-mine (value (select widget)) widget))))
           (setf (value widget) (elt mines 0))))
    (with-html
      (:div :class "mine-selector"
            (:label "Mine:")
            (if (plusp (length mines))
                (render select)
                (htm
                 (:strong :style "color: red;" "No mines")))))))

(defmethod action-handler ((widget mine-selector))
  (setf (value widget) (find-mine (value (select widget)) widget)))

(defun add-mine-selector (&optional name)
  (let ((mine-selector (make-widget 'mine-selector :name name)))
    (render mine-selector)
    (value mine-selector)))
