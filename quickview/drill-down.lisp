(in-package :ems-quickview)

(defclass drill-down (ajax-widget)
  ((page :initarg :page
         :initform nil
         :accessor page)))

(defclass quickview-link (link)
  ((id :initarg :id
       :initform nil
       :accessor id)
   (quickview :initarg :quickview
              :initform nil
              :accessor quickview)
   (path :initarg :path
          :initform nil
          :accessor path)))

(defun drill-down-url (quickview page id)
  (with-slots (drill-down-page) quickview
    (if drill-down-page
        (fmt "~a?filter=drill&s=~a&w=~a&l=~a"
             drill-down-page
             (script-name*)
             (name page)
             id)
        (fmt "javascript:~a"
             (js-render "drill-down"
                        (js-pair "id" id)
                        (scroll-to "drill-down"))))))

(defun qv-link (text path
                &key (page *quickview-page*))
  (if t ;; (check-page-permission "drill-down" nil)
      (let* ((id (incf (last-link-id page)))
             (link (make-instance 'quickview-link
                                  :text text
                                  :path path
                                  :quickview *current-quickview*
                                  :id id
                                  :url (drill-down-url *current-quickview* page id)
                                  :target (and (drill-down-page *current-quickview*)
                                               :blank))))
        (push link (links page))
        link)
      text))

(defgeneric display-drill-down (quickview link &key))
(defgeneric quickview-details (quickview path))
(defgeneric quickview-details-description (quickview &key &allow-other-keys))

(defun details-description-plist (link)
  (let* ((parameters (remove-plist-duplicates (parameters link)))
         (keys (apply #'quickview-details-description (quickview link)
                      parameters)))
    (if keys
        (leave-in-plist parameters keys)
        parameters)))

(defun format-description-title (title)
  (ppcre:regex-replace " desc$" (format-symbol title) ""))

(defun details-description (link)
  (with-html
    (loop for (key value) on (details-description-plist link) by #'cddr
          for name = (format-description-title key)
          do (case value
               ((nil))
               ((t) (htm (:b (esc name))
                         (:br)))
               (t (htm (esc (conc name  ": "))
                       (:b (str (escape value)))
                       (:br)))))))

(defmethod render ((widget drill-down) &key)
  (with-parameters (id)
   ;; (check-permission script "drill-down")
   (let* ((id (ensure-parse-integer id))
          (link (find id (links (page widget)) :key #'id)))
     (when link
       (display-drill-down (quickview link) link
                           :drill-down widget)))))

(defclass drill-down-grid (grid)
  ((link :initarg :link
         :initform nil
         :accessor link))
  (:default-initargs :editable nil))

(defmethod get-rows ((grid drill-down-grid))
  (setf (rows grid)
        (loop with link = (link grid)
              for object in
              (quickview-details
               (quickview link) (path link))
              collect object)))

(defmethod display-drill-down ((qv quickview-widget) link
                               &key drill-down)
  (let ((grid
          (make-widget 'drill-down-grid
                         :columns
                         (list
                          ;; (make-instance 'grid-column :name 'entity)
                          ;; (make-instance 'grid-column :name 'employee-number)
                          (make-instance 'grid-column :name 'name)
                          ;; (make-instance 'grid-column :name 'surname)
                          )
                         :name "drill-down-grid")))
    (setf (link grid) link)
    (render grid
            :footer
            (with-html-string
              (:div :class "actions"
                    (:div :class "actions-rightleft"
                          (:button :class "red"
                                   :onclick
                                   (js-render drill-down
                                              (scroll-to grid))
                                   "Ok")))))))

(defmethod quickview-details-description (quickview &key)
  nil)

(defmethod apply-grid-filter (grid data (filter (eql :drill)))
  (let* ((params (filter-parameters grid))
         (script (assoc-value "s" params))
         (widget-name (assoc-value "w" params))
         (link (assoc-value "l" params))
         (widget (get-widget widget-name :script-name script))
         (id (ensure-parse-integer link))
         (link (and widget id
                    (find id (links widget) :key #'id))))
    (when link
      (quickview-details (quickview link) (path link)))))
