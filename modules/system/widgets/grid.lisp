(in-package :ems)

(defclass grid (ajax-widget)
  ((columns
    :initarg :columns :initform nil
    :accessor columns)
   (row-object-class
    :initarg :row-object-class :initform nil
    :accessor row-object-class)
   (rows
    :initarg :rows :initform nil
    :accessor rows)
   (total-row-count :initarg :total-row-count
                    :initform nil
                    :accessor total-row-count) 
   (rows-per-page
    :initarg :rows-per-page :initform 10
    :accessor rows-per-page)
   (selected-row :initform nil
                 :accessor selected-row)
   (editable
    :initarg :editable :initform t
    :accessor editable)
   (edit-inline
    :initarg :edit-inline :initform t
    :reader edit-inline)
   (table-type
    :initarg :table-type :initform "table"
    :accessor table-type)
   (grid-links
    :initarg :grid-links :initform nil
    :accessor grid-links)
   (editing-row
    :initform nil :accessor editing-row)
   (edit-form
    :initform nil
    :accessor edit-form)
   (error-message
    :initform nil :accessor error-message)
   (allowed-actions
    :initarg :allowed-actions :initform nil
    :accessor allowed-actions)
   (title :initarg :title
          :initform nil
          :accessor title)
   (editor :initarg :editor
           :initform nil
           :accessor editor)
   (search-term :initarg :search-term
                :initform nil
                :accessor search-term)
   (sort-direction :initarg :sort-direction
                   :initform :ascending
                   :accessor sort-direction)
   (sort-column :initarg :sort-column
                :initform nil
                :accessor sort-column)
   (grid-filter :initarg :grid-filter
                :initform nil
                :accessor grid-filter)
   (filter-parameters :initarg :filter-parameters
                      :initform nil
                      :accessor filter-parameters)))

(defclass grid-column ()
  ((header :initarg :header
           :initform nil
           :accessor header)
   (name :initarg :name
         :initform nil
         :accessor name)
   (printer :initarg :printer
            :initform nil
            :accessor printer)
   (style :initarg :style
          :initform nil
          :accessor style)
   (width :initarg :width
          :initform nil
          :accessor width)
   (editor-type :initarg :editor-type
                :initform :text
                :accessor editor-type)
   (editor-init-values :initarg :editor-init-values
                       :initform nil
                       :accessor editor-init-values)))

(defmethod initialize-instance :after ((column grid-column) &key)
  (with-slots (header name) column
    (when (and name (not header))
      (setf header (string-capitalize name)))))

(defgeneric delete-row (grid row))
(defgeneric row-count (grid))
(defgeneric get-rows (grid))

(defmethod get-rows :after ((grid grid))
  (setf (total-row-count grid) (length (rows grid))))

(defmethod row-count ((grid grid))
  (length (rows grid)))

(defclass grid-link (widget)
  ((value :initarg :value
          :initform nil)
   (image :initarg :image
          :initform nil)
   (task :initarg :task
         :initform nil
         :accessor task)
   (pass-param :initarg :pass-param
         :initform nil
         :accessor pass-param)
   (url :initarg :url
        :initform nil)
   (alt :initarg :alt
        :initform nil)
   (enabled-p :initarg :enabled-p
              :initform t)))

(defun build-parameters (params values)
  (let ((stream (make-string-output-stream)))
    (mapcar (lambda (x y)
              (format stream "&param-~A=~A" (string-downcase x) y))
            params values)
    (get-output-stream-string stream)))

(defun build-parameters-hidden (params values)

  (if (listp params)
    (let ((stream (make-string-output-stream)))
      (with-html-to-string ()
        (mapcar (lambda (x y)
                  (htm (:input :type "hidden" :name (format nil "param-~A" (string-downcase x))
                               :value y)))
                params values))
      (get-output-stream-string stream)) ;; FIXME: this can't work
    (with-html-to-string ()
      (htm
       (:input :type "hidden" :name params
                   :value values)))))

(defun parse-parameters (parameters values)
  (if (and parameters (listp parameters))
      (build-parameters parameters values)
      (if (nbl-p parameters)
          (format t "&pass-param=~A" parameters)
          "")))

(defmethod render ((widget grid-link)
                   &key task (row-id 0) value image
                        confirmation param-values
                   grid-name)
  (let ((name (widgy-name widget (format nil "~a~d" task row-id)))
        (alt (if (slot-val widget 'alt)
                 (slot-val widget 'alt)
                 (string-capitalize task))))
    (if (slot-val widget 'url)
        (with-html

          (if (slot-val widget 'enabled-p)
              (htm
               (:a :href (format nil "~A?link-~A=~A~A"
                                 (slot-val widget 'url)
                                 (if task
                                     task
                                     (string-downcase (slot-val widget 'task)))
                                 value
                                 (parse-parameters (slot-val widget 'pass-param)
                                                   param-values))
                   (:img :src (if image
                                  image
                                  (slot-val widget 'image))
                         :alt alt
                         :title alt)))
              (htm
               (:img :src (if image
                              image
                              (slot-val widget 'image))
                     :alt alt
                     :title alt))))
        (with-html
          (:form :action ""
                 :method "post"
                 :id name
                 (:div
                  (:input :type "hidden" :name "grid-name"
                          :value grid-name)
                  (:input :type "hidden" :name "action"
                          :value (if task
                                     task
                                     (slot-val widget 'task)))

                  (if (slot-val widget 'pass-param)
                      (str (build-parameters-hidden (slot-val widget 'pass-param)
                                                    (or param-values (slot-val widget 'value)))))

                  (:input :type "hidden" :name "row_id" :value row-id)

                  (if (or value (slot-val widget 'value))
                      (htm (:input :type "hidden" :name "value"
                                   :value (slot-val widget 'value))))
                  (if (slot-val widget 'enabled-p)
                      (htm (:a :href
                               (format nil
                                       "javascript:~@[if(confirm(\"~a\"))~]~
document.getElementById(\"~A\").submit();"
                                       confirmation name)
                               (:img :src (if image
                                              image
                                              (slot-val widget 'image))
                                     :alt alt
                                     :title alt)))
                      (htm
                       (:img :src (if image
                                      image
                                      (slot-val widget 'image))
                             :alt alt
                             :title alt)))))))))

(defun set-current-row (grid)
  (let ((row-id (integer-paremeter "row_id")))
    (when row-id
      (setf (selected-row grid)
            (elt (rows grid) row-id)))))

(defmethod handle-action ((object grid) action))

(defmethod handle-action :before ((object grid) action)
  (when *logging*
   (format *debug-io* "~a ~a~%" action object)))

#|
(defmethod handle-action ((grid grid) (action (eql 'delete)))
  (when (editable grid)
    (let ((row (selected-row grid)))
      (setf (rows grid) (remove row (rows grid)))
      (dq-delete-object row))))
|#

(defmethod handle-action ((grid grid) (action (eql 'init-new)))
  (editing-row grid))

(defmethod handle-action ((grid grid) (action (eql 'new)))
  (when (editable grid)
    (let* ((rows (rows grid))
           (first-row (and (plusp (length rows))
                           (elt rows 0)))
           (object-class
             (or (row-object-class grid)
                 (and first-row
                      (class-of first-row)))))
      (when object-class
        (let ((new-instance (make-instance object-class)))
          (push new-instance (rows grid))
          (setf (editing-row grid) new-instance))
        (handle-action grid 'init-new)))))

(defun parse-cell (key rows columns)
  (ppcre:register-groups-bind (name row-id column-id)
      ("^(cell(\\d+):(\\d+))(\\..+)?$" key)
    (when (and name row-id column-id)
      (let ((row-id (parse-integer row-id))
            (column-id (parse-integer column-id)))
        (when (and (>= row-id 0) (>= column-id 0))
          (let ((row (elt rows row-id))
                (column (nth column-id columns)))
            (when (and row column)
              (values row column
                      (value (get-widget name))))))))))

(defun parse-cells (grid)
  (loop with affected-rows = ()
        for (key . value) in (post-parameters*)
        do
        (multiple-value-bind (row column value)
            (parse-cell key (rows grid) (columns grid))
          (when row
            (let ((cons (assoc row affected-rows :test #'eq)))
              (if cons
                  (push (cons column value) (cdr cons))
                  (push (list row (cons column value))
                        affected-rows)))))
        finally (return affected-rows)))

(defun log-object (object)
  (make-instance
   (find-symbol
    (format nil "~a-~a" (class-name (class-of object)) 'log)
    #.*package*)))


(defmethod handle-action ((grid grid) (action (eql 'edit)))
  (setf (editing-row grid) (selected-row grid)))

(defmethod handle-action ((grid grid) (action (eql 'cancel)))
  (when (edit-form grid)
    (setf (object (edit-form grid)) nil)
    (setf (edit-form grid) nil))
  (finish-editing grid))

(defmethod handle-action ((grid grid) (action (eql 'edit-all)))
  (setf (editing-row grid) :all))

(defmethod action-handler ((grid grid))
  (when (equal (name grid) (parameter "grid-name"))
    (setf (error-message grid) nil)
    (set-current-row grid)
    (handle-action grid
                   (find-symbol (string-upcase (parameter "action"))
                                #.*package*))))

(defun render-grid-header (widget)
  (with-html
    (:thead
     (:tr
      (dolist (column (columns widget))
        (htm (:th (esc (header column)))))
      (when (editable widget)
        (htm (:th)))))))

(defgeneric grid-present (value name type &key &allow-other-keys))

(defmethod grid-present (value name (type (eql :text)) &key init-values)
  (let ((text
          (make-widget 'text :id name :name name :css-class (first init-values)
                             :style (second init-values))))
    (setf (value text) value)
    (render text)))

(defmethod grid-present (value name (type (eql :checkbox)) &key init-values)
  (let ((checkbox
         (make-widget 'checkbox :id name :name name :css-class (first init-values)
                              :style (second init-values))))
    (setf (value checkbox) value)
    (render checkbox)))

(defmethod grid-present (value name (type (eql :select)) &key init-values)
  (let ((select (make-widget 'select :id name :name name
                             :items init-values
                             :first-item "")))
    (setf (value select) value)
    (render select)))

(defun render-editing-row (row row-id columns &key (save-button t))
  (with-html
    (loop for column in columns
          for column-id from 0
          for name = (slot-val row (name column))
          for text = (if (printer column)
                         (funcall (printer column) name)
                         name)
          for widget-name = (format nil "cell~d:~d" row-id column-id)
          do
          (htm (:td
                (grid-present text widget-name
                              (editor-type column)
                              :init-values (editor-init-values column)))))
    (when save-button
      (htm
       (:td (:input :type "hidden" :name "action" :value "save")
            (:input :type "image" :src "/images/save-small.png"))))))


(defgeneric apply-grid-link-rules (grid link row-object))

(defmethod apply-grid-link-rules ((grid grid) (link grid-link) row-object)
  t)

(defun test-allowed-action (allowed-actions action)
  (if allowed-actions
       (find action allowed-actions)
       t))

(defun collect-params-values (params object)
  (if (and params (listp params))
      (let ((col ()))
        (dolist (param params)
          (if (slot-exists-p object param)
              (setf col (append col (list (slot-val object param))))))
        col)))

(defun render-ajax-edit-button (grid row-id)
  (with-html
    (:a :href
        (js-link (scroll-to (editor grid))
                 (js-render (editor grid)
                            (js-pair "grid-name" (name grid))
                            (js-pair "action" "edit")
                            (js-pair "row_id" row-id)))
        (make-icon "card--pencil"
              :title "Edit"))))

(defun render-ajax-delete-button (grid row-id)
  (with-html
    (:a :href
        (js-link (js-render (editor grid)
                            (js-pair "grid-name" (name grid))
                            (js-pair "action" "delete")
                            (js-pair "row_id" row-id)))
        (make-icon "card--minus"
              :title "Delete"))))

(defun render-grid-rows (grid &key editing)
  (with-html
    (:table
        :id (sub-name grid "table")
      :class "table"
      (render-grid-header grid)
      (:tbody
       (:td :colspan (- (length (columns grid))
                        (if (editable grid)
                            0
                            1))
            "Loading data")
       )
      (when (eq editing :all)
        (htm
         (:td (:input :type "hidden" :name "action" :value "save")
              (:input :type "image" :src "/images/save.png")))))))

(defun scroll-to (widget)
  (format nil
          "$(\"html, body\").animate({scrollTop: $(\"#~a\").offset().top})"
          (if (typep widget 'widget)
              (name widget)
              widget)))

(defgeneric render-row-editor (grid row))

(defmethod render-row-editor ((grid grid) row)
  (let ((row-id (position row (rows grid)))
        (columns (columns grid)))
    (with-html
      (:div
       :class "grid_6"
       (:div
        :class "box"
        (box-header "Edit" :icon "card--pencil"
                           :collapsible t)
        (:form
         :method "post"
         :onsubmit "return false;"
         (:div
          :class "content no-padding"
          (loop for column in columns
                for column-id from 0
                for name = (slot-val row (name column))
                for text = (if (printer column)
                               (funcall (printer column) name)
                               name)
                for widget-name = (format nil "cell~d:~d" row-id column-id)
                do
                (htm
                 (:div :class "section _100"
                       (:label (esc (header column)))
                       (:div (grid-present text widget-name
                                           (editor-type column)
                                           :init-values (editor-init-values column)))))))
         (:div :class "actions"
               (:div :class "actions-left"
                     (:button :onclick "alert(\"Save does not work at the moment\");"
                              "Save"))
               (:div :class "actions-right"
                     (:button :class "red"
                              :onclick
                              (js-render (editor grid)
                                         (js-pair "grid-name" (name grid))
                                         (js-pair "action" "cancel")
                                         (scroll-to grid))
                              "Cancel")))))))))

(defclass grid-editor (ajax-widget)
  ((grid :initarg :grid
         :initform nil
         :accessor grid))
  (:metaclass widget-class))

(defmethod render ((editor grid-editor) &key)
  (let* ((grid (grid editor))
         (editing-row (editing-row grid)))
    (when (and (not (edit-inline grid))
               editing-row)
      (render-row-editor grid editing-row))))

(defmethod render ((grid grid) &key footer)
  (let ((editing-row (and (edit-inline grid)
                          (editing-row grid)))
        (editor (make-widget 'grid-editor
                             :name (sub-name grid "editor")
                             :grid grid))
        (filter (make-widget 'grid-filter-selector
                             :name (sub-name grid "filter-selector")
                             :grid grid)))
    (setf (editor grid) editor)
    (with-html
      (:div :class "box"
            (box-header (title grid)
                        :icon "table-excel"
                        :collapsible t
                        :body (with-html-string
                                (render filter)
                                (:button :onclick
                                         (format nil "window.open(\"/ems/export-csv?grid=~a&script-name=~a\")"
                                                 (name grid)
                                                 (script-name*))
                                         "Export CSV")))
            (:div :class "content"
                  (render-grid-rows grid :editing editing-row)))
      (:div :class "box"
            (:table
                :style "text-align:center"
              (:tr
               (:td
                (when (error-message grid)
                  (htm
                   (:div :class "edit-form-error"
                         (esc (error-message grid)))))))
              (:tr
               (:td
                (unless (or editing-row
                            (not (editable grid)))
                  (htm
                   (:table :class "table"
                     (:tr
                      (htm
                       (:td
                        (:a :href
                            (js-link (js-render (editor grid)
                                                (js-pair "grid-name" (name grid))
                                                (js-pair "action" "new")))
                            (make-icon "plus" :size 32
                                              :title "Add"))))
                      (when (edit-inline grid)
                        (htm
                         (:td
                          (let ((link (make-widget 'grid-link
                                                   :name "grid-link")))
                            (setf (slot-value link 'enabled-p)
                                  (test-allowed-action (allowed-actions grid) "Edit All"))
                            (render (make-widget 'grid-link
                                                 :name "grid-link"
                                                 :grid-name (name grid))
                                    :image "/images/edit.png"
                                    :task "edit-all"))))))))))))
            (str footer))
      (render editor)
      (defer-js
          (format nil "$('#~a').dataTable({
\"bProcessing\": true,
\"bServerSide\": true,
\"sAjaxSource\": \"/ems/ajax/TABLE?script-name=~a&id=~a\",
~:[~;\"aoColumnDefs\": [{\"bSortable\": false, \"aTargets\": [~a]}]~]
});"
                  (sub-name grid "table")
                  (script-name*)
                  (name grid)
                  (editable grid)
                  (length (columns grid)))))))

(defun column-text (row column)
  (let ((value (slot-val row (name column))))
    (if (printer column)
        (funcall (printer column) value)
        (princ-to-string value))))

(defun expand-columns (grid row)
  (loop for column in (columns grid)
        collect (column-text row column)))

(defun search-columns (grid row)
  (loop with term = (search-term grid)
        for column in (columns grid)
        thereis (search term (column-text row column)
                        :test #'equalp)))

(defun expand-all-columns (grid rows)
  (loop for row being the elements of rows
        collect (expand-columns grid row)))

(defun grid-filtered-rows (grid)
  (let ((filtered (apply-grid-filter grid
                                     (get-rows grid)
                                     (grid-filter grid))))
    (setf (rows grid)
          (sort-data grid
                     (remove-if-not
                      (lambda (row)
                        (search-columns grid row))
                      filtered)))))

(defun get-data-table-rows (grid &key (start 0)
                                      length)
  (let* ((data (grid-filtered-rows grid))
         (data-length (length data))
         (start (max 0 (min start (1- data-length))))
         (cut (subseq data start (and length
                                      (min data-length
                                           (+ start length))))))
    (values 
     (loop for row in (expand-all-columns grid cut)
           for id from start
           collect (append row
                           (list (with-html-string
                                   (render-ajax-edit-button grid id)
                                   (render-ajax-delete-button grid id)))))
     data-length)))

(defun sort-direction-function (grid)
  (if (eq (sort-direction grid) :ascending)
      #'string<
      #'string>))

(defun sort-data (grid rows)
  (let ((column (elt (columns grid) (sort-column grid))))
    (sort rows (sort-direction-function grid)
          :key (lambda (row) (column-text row column)))))

(defmethod process-data-table ((grid grid))
  (print (get-parameters*))
  (with-parameters ((s-echo "sEcho")
                    (start "iDisplayStart")
                    (length "iDisplayLength")
                    (search "sSearch")
                    (sort-column "iSortCol_0")
                    (sort-direction "sSortDir_0"))
    (setf (search-term grid)
          (unless (equal search "")
            search))
    (setf (sort-column grid) (ensure-parse-integer sort-column))
    (setf (sort-direction grid)
          (if (equal sort-direction "asc")
              :ascending
              :descending))
    (multiple-value-bind (data data-length)
        (get-data-table-rows grid
                             :start (or (ensure-parse-integer start)
                                        0)
                             :length (or (ensure-parse-integer length)
                                         10))
      (json:encode-json-plist-to-string
       (list "sEcho" s-echo
             "iTotalRecords" (total-row-count grid)
             "iTotalDisplayRecords" data-length
             "aaData" (or data #()))))))

(defclass grid-edit-form (widget)
  ((object :initarg :object
           :initform nil
           :accessor object)
   (validation-list :initarg :validation-list
                    :initform nil
                    :accessor validation-list)
   (error-message :initform nil
                  :accessor error-message)
   (save-disabled-p :initarg :save-disabled-p
                    :initform nil
                    :accessor save-disabled-p)))

(defun finish-editing (grid)
  (when (edit-form grid)
    (setf (error-message (edit-form grid)) nil))
  (setf (editing-row grid) nil))

(defmethod render ((form grid-edit-form) &key body)
  (let ((name (widgy-name form "edit-form")))
    (with-html

      (:form :class "validate"
             :action ""
             :method "post"
             :id name
             :name name
             (:div :class "content no-padding"
                   (str body)
                   (when (error-message form)
                     (htm
                      (:div :class "section _100 edit-form-error"
                            (str (error-message form))))))
             (:div :class "actions"
                   (:div :class "actions-left"
                         (:input :type "submit"
                                 :name "action"
                                 :disabled (save-disabled-p form)
                                 :onclick (format nil "javascript:return submitForm(\"~A\",~A)"
                                                  name
                                                  (build-validation-array
                                                   (validation-list form)))
                                 :value "Save"))
                   (:div :class "actions-right"
                         (:input :type "submit" :name "action" :value "Cancel")))
             (:div :id "section _100 legend-div" :style "display:none;"
                   (:table  :style "text-align:center;margin:auto;"
                            (:tr (:td :colspan 4 "Validation Error Legend"))
                            (:tr
                             (:td :style "font-weight:bold;background-color:#FF0000;color:#FFFFFF;" "Required Field")
                             (:td :style "font-weight:bold;background-color:#FFCC00" "Email Incorrect")
                             (:td :style "font-weight:bold;background-color:#FFFF00" "Date Format \"dd MMM yyyy\"")
                             (:td :style "font-weight:bold;background-color:pink;" "Numeric Field"))))))))

(define-condition validation-error (error)
  ((error-message :initarg :message
                  :initform nil
                  :accessor error-message))
  (:report
   (lambda (c stream)
     (format stream "Validation Error~@[: ~a~]" (error-message c)))))

(defgeneric export-csv (widget))

(defmethod export-csv ((grid grid))
  (let* ((data (coerce (grid-filtered-rows grid) 'list))
        (slots (if data
                   (class-slots (class-of (elt data 0))))))
    (when data
      (with-output-to-string (stream)
        (dolist (slot slots)
          (format stream "~A|" (slot-definition-name slot)))
        
        (dolist (doc data)
          (format stream "~%")
          (dolist (slot slots)
            (format stream "~A|" (get-val doc (slot-definition-name slot)))))))))

;;;

(defgeneric list-grid-filters (grid))
(defgeneric apply-grid-filter (grid data filter))

(defmethod list-grid-filters (grid))

(defmethod apply-grid-filter (grid data filter)
  data)

(defclass grid-filter-selector (ajax-widget)
  ((grid :initarg :grid
         :initform nil
         :accessor grid)))

(defmethod render ((widget grid-filter-selector) &key)
  (let* ((grid (grid widget))
         (param (get-parameter "filter"))
         (custom-filter (and param
                             (find-symbol (string-upcase param)
                                          :keyword)))
         (defined-filters (list-grid-filters (grid widget)))
         (filters (if custom-filter
                      (cons custom-filter defined-filters)
                      defined-filters)))
    (when (and (not *in-ajax-request*)
               (not custom-filter)
               (filter-parameters grid))
      (setf (grid-filter grid) nil
            (filter-parameters grid) nil))
    (with-html
      (:div :class "grid-filter"
            (when filters
              (let ((select (make-widget 'select
                                         :name (sub-name widget "filter-select")
                                         :first-item "Clear Filter")))
                (setf (on-change select)
                      (js-render grid (js-value select)))
                (when custom-filter
                  (setf (grid-filter grid) custom-filter
                        (filter-parameters grid) (get-parameters*))
                  ;; a hack so that subsequent grids don't get confused
                  (setf (slot-value *request* 'get-parameters)
                        (remove "filter" (get-parameters*)
                                :test #'equal
                                :key #'car)))
                (cond ((parameter (name select))
                       (setf (grid-filter grid)
                             (find (value select) filters
                                   :test #'string-equal)))
                      (t
                       (setf (value select) (if custom-filter
                                                (string-upcase custom-filter)
                                                "Clear Filter")
                             (items select)
                             (mapcar #'string-capitalize filters))))
                (js-render widget (js-value select))
                (render select)))))))
