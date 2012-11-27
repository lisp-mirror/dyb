(in-package :dyb)

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
    :initarg :rows-per-page
    :initform 10
    :accessor rows-per-page)
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
   (sort-key-function :initarg :sort-key-function
                :initform nil
                :accessor sort-key-function)
   (sort-keys :initarg :sort-keys
              :initform nil
              :accessor sort-keys
              :documentation
              "A plist of (column-number key)")
   (initial-sort-column :initarg :initial-sort-column
                        :initform '(0 :ascending)
                        :accessor initial-sort-column
                        :documentation "(column direction)") 
   (grid-filter :initarg :grid-filter
                :initform nil
                :accessor grid-filter)
   (filter-parameters :initarg :filter-parameters
                      :initform nil
                      :accessor filter-parameters)
   (state :initarg :state
          :initform nil
          :accessor state)
   (parent-grid :initarg :parent-grid
                :initform nil
                :accessor parent-grid)
   (css-span :initarg :css-span
          :initform nil
          :accessor css-span)
   (action-widget :initarg :action-widget
                  :initform nil
                  :accessor action-widget)))

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
   (special-printer :initarg :special-printer
            :initform nil
            :accessor special-printer)
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
      (if (blank-p parameters)
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
      (setf (editing-row grid)
            (elt (rows grid) row-id)))))

(defmethod handle-action :around ((grid grid) action)
  (when *logging*
   (format *debug-io* "~a ~a~%" action grid))
  (when (next-method-p)
    (setf (error-message grid) nil)
    (set-current-row grid)
    (when (edit-form grid)
      (setf (error-message (edit-form grid)) nil))
    (call-next-method)))

(defmethod handle-action ((grid grid) (action (eql 'delete)))
  (when (editable grid)
    (remove-doc (editing-row grid)))
  (finish-editing grid)
  (update-table grid))

(defmethod handle-action ((grid grid) (action (eql 'init-new)))
  (editing-row grid))

(defmethod handle-action ((grid grid) (action (eql 'new)))
  (when (editable grid) 
    (when (row-object-class grid)
      ;;TODO: Do we need to add to rows??
        (let ((new-instance (make-instance (row-object-class grid))))
         ; (if (arrayp (rows grid))
         ;     (vector-push-extend new-instance (rows grid))
         ;     (push new-instance (rows grid)))
          (setf (editing-row grid) new-instance))
        (handle-action grid 'init-new))
    
    (unless (row-object-class grid)
      ;;TODO: Do we need to add to rows??
      (let* ((rows (rows grid))
             (first-row (and (plusp (length rows))
                             (elt rows 0)))
             (object-class
              (or (row-object-class grid)
                  (and first-row
                       (class-of first-row)))))
        (let ((new-instance (make-instance object-class)))
       ;   (push new-instance (rows grid))
          (setf (editing-row grid) new-instance))
        ))))

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

(defmethod handle-action ((grid grid) (action (eql 'edit)))
  (editing-row grid))

(defmethod handle-action ((grid grid) (action (eql 'cancel)))
  (when (edit-form grid)
    (setf (object (edit-form grid)) nil)
    (setf (edit-form grid) nil))
  (finish-editing grid))

(defmethod handle-action ((grid grid) (action (eql 'edit-all)))
  (setf (editing-row grid) :all))

(defmethod action-handler ((grid grid))
  (when (equal (name grid) (parameter "grid-name"))
    (handle-action grid
                   (find-symbol (string-upcase (parameter "action"))
                                #.*package*))))

(defun render-grid-header (widget)
  (with-html
    (:thead
     (:tr
      (dolist (column (columns widget))
        (htm (:th :style (if (get-val column 'width)
                             (format nil "width:~A;" (get-val column 'width))) 
              (esc (header column)))))
      (when (editable widget)
        (htm (:th)))))))


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

(defgeneric render-ajax-button (grid row-id action title))

(defmethod render-ajax-button ((grid grid) row-id action title)
  (with-html (:a :href
                 (js-link 
                  (js-render (editor grid)
                             (js-pair "grid-name" (name grid))
                             (js-pair "action" action)
                             (js-pair "row_id" row-id)))

                 title)))

(defmethod render-ajax-buttonx (grid row-id action title)
  (with-html (:a :href
                 (js-link 
                  (js-render (editor grid)
                             (js-pair "grid-name" (name grid))
                             (js-pair "action" action)
                             (js-pair "row_id" row-id)))

                 (str title))))

(defun render-ajax-edit-button (grid row-id)
  (with-html
    
    (:a :href
        (js-link
         (js-render (editor grid)
                    (js-pair "grid-name" (name grid))
                    (js-pair "action" "edit")
                    (js-pair "row_id" row-id)))
        ;(make-icon "card--pencil"
        ;      :title "Edit")
        "Edit")))

(defun render-ajax-delete-button (grid row-id)
  (with-html
    (:a :href
        (js-link (format nil
                         "if(confirm(\"Delete document?\")){~a}"
                         (js-render (editor grid)
                                    (js-pair "grid-name" (name grid))
                                    (js-pair "action" "delete")
                                    (js-pair "row_id" row-id))))
        ;;;(make-icon "card--minus"
        ;;:title "Delete")
        "Delete"
    )))

(defun render-grid-rows (grid &key editing)
  (with-html
    (:table
        :id (sub-name grid "table")
      :class "data-tbl-boxy table" ;;"table"
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
              (:input :type "image" :src "/appimgsave.png")))))))

(defun scroll-to (widget)
  (format nil
          "$(\"html, body\").animate({scrollTop: $(\"#~a\").offset().top})"
          (if (typep widget 'widget)
              (name widget)
              widget)))

(defgeneric render-row-editor (grid row))

(defmethod render-row-editor ((grid grid) row)
  (let ((row-id (position row (rows grid)))
        (columns (columns grid))
        (form (make-instance 'html-framework-form :name "row-editor-generic" 
                             :grid-size 9
                             :header "Edit" :form-id "generic-edit-form"
                             :grid-name (name grid)))
        (form-section (make-widget 'form-section
                                   :name "form-section")))
    (with-html
      (render form 
              :grid grid
              :content
              (with-html-to-string ()
                (loop for column in columns
                   for column-id from 0
                   for name = (slot-val row (name column))
                   for text = (if (printer column)
                                  (funcall (printer column) name)
                                  name)
                   for widget-name = (format nil "cell~d:~d" row-id column-id)
                   do
                   (render form-section
                           :label (string-capitalize (name column))
                            :input
                            (with-html-to-string ()
                              (render-edit-field widget-name  
                                                 text
                                                 :type (editor-type column)
                                                 ))))))
      )))

(defclass grid-editor (ajax-widget)
  ((grid :initarg :grid
         :initform nil
         :accessor grid))
  (:metaclass widget-class))

(defun ancestor-grids (grid)
  (loop for parent = (parent-grid grid) then (parent-grid parent)
        while parent
        collect parent))

(defun update-table (grid)
  (loop for parent in (ancestor-grids grid)
     do
     (defer-js (fmt "updateTable('~a-table')" (name parent))))
  (defer-js (fmt "updateTable('~a-table')" (name grid))))

(defun open-dialog (widget grid &key (width 900) (height 500))
  (defer-js (format nil "$('#~a').dialog({width: ~A, height: ~A,~@
                                           close: function(){~a}})"
                    (name widget)
                    width height
                    (js-render (editor grid)
                               (js-pair "grid-name" (name grid))
                               (js-pair "action" "cancel")))))

(defmethod render ((editor grid-editor) &key (modal-p t))
  (let* ((grid (grid editor))
         (editing-row (editing-row grid)))
    (cond ((and (not (edit-inline grid))
                editing-row)
           (with-html
             (when (error-message grid)
               (htm
                (:div :class "edit-form-error"
                      (esc (error-message grid)))))
             (if (action-widget grid)
                 (render (action-widget grid))
                 (render-row-editor grid editing-row))
             (cond (modal-p
                    (open-dialog editor grid))
                   (t
                    (defer-js (scroll-to editor)))))
           (setf (state grid) :editing))
          ((eql (state grid) :editing)
           (unless (equal (parameter "action") "cancel")
             (update-table grid))
           (if modal-p
               (defer-js (format nil "$('#~a').dialog('close')"
                                 (name editor)))
               (defer-js (scroll-to editor)))
           (setf (state grid) nil)))))

(defun widget-head (text &key icon (icon-size 16)
                              (class "widget-head")
                              collapsible
                              body)
  (with-html
    (:div :class class
          ;; (when icon
          ;;   (make-icon icon
          ;;              :size icon-size))
          (:h5 (esc text))
          ;; (when collapsible
          ;;   (htm (:div :class "collapse")))
          (str body))))

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
      (:div :class (format nil "span~A widget-block" (if (get-val grid 'css-span)
                                                         (get-val grid 'css-span)
                                                         11))
            (widget-head (title grid)
                         :icon "table-excel"
                         :collapsible t
                         :body (with-html-string
                                 (:div :class "widget-control"
                                       (render filter))
                                 (:div :class "widget-control pull-left"
                                       (add-actions-menu grid))))
            (:div :class "widget-content"
                  (:div :class "widget-box"
                        (render-grid-rows grid :editing editing-row)))
            (:div :class "widget-box"
                  (:table
                      :style "text-align:center"
              
                    #|(:tr
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
                                  ;;(make-icon "plus" :size 32
                                  ;;                  :title "Add"
                                   ;;                 )
                                  "Add")))
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
                                          :image "/appimgedit.png"
                                          :task "edit-all")))))))))))
                    |#
                    (:tr
                     (:td
                      (when (error-message grid)
                        (htm
                         (:div :class "edit-form-error"
                               (esc (error-message grid))))))))
                  (str footer))
            (render editor))
      (defer-js
          (format nil "$('#~a').dataTable({
'bProcessing': true,
'bServerSide': true,
'sPaginationType': 'full_numbers',
'bServerSide': true,
'iDisplayLength': 10,
'oLanguage': {
            'sLengthMenu': \"<span class='lenghtMenu'> _MENU_</span><span class='lengthLabel'>Entries per page:</span>\",
        },
'sAjaxSource': '/dyb/ajax/TABLE?script-name=~a&id=~a',
'sDom': '<\"tbl-searchbox clearfix\"flr,<\"clear\">>,<\"table_content\"t>,<\"widget-bottom\"ip<\"clear\">>',

~:[~;'aoColumnDefs': [{'bSortable': false, 'aTargets': [~a]}],~]
~:[~;'aaSorting': [[~a,~s]],~]
'fnServerData': function(sSource,aoData,fnCallback) {
    $.getJSON(sSource, aoData, function(json) {
        if($.isArray(json)) eval(json[1]);
        else fnCallback(json);
    });
},
})"
                  (sub-name grid "table")
                  (script-name*)
                  (name grid)
                  (editable grid)
                  (length (columns grid))
                  (not (equal (initial-sort-column grid)
                              '(0 :ascending)))
                  (car (initial-sort-column grid))
                  (if (eq (cadr (initial-sort-column grid))
                          :ascending)
                      "asc"
                      "desc"))))))

(defun column-text (grid row column &key row-id)
  (let ((value (slot-val row (name column))))
    (if (special-printer column)
        (funcall (special-printer column) grid row value row-id )
        (if (printer column)
            (funcall (printer column) value)
            (princ-to-string value)))))

(defun expand-columns (grid row row-id)
  (loop for column in (columns grid)
        collect (column-text grid row column :row-id row-id)))

(defun search-columns (grid row)
  (loop with term = (search-term grid)
        for column in (columns grid)
        thereis (search term (column-text grid row column )
                        :test #'equalp)))

(defun expand-all-columns (grid rows current-index)

  (loop for row being the elements of rows
     for row-id = (if row-id  (incf row-id) (+ current-index  0))
     collect (expand-columns grid row row-id)))

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
     (loop for row in (expand-all-columns grid cut start) 
           for id from start
           collect 
          (append row
                  (list (with-html-string
                          (:div :class "btn-group pull-right"
                                (:button :data-toggle "dropdown" 
                                         :class "btn dropdown-toggle" 
                                         (:i :class "icon-cog"
                                             (:span :class "caret")))
                                (:ul :class "dropdown-menu"
                                     (when (get-val grid 'grid-links)
                                       (if (typep grid 'generic-grid)
                                           (dolist (link (get-val grid 'grid-links))
                                             (if (equal (first link) 
                                                        (get-val (elt cut (- id start)) 
                                                                 'post-type))
                                                 (htm 
                                                  (:li (render-ajax-buttonx 
                                                        grid id 
                                                        (second link)
                                                        (third link))))))
                                           (dolist (link (get-val grid 'grid-links))
                                             (htm (:li (render-ajax-buttonx 
                                                        grid id 
                                                        (first link)
                                                        (second link)))))))
                                     (unless (get-val grid 'grid-links)
                                       (htm
                                        (:li
                                         (render-ajax-edit-button grid id))
                                        (:li
                                         (render-ajax-delete-button grid id))))
                                     ))))))
     data-length)))

(defun comparison> (x y)
  (typecase x
    (string
     (string> x y))
    (number
     (> x y))
    (t
     (string> (princ-to-string x)
              (princ-to-string y)))))

(defun comparison< (x y)
  (typecase x
    (string
     (string< x y))
    (number
     (< x y))
    (t
     (string< (princ-to-string x)
              (princ-to-string y)))))

(defun sort-direction-function (grid)
  (if (eq (sort-direction grid) :ascending)
      #'comparison<
      #'comparison>))

(defun sort-data (grid rows)
  (let* ((column-number (sort-column grid))
         (column (elt (columns grid) column-number))
         (key (getf (sort-keys grid) column-number)))
    (sort rows (sort-direction-function grid)
          :key
          (cond (key)
                ((and (zerop column-number)
                      (sort-key-function grid)))
                (t
                 (lambda (row) (column-text grid row column)))))))

(defmethod process-data-table ((grid grid))
  ;(print (get-parameters*))
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
  (setf (error-message grid) nil)
  (when (edit-form grid)
    (setf (error-message (edit-form grid)) nil))
  (setf (editing-row grid) nil)
  (setf (action-widget grid) nil))

(defmethod render ((form grid-edit-form) &key body)
  (let ((name (widgy-name form "edit-form")))
    (with-html

      (:form :class "validate"
             :action ""
             :method "post"
             :id name
             :name name
             (:div :class "content no-padding"
                   (str body))
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
             (when (error-message form)
                     (htm
                      (:div :class "section _100 edit-form-error"
                            (str (error-message form)))))))))

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

;;;

(defgeneric list-grid-actions (grid))

(defun drop-down-menu (items)
  (with-html
    (:div :class "btn-group pull-right"
          (:button :data-toggle "dropdown" 
                   :class "btn dropdown-toggle" 
                   (:i :class "icon-cog"
                       (:span :class "caret")))
          (:ul :class "dropdown-menu"
               (loop for (title js) in items
                     do
                     (htm (:li (:a :href (js-link js)
                                   (esc title)))))))))
(defmethod list-grid-actions (grid)
  `(("Export CSV"
     ,(format nil "window.open(\"/dyb/export-csv?grid=~a&script-name=~a\")"
              (name grid)
              (script-name*)))
    ("Add"
     ,(js-render (editor grid)
                                                      (js-pair "grid-name" (name grid))
                                                      (js-pair "action" "new")))))

(defun add-actions-menu (grid)
  (drop-down-menu (list-grid-actions grid)))
