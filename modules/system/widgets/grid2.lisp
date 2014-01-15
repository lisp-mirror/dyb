(in-package :dyb)

(defclass dyb-grid (grid)
  ((grid-links
    :initarg :grid-links
    :initform nil
    :accessor grid-links))
  (:metaclass widget-class)
  (:default-initargs :modal-edit t
                     :bottom-buttons nil))

(register-widget *dyb-theme* 'grid 'dyb-grid)

(defmethod handle-action ((grid dyb-grid) (action (eql :delete)))
  (when (editable grid)
    (remove-doc (editing-row grid) (email (current-user)))))

;;;

(defmethod init-data-table ((grid dyb-grid))
  (when (toolbar-widget grid)
    (defer-js (format nil "$('#~a .grid-widget').html(~s)"
                      (name grid)
                      (substitute #\Space #\Newline (render-to-string (toolbar-widget grid))))))
  (defer-js
      (format nil "$('#~a').bind('page',function (){window.scrollTo(0, $('#~a').offset().top);})
.dataTable({
'bProcessing': true,
'bServerSide': true,
'sPaginationType': 'full_numbers',
'bServerSide': true,
'iDisplayLength': 10,
'oLanguage': {
            'sLengthMenu': \"<span class='lenghtMenu'> _MENU_</span><span class='lengthLabel'>Entries per page:</span>\",
        },
'sAjaxSource': '/dyb/ajax/TABLE?script-name=~a&id=~a',
'sDom': '<\"tbl-searchbox clearfix\"f~:[~;@<\"grid-widget\">~]lr,<\"clear\">>,<\"table_content\"t>,<\"widget-bottom\"ip<\"clear\">>',

'aoColumnDefs': [{'bSortable': false, 'aTargets': [~{~a~^,~}]}],
~:[~;'aaSorting': [[~a,~s]],~]
'fnServerData': function(sSource,aoData,fnCallback) {
    $.getJSON(sSource, aoData, function(json) {
        if($.isArray(json)) eval(json[1]);
        else fnCallback(json);
    });
}})"
              (sub-name grid "table")
              (name grid)
              (script-name*)
              (name grid)
              (toolbar-widget grid)
              (if (editable grid)
                  (cons (length (columns grid))
                        (not-sorting-columns grid))
                  (not-sorting-columns grid))
              (not (equal (initial-sort-column grid)
                          '(0 :ascending)))
              (car (initial-sort-column grid))
              (if (eq (cadr (initial-sort-column grid))
                      :ascending)
                  "asc"
                  "desc"))))

(defclass dyb-grid-filter-selector (grid-filter-selector)
  ()
  (:default-initargs :css-class "grid-filter"))

(register-widget *dyb-theme* 'grid-filter-selector 'dyb-grid-filter-selector)

(defmethod render ((widget dyb-grid-filter-selector) &key)
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
      (:div :class "widget-control grid-filter"
            (when filters
              (htm
               (:label "Filter by:")
               (let ((select (make-widget 'select
                                          :name (sub-name widget "filter-select")
                                          :first-item "Select All")))
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
                                                 "Select All")
                              (items select)
                              (mapcar #'string-capitalize filters))))
                 (js-render widget (js-value select))
                 (render select))))
            (add-actions-menu grid)))))

;;;

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

(defmethod render-row-actions ((grid dyb-grid) row row-id)
  (with-html
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
                                  (get-val row 'post-type))
                           (htm 
                            (:li (render-ajax-buttonx 
                                  grid row-id 
                                  (second link)
                                  (third link))))))
                     (dolist (link (get-val grid 'grid-links))
                       (htm (:li (render-ajax-buttonx 
                                  grid row-id 
                                  (first link)
                                  (second link)))))))
               (unless (get-val grid 'grid-links)
                 (htm
                  (:li
                   (render-ajax-edit-button grid row-id))
                  (:li
                   (render-ajax-delete-button grid row-id))))))))

;;;

(defgeneric list-grid-actions (grid))

(defun drop-down-menu (items)
  (with-html
    (:div :class "btn-group"
          :style "display:inline-block"
          (:button :data-toggle "dropdown" 
                   :class "btn dropdown-toggle" 
                   (:i :class "icon-cog"
                       (:span :class "caret")))
          (:ul :class "dropdown-menu pull-right"
               (loop for (title (kind link)) in items
                     do
                     (htm (:li
                           (ecase kind
                             (:js
                              (htm (:a :href (js-link link)
                                       (esc title))))
                             (:link
                              (htm (:a :target "_blank"
                                       :href link (esc title))))))))))))

(defmethod list-grid-actions (grid)
  `(("Export CSV"
     (:link ,(frmt "/dyb/export-csv?grid=~a&script-name=~a)"
                   (escape (name grid))
                   (escape (script-name*)))))
    ("Add"
     (:js ,(js-render (editor grid)
                      (js-pair "grid-name" (name grid))
                      (js-pair "action" "new"))))))

(defun add-actions-menu (grid)
  (drop-down-menu (list-grid-actions grid)))

