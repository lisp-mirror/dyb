(in-package :dyb)

(defclass permission-grid (grid)
  ()
  (:default-initargs :edit-inline nil
                     :row-object-class 'permissions))

(defclass permission-editor (grid-edit-form ajax-widget)
  ((grid :initarg :grid
         :initform nil
         :accessor grid)
   (cb-list :initarg :cb-list
            :initform nil
            :accessor cb-list)
   (permissions :initarg :permissions
                :initform nil
                :accessor permissions)))

(defmethod get-rows ((grid permission-grid))
  (setf (rows grid) (docs (permissions-collection))))

(defmethod handle-action ((grid permission-grid) (action (eql 'save)))
  (setf (permission-list (editing-row grid))
        (mapcar #'list (value (cb-list (edit-form grid)))))
  (persist (editing-row grid))
  (finish-editing grid))

(defmethod render ((widget permission-editor) &key)
  (let* ((permissions (permissions widget))
         (box (make-widget 'html-framework-box))
         (cb-list (make-widget 'checkbox-list
                               :name "permission-list"))
         (grid (grid widget)))
    (unless (parameter "permission-list")
      (setf (items cb-list)
            (permissions-options (permission-list permissions))))
    (setf (cb-list widget) cb-list)
    (setf (content box)
          (with-html-string
            (:form :action ""
                   :id "permissions-form"
                   :method "post"
                   :onsubmit "return false;"
                   (:div :class "section _100"
                         (:label "Name")
                         (:div
                          (:input :type "text"
                                  :name "name"
                                  :value (name permissions))))
                   (:div
                    (render cb-list))
                   (:div :class "actions"
                         (:div :class "actions-left"
                               (:button :onclick
                                        (js-render-form-values (editor grid)
                                                               "permissions-form"
                                                               (js-pair "grid-name" (name grid))
                                                               (js-pair "action" "save")
                                                               (scroll-to grid))
                                        "Save"))
                         (:div :class "actions-right"
                               (:button :class "red"
                                        :onclick
                                        (js-render (editor grid)
                                                   (js-pair "grid-name" (name grid))
                                                   (js-pair "action" "cancel")
                                                   (scroll-to grid))
                                        "Cancel"))))))
    (render box)))

(defun permissions-options (selected-permissions)
  (loop for (name . types) in *permissions*
        for (selected . selected-types) = (find name selected-permissions
                                                      :test #'equal :key #'car)
        collect
        (list* name name
               (and selected t)
               (loop for type in types
                     collect (list type type
                                   (and selected
                                        (find type selected-types :test #'equal)
                                        t))))))

(defmethod render-row-editor ((grid permission-grid) row)
  (let ((editor (make-widget 'permission-editor
                             :grid grid)))
    (setf (edit-form grid) editor)
    (setf (permissions editor) row
          (wfx:data editor) (list row))
    (render editor)))

(define-easy-handler (permissions-page :uri "/dyb/permissions") ()
  (let ((columns (list (make-instance 'grid-column :name 'name))))
    (render (make-widget 'page :name "permissions"
                               :title "Permissions")
            :body
            (with-html-to-string ()
              (render (make-widget 'permission-grid :columns columns))))))
