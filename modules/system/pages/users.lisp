(in-package :dyb)

(defclass user-grid (grid)
  ())

(defclass user-editor (widget)
  ((grid :initarg :grid
         :initform nil
         :accessor grid)
   (user :initarg :user
         :initform nil
         :accessor user
         :accessor object)
   (entity-editor :initform nil
                  :accessor entity-editor)
   (error-message :initform nil
                  :accessor error-message)
   (permission-editor :initform nil
                      :accessor permission-editor)))

(defmethod get-rows ((grid user-grid))
  (setf (rows grid)
	(if (super-user-p (current-user))
            (users)
            (find-users (lambda (doc)
                          (match-entities
                           doc
                           (get-val (current-user)
                                    'accessible-entities)))))))

(defmethod handle-action ((grid user-grid) (action (eql :new)))
  (setf (editing-row grid) t))

(defmethod handle-action ((grid user-grid) (action (eql :save)))
  (let* ((editor (edit-form grid))
         (user (user editor))
         (permission-editor (permission-editor editor)))
    (setf (error-message editor) nil)
    (flet ((fail (message)
             (setf (error-message editor) message)
             (return-from handle-action)))
      (with-parameters (email password repeat action)
        (when (equal password "")
          (setf password nil))
        (cond ((not (or (equalp action "save")
                        (equalp action "add")))
               (return-from handle-action))
              ((and password repeat
                    (not (equal password repeat)))
               (fail "Passwords don't match"))
              ((< 0 (length password) *min-passwrod-length*)
               (fail (format nil "Password should be ~a characters minimum"
                             *min-passwrod-length*)))
              ((and user)
               (change-user user password))
              ((not email))
              ((equal (trim-whitespace email) "")
               (fail "Email can't be empty"))
              ((not password)
               (fail "Password can't be empty"))
              ((get-user email)
               (fail "User with such email already exists"))
              (email
               (setf user (make-user email password))))))
    (setf (permissions user) (value permission-editor)
          (accessible-entities user)
          (list (root (entity-editor editor))
                (selected-entities (entities-select (entity-editor editor)))))
    (persist user))
  (finish-editing grid))

(defmethod render ((editor user-editor) &key)
  (let* ((user (user editor))
         (form-section (make-widget 'form-section
                                    :name "form-section")))
    (with-html
      (when (error-message editor)
        (htm
         (:div :class "edit-form-error"
               (str (error-message editor)))))
      (render form-section
              :label "E-mail"
              :input (with-html-string
                       (render-edit-field "email" (or (parameter "email")
                                                      (and user (email user)))
                                          :required t)))
      (render form-section
              :label "Password"
              :input (with-html-string
                       (render-edit-field "password" (parameter "password")
                                          :type :password)))
      (render form-section
              :label "Repeat password"
              :input (with-html-string
                       (render-edit-field "repeat" (parameter "repeat")
                                          :type :password))))))

(defmethod render-row-editor ((grid user-grid) row)
  (let* ((editor (make-widget 'user-editor
                              :name "user-editor"
                              :grid grid))
         (permissions (make-widget 'user-permission-editor))
         (entitites (make-widget 'user-entity-editor))
         (tabs (make-instance 'html-framework-tab-box
                              :name "user-edit"
                              :header "Users"
                              :icon "card--pencil")))
    (setf (edit-form grid) editor
          (user editor) (unless (eq row t) row)
          (entity-editor editor) entitites)
    (setf (tabs tabs)
          `(("User" ,(render-to-string editor))
            ("Entities" ,(render-to-string entitites :editor editor))
            ("Permissions" ,(render-to-string permissions :editor editor))))
    (setf (body-content tabs)
          (with-html-string
            (:div :class "form-actions"
                  (:button
                   :class "btn btn-info"
                   :onclick
                   (js-render-form-values (editor grid)
                                          "user-editor-form"
                                          (js-pair "grid-name" (name grid))
                                          (js-pair "action" "save")
                                          (scroll-to grid))
                   "Save")
                  (:button :class "btn btn-warning"
                           :onclick
                           (format nil "event.preventDefault(); ~a"
                                   (js-render (editor grid)
                                            (js-pair "grid-name" (name grid))
                                            (js-pair "action" "cancel")
                                            (scroll-to grid)))
                           "Cancel"))))
    (with-html
      (:div
       :class "grid_6"
       (:form
        :class "validate"
        :method "post"
        :onsubmit "return false;"
        :id "user-editor-form"
        (:input :type "hidden" :name "form-id" :value "user-editor-form")
        (render tabs))))))

(defclass user-permission-editor (ajax-widget)
  ((value :initarg :value
          :initform nil
          :accessor value)
   (cb-list :initarg :cb-list
            :initform nil
            :accessor cb-list)))

(defmethod render ((widget user-permission-editor) &key editor)
  (let ((all-permissions (docs (permissions-collection)))
        (select (make-widget 'select :blank-allowed nil
                                     :name "permission-selector"))
        (cb-list (make-widget 'checkbox-list
                              :name "permission-list"))
        (user (and editor
                   (user editor))))
    (when editor
      (setf (permission-editor editor) widget
            (cb-list widget) cb-list))
    (setf (on-change select)
          (js-render widget (js-value select)
                     (js-pair "permission-editor-template" "")))
    (cond ((parameter "permission-editor-template")
           (let ((permission (find (value select)
                                   all-permissions
                                   :key #'name :test #'equal)))
             (and permission
              (setf (items cb-list)
                    (permissions-options (permission-list permission))))))
          ((parameter "permission-selector"))
          (user
           (setf (first-item select) "Select")
           (setf (items select)
                 (map 'list #'name all-permissions))
           (setf (items cb-list)
                 (permissions-options (permissions user))))
          (t
           (setf (first-item select) "Select")))
    (with-html
      (:div :class "section _100"
            (:label "Template")
            (:div
             (render select)))
      (:div :class "section _100"
            (render cb-list)))))

(defmethod action-handler ((widget user-permission-editor))
  (setf (value widget)
        (mapcar #'list (value (cb-list widget)))))

(defclass user-entity-editor (ajax-widget)
  ((root :initform nil
         :accessor root)
   (entities-select :initarg :entities-select
                    :initform nil
                    :accessor entities-select)
   (user :initform nil
         :accessor user)))

(defmethod render ((widget user-entity-editor) &key editor)
  (let* ((user (if editor
                   (setf (user widget) (user editor))
                   (user widget)))
         (entities
           (make-widget 'entity-edit-tree
                        :name (format
                               nil
                               "entities~d"
                               (or (and user
                                        (xid user))
                                   0))))
         (root-select (make-widget 'select :name "root-select")))
    (setf (on-change root-select)
          (js-render widget (js-value root-select)))
    (setf (items root-select)
          (loop for entity-rel across (entity-relationships)
                for entity = (entity entity-rel)
                when (equal (slot-value entity-rel 'doc-status) "active")
                collect (list (xid entity)
                              (entity-name entity))))
    (setf (entities-select widget) entities)
    (cond ((parameter "root-select"))
          (user
           (setf (selected-entities entities) (cadr (accessible-entities user)))
           (setf (value root-select)
                 (car (accessible-entities user))))
          (t
           (setf (value root-select)
                 (item-value (car (items root-select))))))
    (let* ((root-id (ensure-parse-integer (value root-select)))
           (root-entity (get-root root-id)))
      (setf (root widget) root-id)
      (with-html
        (:div :class "section _100"
              (:label "Root")
              (:div
               (render root-select)))
        (:div :class "section _100"
              (:label "Entities")
              (:div
               (when root-entity
                 (setf (wfx:data entities) (list root-entity))
                 (render entities))))))))

(define-easy-handler (users-page :uri "/dyb/users") ()
  (let ((columns (list (make-instance 'grid-column :name 'email))))
    (render (make-widget 'page :name "all-users"
                               :title "Users")
            :body
            (render-to-string
             (make-widget 'user-grid :name "user-grid" :columns columns)))))
