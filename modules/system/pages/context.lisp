(in-package :dyb)

(defclass context (ajax-widget)
  ((root :initarg :root
         :initform nil
         :accessor root)))

(defmethod render ((widget context) &key)
  (let ((root (make-widget 'select :name "root-select"))
        (entities
          (make-widget 'entity-selection-tree))
        (user (current-user))
        (box (make-widget 'html-framework-box)))

    (setf (on-change root)
          (js-render widget (js-value root)))
    
    (setf (items root)
          (if (super-user-p user)
              (loop for entity-rel across (entity-relationships)
                    for entity = (entity entity-rel)
                    when (string-equal (slot-value entity-rel 'doc-status) "active")
                    collect (list (xid entity)
                                  (entity-name entity)))
              (loop for (root) in (list (accessible-entities user))
                 collect (list root
                               (entity-name (get-entity-by-id root))))))

    (cond ((parameter "root-select"))
          ((not (super-user-p user))
           
           ;;TODO: Why is this set crashing with no method for ...
;           (setf (selected-entities entities) (cadr (accessible-entities user)))

           (setf (value root)
                 (car (accessible-entities user))))
          (t
           (setf (value root)
                 (item-value (car (items root))))))
    (let* ((root-id (ensure-parse-integer (value root)))
           (root-entity (get-root root-id)))
      (setf (root widget) root-id)
      (setf (content box)
       (with-html-string
         (:form :id (widgy-name entities "select-tree-form")
                :action ""
                :method "post"
                (with-html
                  (:div :class "section _100"
                        (:label "Root")
                        (:div
                         (render root)))
                  (:div :class "section _100"
                        (:label "Entities")
                        (:div
                         (when root-entity
                           (setf (wfx:data entities) (list root-entity))
                           (render entities))))))))
      (render box))))

(define-easy-handler (context-page :uri "/dyb/context") ()
  (render (make-widget 'page :title "Context")
          :body
          (render-to-string (make-widget 'context))))
