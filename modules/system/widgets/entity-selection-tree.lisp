(in-package :dyb)

(defclass entity-selection-tree (entity-tree)
  ())

(defclass entity-selection-tree-node (tree-node)
  ())

(defmethod render ((tree-node entity-selection-tree-node)
                   &key tree)
  (let* ((entity (entity (car (wfx:data tree-node))))
         (checkbox (make-widget
                    'checkbox
                    :name (format nil "entity-~A" (xid entity))
                    :description (entity-name entity))))
    ;;Keep selected nodes in sync with checkbox values
    (cond ((not (value checkbox))
           (setf (selected-nodes tree) (remove tree-node (selected-nodes tree))))
          ((not (find tree-node (selected-nodes tree)))
           (setf (selected-nodes tree) (push tree-node (selected-nodes tree)))))
    (render checkbox)))

(defmethod render-tree-node ((tree entity-selection-tree) &key data leaf-p)
  (let ((node (make-widget 'entity-selection-tree-node
                           :name (xid (entity data)))))
    (setf (slot-value node 'leaf-p) leaf-p)
    (setf (wfx:data node) (list data))
    (render node :tree tree)))

(defmethod render ((tree entity-selection-tree) &key)
  (with-html
    (:form
     :action ""
     :id (widgy-name tree "select-tree-form")
     (:div :class "entity-tree"
           (:ul
            (if (action tree)
                (handle-tree-action tree)
                (map-tree-data tree (car (wfx::data tree))))))
     (:input :type "button"
             :value "Set"
             :onclick (js-render-form-values
                       tree
                       (widgy-name tree "select-tree-form")
                       (js-pair "action" "set")))
     (when (equal (parameter "action") "set")
       (setf (last-context (current-user))
             (loop for node in (selected-nodes tree)
                   collect (xid (entity (car (wfx:data node))))))
       (setf (context) (last-context (current-user)))
       (persist (current-user))))))

(defclass entity-edit-tree (entity-selection-tree)
  ((selected-entities :initarg :selected-entities
                      :initform nil
                      :accessor selected-entities)
   (checkboxes :initarg :checkboxes
               :initform nil
               :accessor checkboxes)))

(defclass entity-edit-tree-node (tree-node)
  ())

(defmethod render-tree-node ((tree entity-edit-tree) &key data leaf-p)
  (let ((node (make-widget 'entity-edit-tree-node
                           :name (xid (entity data)))))
    (setf (slot-value node 'leaf-p) leaf-p)
    (setf (wfx:data node) (list data))
    (render node :tree tree)))

(defmethod render ((tree-node entity-edit-tree-node)
                   &key tree)
  (let* ((entity (entity (car (wfx:data tree-node))))
         (xid (xid entity))
         (checkbox (make-widget
                    'checkbox
                    :name (format nil "entity-~A" xid)
                    :description (entity-name entity))))
    (setf (value checkbox)
          (and (find xid (selected-entities tree)) t))
    (unless (member xid (checkboxes tree) :key #'car)
      (push (cons xid checkbox) (checkboxes tree)))
    (render checkbox)))

(defmethod render ((tree entity-edit-tree) &key)
  (with-html
    (:div :class "entity-tree"
          (:ul
           (if (action tree)
               (handle-tree-action tree)
               (map-tree-data tree (car (wfx:data tree))))))))

(defmethod action-handler ((tree entity-edit-tree))
  (setf (selected-entities tree)
        (loop for (xid . checkbox) in (checkboxes tree)
              when (value checkbox)
              collect xid)))
