(in-package :ems)

(defclass entities-edit-tree (entity-tree)
  ())

(defclass entities-edit-tree-node (entity-tree-node)
  ())

(defmethod action-handler ((tree entities-edit-tree-node))
  )
(defmethod action-handler ((tree entities-edit-tree))
  
  (when (or (parameter "add-entity-click")  (parameter "delete-entity-click"))
      (let ((rel
             (get-relationship-tree-item-by-id
              (car (wfx:data tree)) 
              (parse-integer (parameter "add-entity-click") :junk-allowed t))))

        (setf (selected-nodes tree) rel))
      ))

(defmethod render ((tree-node entities-edit-tree-node)
                   &key tree)
 (with-html
    (:div :style "display:table-cell; vertical-align:middle"
          (:label (str (slot-value tree-node 'caption))))
    (:div :style "display:table-cell; vertical-align:middle;"
          (:form :id (format nil "add-entity-~A" (get-val (wfx:data tree-node) 'xid))  :method "post"
                 (:input :type "hidden" :name "add-entity-click" 
                         :value (get-val (wfx:data tree-node) 'xid))
                 (:img :src "/images/add-ex-small.png" 
                       :onclick (format nil "javascript:document.getElementById(\"add-entity-~A\").submit();" (get-val (wfx:data tree-node) 'xid)))))
    (:div :style "display:table-cell; vertical-align:middle"
          (:form :id (format nil "delete-entity-~A" (get-val (wfx:data tree-node) 'xid)) :method "post"
                 (:input :type "hidden" :name "delete-entity-click" 
                         :value (get-val (wfx:data tree-node) 'xid))
                 (:img :src "/images/delete-ex-small.png" 
                       :onclick (format nil "javascript:document.getElementById(\"delete-entity-~A\").submit();" (get-val (wfx:data tree-node) 'xid))))) ))

(defmethod render-tree-node ((tree entities-edit-tree) &key data leaf-p)
  (let ((node-data (if (listp data)
                       (first data)
                       data)))
    (let ((node (make-widget 'entities-edit-tree-node 
                             :name (entity-name (slot-value node-data 'entity)))))
      (setf (slot-value node 'leaf-p) leaf-p)
      (setf (wfx:data node) node-data)
      (setf (slot-value node 'caption) (entity-name (slot-value node-data 'entity)))
      (render node))))

(defmethod map-tree-data ((tree entities-edit-tree) tree-data)
  (with-html
    (labels ((rec (relationship)
               (htm 
                (:li :style "list-style-type:none;"
                     (render-tree-node tree 
                                       :data relationship 
                                       :leaf-p t)
                     (if (children relationship)
                         (htm
                          (:ul (dolist (par (children relationship))
                                 (rec par)))))))))
    (rec tree-data))))

(defmethod render ((tree entities-edit-tree) &key)
  (with-html
    (:div :class "entity-tree"
          (:ul
           (if (action tree)
               (handle-tree-action tree)
               (map-tree-data tree (car (wfx:data tree))))))))

(defclass entities (ajax-widget)
  ((root :initarg :root
          :initform nil
          :accessor root)
   (tree :initarg :tree
         :initform :tree
         :accessor tree)))

(defmethod action-handler ((widget entities))
  
  (when (parameter "add-entity")
    (let ((root (copy (car (wfx:data (tree widget)))))
          (entity (if (parameter "entity")
                      (get-entity (parameter "entity")))))

      (setf (get-val root 'xdb2::id) (next-id (entity-relationships-collection)))
      (setf (get-val root 'xdb2::written) nil)
      (setf (get-val root 'doc-status) "Active")

      (when (and (parameter "new-entity") 
                 (not (string-equal (parameter "new-entity") "")))
        (setf entity (make-entity 
                      (get-val (get-entity-type-by-id 
                                (parse-integer (parameter "entity-type-select"))) 
                               'entity-type-name)
                      (parameter "new-entity"))))

      (persist-doc entity)
      
      (add-relationship-child (if (get-val (selected-nodes (tree widget)) 'parent)
                                   (selected-nodes (tree widget))
                                   root) 
                              (make-entity-relationship    
                               root 
                               (if (get-val (selected-nodes (tree widget)) 'parent)
                                   (selected-nodes (tree widget))
                                   root) 
                               entity
                               nil))

      (persist-doc root)))
  
  (when (parameter "delete-entity-click")
    (remove-relationship-tree-child
     (car (wfx:data (tree widget)))
     (selected-nodes (tree widget)))
    )
)

(defmethod render ((widget entities) &key)
  (let ((root (make-widget 'select :name "root-select"))
        (tree (make-widget 'entities-edit-tree :name "entities-edit-tree"))
        (user (current-user))
        (box (make-widget 'peach-box)))
    
    (setf (on-change root)
          (js-render widget (js-value root)))

    (setf (items root)
          (if (super-user-p user)
              (loop for entity-rel across (entity-relationships)
                    for entity = (entity entity-rel)
                    when (string-equal (slot-value entity-rel 'doc-status) "active")
                    collect (list (xid entity)
                                  (entity-name entity)))
              (loop for (root) in (accessible-entities user)
                    collect (list root
                                  (entity-name (get-entity-by-id root))))))
    (setf (tree widget) tree)
    (let* ((root-id (ensure-parse-integer (value root)))
                   (root-entity (get-root root-id)))

      (setf (root widget) root-id)
      
      (setf (content box)
            (with-html-string
              (:div :class "section _100"
                    (:label "Root")
                    (:div
                     (render root)))
              (:div :class "section _100"
                    (:label "Entities")
                    (:div
                     (when root-entity
                       
                       (setf (wfx:data tree) (list root-entity))
                       (render tree))))
              (when (parameter "add-entity-click")
                (let ((entity-select (make-widget 'select :name "entity-select"))
                      (entity-type-select (make-widget 'select :name "entity-type-select"))
                      (edit-box (make-widget 'peach-box :name "edit-box")))

                  (setf (items entity-select) (entity-list))
                  (setf (items entity-type-select) (entity-type-list))
                  (setf (value entity-select) nil)
                  (setf (content edit-box)
                        (with-html-string
                            (:form :id "add-entity" :method "post"
                                   (:input :type "hidden" :name "parent-entity"
                                           :value (if (selected-nodes tree)
                                                      (entity-name 
                                                       (entity (selected-nodes tree)))))
                                   (str "Add an existing entity")

                                   (render entity-select)

                                   (:br)
                                   (str "OR")
                                   (:br)
                                   (:br)

                                   (str "Create a new entity.")

                                   (:input :type "text" :name "new-entity" 
                                           :value "")

                                   (render entity-type-select)
                                   
                                   (:input :type "submit" :name "add-entity" 
                                           :value "Add"))))
                  (htm (:div :class "section _100"                              
                             (render edit-box)))))))
      (render box))))

(define-easy-handler (entities-page :uri "/ems/entities") ()
  (render (make-widget 'page :name "all-users"
                         :title "Entities")
            :body
            (render-to-string (make-widget 'entities))

            ))