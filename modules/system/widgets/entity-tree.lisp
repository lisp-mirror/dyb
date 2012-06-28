(in-package :ems)

(defclass entity-tree (tree)
  ())

(defclass entity-tree-node (tree-node)
  ())


(defmethod render-tree-node ((tree entity-tree) &key data leaf-p)
  ;;(break "~A ~A ~A" data (listp data) (first data) )
  (let ((node-data (if (listp data)
                       (first data)
                       data)))
    (let ((node (make-widget 'entity-tree-node :name (entity-name (slot-value node-data 'entity)))))
      (setf (slot-value node 'leaf-p) leaf-p)
      (setf (wfx:data node) node-data)
      (setf (slot-value node 'caption) (entity-name (slot-value node-data 'entity)))
      (render node))))

(defmethod map-tree-data ((tree entity-tree) tree-data)
  (with-html
    (labels ((rec (relationship)
               (htm 
                (:li :style "list-style-type:none;"
                     (render-tree-node tree 
                                       :data relationship 
                                       :leaf-p t)
                     (if (children (if (listp relationship)
                                       (first relationship)
                                       relationship))
                         (htm
                          (:ul (dolist (par (children (if (listp relationship)
                                                          (first relationship)
                                                          relationship)))
                                 (rec par)))))
                 
                     
                     ))))
    (rec tree-data))))
