(in-package :dyb)

(defclass node-data ()
  ((id :initarg :id :initform nil)
   (value :initarg :value :initform nil)))

(defclass tree (ajax-widget)
  ((action :initarg :action :initform nil :accessor action)
   (selected-nodes :initarg :seleted-nodes :initform nil
                   :accessor selected-nodes)
   (css-class :initarg :css-class
              :initform nil
              :accessor css-class)))

(defclass tree-node (widget)
  ((caption :initarg :caption :initform nil)
   (leaf-p :initarg :leap-p :initform nil)))

(defmethod render ((tree-node tree-node) &key)
  (with-html
    ;(when (slot-value tree-node 'leaf-p)
    ;          (str "-"))
    (str (slot-value tree-node 'caption))
    ))

(defgeneric render-tree-node (tree &key data leaf-p))

(defmethod render-tree-node ((tree tree) &key data leaf-p)
  (let ((node (make-widget 'tree-node :name (slot-value data 'id))))
    (setf (slot-value node 'leaf-p) leaf-p)
    (setf (wfx:data node) data)
    (render node)))

(defgeneric handle-tree-action (tree))

(defmethod handle-tree-action ((tree tree)))

(defgeneric map-tree-data (tree tree-data))

(defmethod map-tree-data ((tree tree) tree-data)
  (with-html
    (dolist (item tree-data)
      (htm 
       (:li :style "list-style-type:none;"
            (render-tree-node tree :data 
                              (if (listp item)
                                  (first item)
                                  item) 
                              :leaf-p t) 
                
            (when (listp item)
              (when (rest item) 
                (htm
                 (:ul
                  (map-tree-data tree (rest item)))))))))))

(defmethod render ((tree tree) &key)
  (with-html
    (:div :class (css-class tree)
     (:ul
          (when (action tree)
            (handle-tree-action tree))
          (unless (action tree)
            (map-tree-data tree (wfx::data tree)))))))
