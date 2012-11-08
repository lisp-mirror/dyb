(in-package :dyb)

(defclass generic-grid (grid)
  ()
  (:metaclass widget-class)
  (:include-css "/appcss/posts.css")
  (:default-initargs :edit-inline nil))

(defclass grid-action (ajax-widget)
  ((grid :initarg :grid
         :initform nil
         :accessor grid))
  (:metaclass widget-class))

(defmethod render ((widget grid-action) &key )
  (with-html
    "Grid Action")
  (open-dialog widget (grid widget)))

(defmethod handle-action ((grid generic-grid) (action (eql 'block-user)))
  (setf (action-widget grid)
        (make-widget 'grid-action :grid grid :name "XXX")))

(defun get-generic-data (grid &key filter search)
  (declare (ignore grid search))
  (find-docs 'vector
              (lambda (doc)
             ;;   (if (match-context-entities (get-val doc 'payload) ))
                (cond ((equal filter 'with-audit-data)
                           doc)
                          (t 
                           (if (not (string-equal (get-val doc 'doc-status) "superseded"))
                               doc))))
              (generic-post-collection)))

(defmethod get-rows ((grid generic-grid))
  (setf (rows grid)
	(get-generic-data grid 
                          :filter (grid-filter grid)  
                          :search (search-term grid))))

(defclass grid-action (ajax-widget)
  ((grid :initarg :grid
         :initform nil
         :accessor grid))
  (:metaclass widget-class))

(defmethod render ((widget grid-action) &key )
  (with-html
    "Grid Action")
  (open-dialog widget (grid widget)))

(defmethod handle-action ((grid generic-grid) (action (eql 'block-user)))
  (setf (action-widget grid)
        (make-widget 'grid-action :grid grid :name "XXX")))







