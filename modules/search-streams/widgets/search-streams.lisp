(in-package #:ems)


(defclass search-streams-grid (grid)
  ((parent-grid :initarg :parent-grid)
   (current-doc :initarg nil))
  (:default-initargs :edit-inline nil))


(defun get-search-streams-data (grid &key filter search)
  (declare (ignore grid search))
  (find-docs 'vector
              (lambda (doc)
             ;;   (if (match-context-entities (get-val doc 'payload) ))
                (cond ((equal filter 'with-audit-data)
                           doc)
                          (t 
                           (if (not (string-equal (get-val doc 'doc-status) "superseded"))
                               doc))))
              (search-streams-collection)))

(defmethod get-rows ((grid search-streams-grid))
  (setf (rows grid)
	(get-search-streams-data grid 
                          :filter (grid-filter grid)  
                          :search (search-term grid))))