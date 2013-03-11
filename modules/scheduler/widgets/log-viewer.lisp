(in-package :dyb)

(defclass error-log-grid (grid)
  ())

(defmethod get-rows ((grid error-log-grid))
  (setf (rows grid) (error-logs)))

(define-easy-handler (error-log-viewer :uri "/dyb/error-log") ()
  (let* ((columns
           (list
            (make-instance 'grid-column
                           :name 'task-name)
            (make-instance 'grid-column
                           :name 'printed-condition
                           :header "Error")
            (make-instance 'grid-column
                           :name 'stamp-date
                           :header "Date"
                           :printer #'format-universal-date-time)))
         (grid (make-widget 'error-log-grid
                            :columns columns
                            :title "Error Log"
                            :editable nil)))
    (setf (get-val grid 'columns) columns)
    (setf (sort-keys grid) '(2 stamp-date))
    (setf (initial-sort-column grid) '(2 :descending))
    (render (make-widget 'page :name "error-log")
            :body (render-to-string grid))))
