(in-package :dyb)

(defclass companies-grid (grid)
  ())

(defmethod get-rows ((grid companies-grid))
  (setf (rows grid)
	(loop for companies across (companies)
              collect companies)))
