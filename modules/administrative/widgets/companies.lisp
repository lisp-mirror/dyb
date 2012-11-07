(in-package :dyb)

(defclass companies-grid (grid)
  ()
  (:default-initargs :edit-inline nil))

(defmethod get-rows ((grid companies-grid))
  (setf (rows grid)
	(loop for companies across (companies)
              collect companies)))
