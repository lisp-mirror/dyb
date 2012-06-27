(in-package :ems)

(defclass address-grid (grid)
  ()
  (:default-initargs :edit-inline nil))

(defmethod get-rows ((grid address-grid))
  (setf (rows grid)
	(loop for address across (address)
              collect address)))

