(in-package :dyb)

(defun %intern (value)
  (intern (format nil "~:@(~a~)" value)
          #.*package*))

;;; That's a strange name choice
(defun blank-p (value)
  (and value
       (not (equal value ""))))

