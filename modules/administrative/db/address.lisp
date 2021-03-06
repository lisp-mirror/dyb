(in-package :dyb)

(defclass address (doc)
  ((address-type :initarg :address-type 
                 :initform nil)
   (address-line :initarg :address-line 
                 :initform nil
                 :accessor address-line) 
   (country-town :initarg :country-town 
                 :initform nil
                 :accessor country-town))
  (:metaclass storable-versioned-class))

(defun make-address (address-type 
                     address-line 
                     country-town)
  (make-instance 'address 
                 :address-type address-type
                 :address-line address-line                 
                 :country-town country-town))
