(in-package :ems)

(defclass address ()
  ((address-type :initarg :address-type 
                 :initform nil)
   (address-line :initarg :address-line 
                 :initform nil) 
   (country :initarg :country 
            :initform nil)
   (province :initarg :province 
             :initform nil)
   (town :initarg :town 
         :initform nil))
  (:metaclass storable-class))

(defun make-address (address-type 
                     address-line 
                     country
                     province              
                     town)
  (make-instance 'address 
                ; :key address-type
                 :address-type address-type
                 :address-line address-line                 
                 :country country
                 :province province
                 :town town))