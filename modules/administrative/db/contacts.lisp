(in-package :dyb)

(defclass contact ()
  ((contact-name :initarg :contact-name 
                 :initform nil
                 :accessor contact-name)
   (contact-type :initarg :contact-type 
                 :initform nil) 
   (telephone-number :initarg :telephone-number 
                     :initform nil
                     :accessor telephone-number)
   (facsimile-number :initarg :facsimile-number 
                     :initform nil)
   (email-address :initarg :email-address 
                  :initform nil))
  (:metaclass storable-class))

(defun make-contact (contact-name
                     contact-type                    
                     telephone-number
                     facsimile-number              
                     email-address)
  (make-instance 'contact 
                 :contact-name contact-name
                 :contact-type contact-type                                 
                 :telephone-number telephone-number
                 :facsimile-number facsimile-number
                 :email-address email-address))

