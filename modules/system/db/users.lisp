(in-package :ems)

(defclass user ()
  ((identifier :initarg :identifier
           :accessor identifier)
   (client :initarg :client
           :accessor client)
   (email :initarg :email
          :accessor email))
  ;(:metaclass storable-class)
  )

(defgeneric persist (user))

(defclass system-user (user document)
  ((name :initarg :name
          :accessor name)
   (surname :initarg :surname
          :accessor surname)
   (password :initarg :password
             :accessor password)
   (salt :initarg :salt
         :accessor salt)))

(defmethod persist ((user system-user))
  (setf (doc-type user) 'system-user)
  (store-doc (users) user))

(defun make-system-user (client email name surname password)
  (let ((user (make-instance 
                  'system-user
                  :key '(client email 'system-user) 
                  :type 'system-user
                  :client client                
                  :email email
                  :name name
                  :surname surname
                  :password password
                  :salt "")))
    (persist user)))

(defmethod validate-password-p ((user system-user) password)
  (equalp (password user) password))

(defun get-system-user (client email)
  (find-doc (users)
            :test
            (lambda (doc)
              (if (equal (type-of doc) 'system-user)
                  (and (equal (client doc) client) 
                   (equal (email doc) email))))))

(defclass service-user (user document)
  ((service :initarg :service
            :accessor service)
   (nick :initarg :nick
          :accessor nick)
   
   (access-token :initarg :access-token
                  :initform nil
                  :accessor access-token)))

(defmethod persist ((user service-user))
    (setf (doc-type user) 'service-user)

  (store-doc (users) user))


(defun make-service-user (client service email nick)
  (let ((user))
    (cond ((equal service 'twitter)
           (setf user
                 (make-instance 
                  'service-user
                  :type 'service-user
                  :client client
                  :key '(client email 'service-user)
                  :identifier (get-twit-user-id nick)
                  :service service
                  :email email
                  :nick nick
                  ))
    
           (persist user)))))

(defun get-service-user (client email service)
  (find-doc (users)
   :test
   (lambda (doc)
     (if (equal (doc-type doc) 'service-user)
         (and 
          (equal (service doc) service)
          (equal (client doc) client)
          (equal (email doc) email))))))




