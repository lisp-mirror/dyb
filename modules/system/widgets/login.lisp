(in-package #:ems)

(defclass login (widget)
  ((email :initarg :email
          :accessor email)
   (password :initarg :password
             :accessor password)
   (message :initarg :message
            :accessor message)
   (css-class :initarg :css-class
          :initform nil
          :accessor css-class))
  (:metaclass widget-class)
  (:action-params submit-login))

(defgeneric validate-user (login))
(defgeneric on-success (login &key))
(defgeneric on-failure (login &key))

(defmethod handle-action ((widget login) (action (eql 'submit-login)))
  (when (and (get-val widget 'email) (get-val widget 'password))
    (let ((user (validate-user widget)))
      (if user
          (on-success widget :user user)
          (on-failure widget)))))

(defmethod render ((widget login) &key)
  (with-html-output-to-string (*standard-output*)
      (:form :method "post"
             :action ""
             :class (if (get-val widget 'css-class)
                        (get-val widget 'css-class))
             (:table 
                 (:tr
                  (:td 
                   (:label :for "email" "Email"))
                  (:td 
                   (:input :type "text" :name (widgy-name widget "email") :id "email"
                           :style "width:250px;"
                           :value (get-val widget 'email))))
               (:tr
                (:td
                 (:label :for "password" "Password"))
                (:td
                 (:input :type "password" :name (widgy-name widget "password") 
                         :id "password"
                         :style "width:250px;"))))
             (:br)
             (:p 
              (:input :name (widgy-name widget "submit-login") 
                      :type "submit" :value "Login")))
      (if (get-val widget 'message)
          (htm
           (:div :class "error" (esc (get-val widget 'message)))))))





