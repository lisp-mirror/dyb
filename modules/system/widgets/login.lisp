(in-package :dyb)

(defclass login (widget)
  ((email :initarg :email
          :initform nil
          :accessor email)
   (password :initarg :password
             :initform nil
             :accessor password)
   (message :initarg :message
            :initform nil
            :accessor message))
  (:metaclass widget-class)
  (:action-params submit-login))

(defgeneric validate-user (login))
(defgeneric on-success (login &key))
(defgeneric on-failure (login &key))

(defmethod handle-action ((widget login) (action (eql 'submit-login)))
  (when (and (email widget) (password widget))
    (let ((user (validate-user widget)))
      (if user
          (on-success widget :user user)
          (on-failure widget)))))

(defmethod render ((widget login) &key)
  (with-html
    (:div :class "box"
          (box-header "Login" :icon "lock"
                              :class "header grey")
          (:form :method "post"
                 :action ""               
                 (:div :class "content no-padding"
                       (:div :class "section _100"
                             (:label :for "email" "Email")
                             (:div
                              (:input :class "required" :type "text" 
                                      :name (widgy-name widget "email") 
                                      :id "email"
                                      :value (email widget))))
                       (:div :class "section _100"
                             (:label :for "password" "Password")
                             (:div
                              (:input :class "required" :type "password" 
                                      :name (widgy-name widget "password") 
                                      :id "password"))))
                 (:div :class "actions"
                       (:div :class "actions-right"
                             (:input :name (widgy-name widget "submit-login") 
                                     :type "submit" :value "Login"))))
          (when (message widget)
            (htm
             (:div :class "error" (esc (message widget))))))
    (defer-js "$('#email').focus()")))






