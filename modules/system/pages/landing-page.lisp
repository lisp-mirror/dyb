(in-package :ems)

(defun check-password (user password)
  (equalp
   (password user)
   (hash-password password (salt user))))

(defmethod validate-user ((widget login))
  (let ((user (get-user (slot-val widget 'email))))
    (unless (and user (check-password user (slot-val widget 'password)))
      (setf user nil)
      (setf (slot-value widget 'message) "Email or password incorrect"))
    user))

(defmethod on-success ((login login) &key user)
  (when (session-value 'user)
    (hunchentoot:remove-session *session*))
  (setf (session-value 'user) user)
          	    
  (log-login "Login" (get-val login 'email) "Passed" "Login passed.")
  (set-cookie "expanded" :value "" :path "/")

  (redirect "/ems/dashboard"))

(defmethod on-failure ((login login) &key)
  (log-login "Login" (get-val login 'email) "Failed" "User name or password incorrect."))

(defclass landing-page (widget)
  ()
   (:metaclass widget-class)
   (:include-css "/css/ems/site_style.css"))

(defmethod render ((page landing-page) &key)
  (let ((peach-page (make-widget 'special-peach-page
                                 :name (widgy-name page "special-landing-page"))))
    (setf (slot-value peach-page 'header) "Login")
    (setf (slot-value peach-page 'author) "DATA X-WARE; info@dataxware.co.za")
    (setf (slot-value peach-page 'key-words) "Social and Labour plan")

    (render
     peach-page
     :body
     (render-to-string (make-widget 'login :name "login")))))

(define-easy-handler (login :uri "/ems/login"
                            :for-everyone t) ()
  (render (make-widget 'landing-page :name "special-login-page")))
