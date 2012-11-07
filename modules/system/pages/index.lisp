(in-package #:ems)



;;TODO: Replace hard coded client
(defun current-client ()
  (session-value 'current-client))

(defmethod validate-user ((login login))
  (let ((user (get-system-user (current-client) (get-val login 'email))))
    (unless (and user (validate-password-p user (get-val login 'password)))
      (setf user nil)
      (setf (slot-value login 'message) "Email or password incorrect"))
    user))

(defmethod on-success ((login login) &key user)

  (let ((redirect (session-value 'redirect-after-login)))
    
            (when (session-value 'user)
              (hunchentoot:remove-session *session*))
	    (setf (session-value 'user) user)
            (delete-session-value 'redirect-after-login)	    
          ;;  (log-login "Login" (get-val login 'email) "Passed" "Login passed.")
            (set-cookie "expanded" :value "" :path "/")

	    (redirect (or redirect
			  "/dyb/home"))))

(defmethod on-failure ((login login) &key)
;;  (log-login "Login" (get-val login 'email) "Failed" "User name or password incorrect.")
)


(define-easy-handler (landing-page :uri "/dyb/index") ()
  (let ((page (make-widget 'page :name "index-page"))
        (login (make-widget 'login :name "login-ems")))

    
    (setf (session-value 'current-client) "DX")

    (with-html-output-to-string (*standard-output*)
      (render page
              :header (with-html-output-to-string (*standard-output*)
                        (str "Header goes here"))
              :body (with-html-output-to-string (*standard-output*)
                   
                      (str (render login)))
              :footer (with-html-output-to-string (*standard-output*)
                        (str "Footer goes here"))
              ))))