(in-package :ems)


(define-easy-handler (system-users-page :uri "/ems/system-users") ()
  (setf (session-value 'current-client) "DX")

  (let ((page (make-widget 'ems-page :name "systems-users-page"))
        (user (if (parameter "email") 
                  (get-system-user (current-client) (parameter "email")))))

    (when (parameter "submit")

      (unless user

        (make-system-user  (current-client) (parameter "email") (parameter "name") (parameter "surname")
                           (parameter "password"))
        (setf user (get-system-user (current-client) (parameter "email")))
        )
      )
    
    (with-html-output (*standard-output*)
      
      (render page
              :body 
              (with-html-output-to-string (*standard-output*)

                (let ((box (make-widget 'peach-form :name "system-user-form"))
                      (form-section (make-widget 'form-section :name "row")))
                  (setf (header box) "System User")
                  (str (render box
                               :content
                               (with-html-output-to-string (*standard-output*)
                                ; (str (post-parameters*))
                                 
                                 (str (render form-section
                                              :label "Email"
                                              :input 
                                              (with-html-output-to-string (*standard-output*) 
                                                (:input :class "required" :id "email" :name "email" :type "email"
                                                            :value (or (parameter "email") (get-val user 'email))))))
                                 (str (render form-section
                                              :label "Name"
                                              :input 
                                              (with-html-output-to-string (*standard-output*) 
                                                (:input :class "required" :id "name" :name "name"
                                                           :value (or (parameter "name") (get-val user 'name))))))
                                 (str (render form-section
                                              :label "Surname"
                                              :input 
                                              (with-html-output-to-string (*standard-output*) 
                                                (:input :class "required" :id "surname" :name "surname"
                                                           :value (or (parameter "surname") (get-val user 'surname))))))
                                 (str (render form-section
                                              :label "Password"
                                              :input 
                                              (with-html-output-to-string (*standard-output*) 
                                                (:input :class "required" :id "password" :name "password"
                                                           :value (or (parameter "password") (get-val user 'surname)))))))))))))))


(define-easy-handler (service-users-page :uri "/ems/service-users") ()
  (setf (session-value 'current-client) "DX")

  (let ((page (make-widget 'ems-page :name "service-users-page"))
        (user (if (parameter "email") 
                  (get-twitter-user    (current-client) (parameter "email")))))

    (when (parameter "submit")

      (unless user

        (make-service-user  (current-client) 'twitter (parameter "email") (parameter "nick"))
        (setf user (get-twitter-user (current-client) (parameter "email")))
        )
      (when user
;(break "~A" )
        (unless (and (get-val user 'identifier) (not (equal (get-val user 'identifier) -1)))
          
          (setf (get-val user 'identifier) (get-twit-user-id (get-val user 'nick))))

        (persist user)
        (when (and (get-val user 'identifier) (not (equal (get-val user 'identifier) -1)))

          (unless (and (get-val user 'access-token) (not (equal (get-val user 'access-token) "")))
              (redirect "/ems/twitauth")
              )
          (persist user))))
    
    (with-html-output (*standard-output*)
      
      (render page
              :body 
              (with-html-output-to-string (*standard-output*)

                (let ((box (make-widget 'peach-form :name "service-user-form"))
                      (form-section (make-widget 'form-section :name "row")))
                  (setf (header box) "User")
                  (str (render box
                               :content
                               (with-html-output-to-string (*standard-output*)
                                ; (str (post-parameters*))
                                 (str (render form-section
                                              :label "Service"
                                              :input 
                                              (with-html-output-to-string (*standard-output*)
                                                (:input :class "required" :id "service" :name "service" 
                                                           :placeholder "Twitter"
                                                           :autofocus "autofocus"
                                                           :value (or (parameter "service") (get-val user 'service)) ))))
                                 (str (render form-section
                                              :label "Email"
                                              :input 
                                              (with-html-output-to-string (*standard-output*) 
                                                (:input :class "required" :id "email" :name "email" :type "email"
                                                            :value (or (parameter "email") (get-val user 'email))))))
                                 (str (render form-section
                                              :label "Nick"
                                              :input 
                                              (with-html-output-to-string (*standard-output*) 
                                                (:input :class "required" :id "nick" :name "nick"
                                                           :value (or (parameter "nick") (get-val user 'nick))))))
                                 (str (render form-section
                                              :label "ID"
                                              :input 
                                              (with-html-output-to-string (*standard-output*) 
                                                (:input  :id "identifier" :name "identifier" :readonly "readonly" 
                                                           :value (get-val user 'identifier)))))
                                 (str (render form-section
                                              :label "Token"
                                              :input 
                                              (with-html-output-to-string (*standard-output*) 
                                                (:input  :id "token" :name "access-token"  :readonly "readonly"
                                                        :value (or (parameter "access-token") (get-val user 'acces-token))
                                                           ))))
                                )))
                  )
                )
              ))))




