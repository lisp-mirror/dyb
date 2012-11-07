(in-package :dyb)

(define-easy-handler (home-page :uri "/dyb/home") ()

  (let ((page (make-widget 'ems-page :name "home" )))
    (with-html-output-to-string (*standard-output*)
      (render page

              :body 
              (with-html-output-to-string (*standard-output*)
                (:form :name "user-form" :id  "user-form" :method "post" :action ""
                       (:ol :style "list-style: none;"
                        (:li
                         (:label :form "user-form" :for "service"
                                 (str "Service"))
                         (:input :id "service" :name "service" 
                                 :placeholder "Twitter"
                                 :required "required" :autofocus "autofocus"))
                        (:li
                         (:label :form "user-form" :for "email"
                                 (str "Email"))
                         (:input :id "email" :name "service" :type "email"
                                 :required "required" ))
                        )))
              ))))


(define-easy-handler (preview :uri "/dyb/preview") ()
  (setf (session-value 'current-client) "DX")

  (let ((page (make-widget 'ems-page :name "preview"))
        (user (if (parameter "email") 
                  (get-twitter-user    (current-client) (parameter "email")))))

    (when (parameter "submit")

      (unless user

        (make-channel-user  (current-client) 'twitter (parameter "email") (parameter "nick"))
        (setf user (get-twitter-user (current-client) (parameter "email")))
        )
      (when user
;(break "~A" )
        (unless (and (get-val user 'identifier) (not (equal (get-val user 'identifier) -1)))
          
          (setf (get-val user 'identifier) (get-twit-user-id (get-val user 'nick))))

        (persist user)
        (when (and (get-val user 'identifier) (not (equal (get-val user 'identifier) -1)))

          (unless (and (get-val user 'access-token) (not (equal (get-val user 'access-token) "")))
              (redirect "/dyb/twitauth")
              )
          (persist user))))
    
    (with-html-output (*standard-output*)
      
      (render page
              :body 
              (with-html-output-to-string (*standard-output*)

                (let ((box (make-widget 'html-framework-form :name "channel-user-form"))
                      (form-section (make-widget 'form-section :name "row")))
                  (setf (header box) "Twitter User")
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

