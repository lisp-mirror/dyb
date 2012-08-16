(in-package #:ems)


(define-easy-handler (ems :uri "/ems/proof") ()
  (with-html-output-to-string (*standard-output*)

    (let ((profile ))
      

      (cond ((parameter "profile")
             (setf profile (json::decode-json-from-string 
                            (drakma::http-request   
                              (format nil "https://graph.facebook.com/~A" 
                                      (if (parameter "token")
                                      (parameter "token")
                                      129436193802362))))))
            ((parameter "posts")
             (setf profile (json::decode-json-from-string 
                             (drakma::http-request 
                              (format nil "https://graph.facebook.com/~A/posts/" 
                                      (if (parameter "token")
                                      (parameter "token")
                                      129436193802362)))))))
      
      (htm
       (:form :action "" :method "post" :name "proof-form"
           
              (:table 
               (:tr
                (:td "Id:"
                     (:select :name "token"
                          (:option :value 100003313046336 "Phil" )
                          (:option :value 129436193802362 "Data X-Ware" )
                          (:option :value 1009173975 "Haji" )))
                (:td 
                 

                 ;;(:input :type "text" :name "token" :value (if (parameter "token")
                 ;;                                                  (parameter "token")
                 ;;                                                  129436193802362))
                 ))
               (:tr (:td  
                         (:textarea :cols "100" :rows "20"
                                    (if (parameter "profile")
                                        (str (parse-profile profile)))))
                    (:td (:img :src (parse-picture profile))
                         ))
               (:tr (:td :colspan 2
                         (:input :type :submit :name "profile" :value "Profile")
                         (:input :type :submit :name "post" :value "Posts")))))))))


(defun update-facebook-posts-for-users (grid)
  
  (dolist (user (find-docs 'list 
                           (lambda (doc)
                             (match-context-entities doc))
                            (service-users-collection)))
        
        (when (and user (string-equal (get-val user 'doc-status) "Active"))
          ;;TODO: How to get error messages in for users without access tokens.
          (when (get-val user 'last-access-token)
            (multiple-value-bind (bodyx)
                (drakma:http-request 
                 (format nil "https://graph.facebook.com/~A/feed?access_token=~A" 
                         (url-encode (get-val user 'user-id))
                         (get-val user 'last-access-token)))

              (let ((post-list (rest (first (json::decode-json-from-string bodyx)))))

                (if (populate-generic-db-from-post post-list )
                    (if (string-equal (car (car post-list)) "MESSAGE")
                  
                        (when grid 
                    
                          (setf (error-message grid)
                                (format nil (error-message grid) "~A~%~A%"
                                        (error-message grid)
                                        (car (cdr post-list))) ) ))))
              )))))

(define-easy-handler (generic-page :uri "/ems/generic") ()
  (let* ((columns
           (list
            (make-instance 'grid-column
                           :name 'payload
                           :header "From"
                           :printer (lambda (doc)
                                     (if doc
                                         (if (get-val doc 'from)
                                             (if (get-val (get-val doc 'from) 'name)
                                                 (get-val (get-val doc 'from) 'name)
                                                 0)
                                             0)
                                         0)))
            (make-instance 'grid-column
                           :name 'title
                           :header "Title")
            (make-instance 'grid-column
                           :name 'type
                           :header "Type")
           (make-instance 'grid-column
                           :name 'interaction
                           :header "Response")
           (make-instance 'grid-column
                          :name 'payload
                          :header "Likes"
                          :printer (lambda (doc)
                                     (if doc
                                         (if (get-val doc 'likes)
                                             (if (get-val (get-val doc 'likes) 'count)
                                                 (get-val (get-val doc 'likes) 'count)
                                                 0)
                                             0)
                                         0)))
           (make-instance 'grid-column
                          :name 'payload
                          :header "Comments"
                          :printer (lambda (doc)
                                     (if doc
                                         (if (get-val doc 'comments)
                                             (if (get-val (get-val doc 'comments) 'count)
                                                 (get-val (get-val doc 'comments) 'count)
                                                 0)
                                             0)
                                         0)))
           (make-instance 'grid-column
                          :name 'created
                          :header "Created")
           ))
         (grid (make-widget 'generic-grid :name "generic-post-gridXx"
                                       :columns columns
                                       :edit-inline nil
                                       :title "Facebook Inbox"
                                       :row-object-class 'generic-entry)))
    
    (when (parameter "get-facebook-data")
      (update-facebook-posts-for-users grid))
    
    (render (make-widget 'page :name "generic-page")
            :body (with-html-to-string ()
                    (:form :name "fetch-data"
                           :method :post
                           (:input :type "submit" :name "get-facebook-data" 
                                   :value "Get Facebook Data"))
                    (str (render grid)))))


  
  )
