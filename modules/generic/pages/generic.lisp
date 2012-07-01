(in-package #:ems)

#|
(defun parse-profile (json-list)
  (if (listp json-list)
      (let ((stream ""))
        (dolist (parm json-list)
          (setf stream (concatenate 'string stream
                                    (format nil "~A : ~A~%" (car parm) (cdr parm)))))
        stream))
  
  )

(defun parse-picture (json-list)

  (if (listp json-list)
      (let ((stream ""))
        (dolist (parm json-list)
          
          (when (string-equal (car parm) "picture")
            
              (setf stream (concatenate 'string stream
                                        (cdr parm)))))
        stream)))

|#
#|
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
                         (:input :type :submit :name "post" :value "Posts")))
               )

           
    
              )))))
|#

(define-easy-handler (generic-page :uri "/ems/generic") ()
  (let* ((columns
           (list
            (make-instance 'grid-column
                           :name 'pid
                           :header "ID")
            (make-instance 'grid-column
                           :name 'title
                           :header "Title")
            (make-instance 'grid-column
                           :name 'type
                           :header "Type")
	    (make-instance 'grid-column
                           :name 'created
                           :header "Created")
 ))
         (grid (make-widget 'generic-grid :name "generic-grid0"
                                       :columns columns
                                       :edit-inline nil
                                       :title "Facebook Inbox"
                                       :row-object-class 'generic-entry)))
    
    (render (make-widget 'page :name "generic-page")
            :body (render-to-string grid)))


  
  )
