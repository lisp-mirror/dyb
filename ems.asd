;;;; ems.asd

(asdf:defsystem #:ems
  :serial t
  :depends-on (#:hunchentoot
               #:cl-who
               #:drakma
               #:cl-json
               #:xdb2
               #:cl-oauth
               #:wfx)
  :components ((:file "package")
               (:file "ini")
               (:file "ems")

               (:file "data")
               (:module "modules"
                        :serial t
                        :components
                        ((:file "common")
                         (:module "system"
                                  :serial t
                                  :components
                                  
                                  (;(:file "common")
                                   
                                   (:module "db"
                                            :serial t
                                            :components 
                                            ((:file "classes")
                                            
                                             ))
                                   (:module "widgets"
                                            :serial t
                                            :components 
                                            ((:file "login")
                                             (:file "page-template")
                                         ))
                                   (:module "pages"
                                            :serial t
                                            :components 
                                            (
                                             (:file "home")
                                             (:file "index")
                                             (:file "user")))))
                         (:module "twitter"
                                  :serial t
                                  :components
                                  
                                  ((:file "common")
                                   
                                   (:module "db"
                                            :serial t
                                            :components 
                                            ((:file "twitter")
                                            
                                             ))
                                   (:module "pages"
                                            :serial t
                                            :components 
                                            ((:file "auth")
                                             
                                             ))
                                   
                                   ))))
               (:file "load-data")))

