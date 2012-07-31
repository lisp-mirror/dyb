;;;; ems.asd

(asdf:defsystem #:ems
  :serial t
  :depends-on (#:hunchentoot
               #:cl-who
               #:drakma
               #:cl-json
               #:xdb2
               #:cl-oauth
               #:wfx
               #:ht-simple-ajax)
  :components ((:file "package")
               (:file "common")
               (:file "requests")
               (:file "ini")
               (:module "modules"
                :serial t
                :components
                ((:module "db"
                  :serial t
                  :components
                  ((:file "copy")
                   (:file "common")
                   (:module "xdb"
                    :serial t
                    :components
                            ((:file "xdb")
                             (:file "classes")))))
                 (:module "system"
                  :serial t
                  :components
                  ((:file "common")
                   (:module "db"
                    :serial t
                    :components
                            ((:file "allsorts")
                             (:file "user")
                             (:file "entities")
                             (:file "entity-relationships")
                             (:file "system-log")))

                   (:module "widgets"
                    :serial t
                    :components
                            ((:file "ajax")
                             (:file "allsorts")
                             (:file "grid")
                             (:file "tree")
                             (:file "entity-tree")
                             (:file "entity-selection-tree")
                             (:file "menu")
                             (:file "peach-page")
                             (:file "ems-page")
                             (:file "input-widgets")
                             (:file "login")))
                   (:module "pages"
                    :serial t
                    :components
                            ((:file "landing-page")
                             (:file "allsorts")
                             (:file "dashboard")
                             (:file "context")
                             (:file "entities")
                             (:file "users")
                             (:file "permissions")
                             (:file "export-csv")))))
                 (:module "administrative"
                  :serial t
                  :components
                  ((:module "db"
                    :serial t
                    :components
                            ((:file "country-town")
                             
                             (:file "contacts")
                             (:file "address")
                             (:file "companies")
                             (:file "periods")
                             (:file "service-users")))

                   (:module "widgets"
                    :serial t
                    :components
                            ((:file "address")
                             (:file "clients")
                             (:file "companies")
                             (:file "country-town")
                             
                             (:file "periods")
                             (:file "service-users")))

                   (:module "pages"
                    :serial t
                    :components
                            ((:file "address")
                             (:file "clients")
                             (:file "companies")
                             (:file "country-town")
                             
                             (:file "periods")
                             (:file "service-users")))))
                 
                 
                 (:module "facebook"
                  :serial t
                  :components
                  ((:file "facebook-parser")
                   (:module "db"
                    :serial t
                    :components
                            ((:file "facebook")))
                   (:module "widgets"
                    :serial t
                    :components
                            ((:file "facebook")))
                   (:module "pages"
                    :serial t
                    :components
                            ((:file "facebook")))))
                 (:module "twitter"
                  :serial t
                  :components
                  ((:file "twitter-parser")
                   (:module "db"
                    :serial t
                    :components
                            ((:file "twitter-classes")))
                   
                   ))
                 (:module "generic"
                  :serial t
                  :components
                  (
                   (:module "db"
                    :serial t
                    :components
                            ((:file "generic-entry")))
                   (:module "widgets"
                    :serial t
                    :components
                            ((:file "generic")))
                   (:module "pages"
                    :serial t
                    :components
                            ((:file "generic"))))))) ))

