
(defsystem ems
  :version "0.2"
  :depends-on (wfx
               hunchentoot
               cl-who
               postmodern
               simple-date
               date-calc
               local-time
               alexandria
               cl-ppcre
               csv-parser
               cl-typesetting
               ironclad
               cl-json
               ht-simple-ajax
 ;;              vecto-graphs
               closure-html
               closer-mop
               cl-jpeg
               xdb2
               sb-posix
               drakma)
  :serial t
  :components ((:file "packages")
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
                             (:file "login")
                             (:file "root-entities")))
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
                           ;;  (:file "export-csv")
                             (:file "root-entities")))))
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
                             (:file "service-users")
                             (:file "periods")
                             ))

                   (:module "widgets"
                    :serial t
                    :components
                            ((:file "address")
                             (:file "companies")
                             (:file "country-town")
                             (:file "service-users")
                             (:file "periods")
                             (:file "clients")))

                   (:module "pages"
                    :serial t
                    :components
                            ((:file "address")
                             (:file "companies")
                             (:file "country-town")
                             (:file "service-users")
                             (:file "periods")
                             (:file "clients")
                             ))))
                 
                 
                 
                 
                 
                
 (:module "facebook"
                  :serial t
                  :components
                  ((:file "facebook-parser")
                   (:file "common")
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
