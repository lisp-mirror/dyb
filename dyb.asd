
(defsystem dyb
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
             
               ironclad
               cl-json
               ht-simple-ajax
             
             
               closer-mop
               cl-jpeg
               xdb2
               sb-posix
               drakma
               split-sequence
               trivial-timers
               )
  :serial t
  :components ((:file "packages")
               (:file "utils")
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
                                             (:file "system-log")
                                             (:file "short-url")))

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
                                             (:file "html-framework-page")
                                             (:file "ems-page")
                                             (:file "input-widgets")
                                             (:file "login")
                                             (:file "root-entities")
                                             (:file "graphs")))
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
                                             (:file "root-entities")
                                             (:file "short-url")))))
                         (:module "administrative"
                                  :serial t
                                  :components
                                  ((:file "common")
                                   (:module "db"
                                            :serial t
                                            :components
                                            ((:file "country-town")
                                             (:file "contacts")
                                             (:file "address")
                                             (:file "companies")
                                             (:file "social-channels")
                                             (:file "channel-users")
                                             (:file "periods")
                                             ))

                                   (:module "widgets"
                                            :serial t
                                            :components
                                            ((:file "address")
                                             (:file "companies")
                                             (:file "country-town")
                                             (:file "channel-users")
                                             (:file "periods")
                                             (:file "clients")))

                                   (:module "pages"
                                            :serial t
                                            :components
                                            ((:file "address")
                                             (:file "companies")
                                             (:file "country-town")
                                             (:file "channel-users")
                                             (:file "periods")
                                             (:file "clients")
                                             (:file "oauth")
                                             ))))
                 
                 
                 
                         (:module "search-streams"
                                  :serial t
                                  :components
                                  ((:file "parser")
                                   
                                   (:file "common")
                                   (:file "end-points")
                                   (:file "listener")
                                   (:module "db"
                                            :serial t
                                            :components
                                            ((:file "search-streams")))
                                   (:module "widgets"
                                            :serial t
                                            :components
                                            ((:file "search-streams")))
                                   (:module "pages"
                                            :serial t
                                            :components
                                            ((:file "search-streams")))))
                 
                
                         (:module "facebook"
                                  :serial t
                                  :components
                                  ((:file "parser")
                                  ;; (:file "oauth")
                                   (:file "common")
                                   (:file "end-points")
                                   (:file "listener")
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
                                            ((:file "facebook")))
                                   
                                   ))

                         (:module "twitter"
                                  :serial t
                                  :components
                                  ((:file "parser")
                                 ;;  (:file "oauth")
                                   (:file "common")
                                   (:file "end-points")
                                   (:file "listener")
                                   (:module "db"
                                            :serial t
                                            :components
                                            ((:file "twitter")))
                   
                                   ))
                         (:module "linkedin"
                                  :serial t
                                  :components
                                  ((:file "parser")

                                   (:file "end-points")
                                   
                                   (:file "listener")
                                   
                   
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
                                            ((:file "generic")
                                             (:file "facebook-actions")
                                             (:file "twitter-actions")
                                             (:file "linkedin-actions")
                                             (:file "tasks")))
                                   (:module "pages"
                                            :serial t
                                            :components
                                            ((:file "generic")))))
                         (:module "scheduler"
                                  :serial t
                                  :components
                                  ((:file "scheduler")
                                   (:module "db"
                                            :serial t
                                            :components
                                            ((:file "generic-scheduler")))
                                   (:module "widgets"
                                            :serial t
                                            :components
                                            ((:file "generic-scheduler")
                                             ))
                                   (:module "pages"
                                            :serial t
                                            :components
                                            ((:file "generic-scheduler")
                                             (:file "manual-scheduler")))
                                   
                   
                                   ))
                         (:module "tasks"
                                  :serial t
                                  :components
                                  (
                                   (:module "db"
                                            :serial t
                                            :components
                                            ((:file "tasks")))
                                   (:module "widgets"
                                            :serial t
                                            :components
                                            ((:file "tasks")))
                                   (:module "pages"
                                            :serial t
                                            :components
                                            ((:file "tasks")))))))))
