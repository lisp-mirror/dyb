(in-package :ems)

(add-collection (system-db*) "users" :load-from-file-p nil)

;;Init an admin user just to get things started.
(let ((user (get-system-user   "DX" "haragx@gmail.com")))
  (unless user
    (make-system-user "DX" "haragx@gmail.com" "" ""  "")))

(let ((user (get-service-user "DX" "haragx@gmail.com" 'twitter)))
  (unless user
    (make-service-user "DX" 'twitter "haragx@gmail.com" "haragx")))

(add-collection (system-db*) "services" :load-from-file-p nil)

(let ((twit (get-service "DX" "Test App" 'twitter)))
  (unless twit
      (make-twitter-service-app "DX" "Test App" 
                                :key "H5wWh6azz3n0Go4hOu5kgg" 
                                :secret "m7u7UoEyTPIg5p0Gl1EV73hkl139tu3GkZjMetzS7G8" 
                                :callback-uri "http://smackaho.st:8000/twitter/callback"
                                :port 8000
                                )))