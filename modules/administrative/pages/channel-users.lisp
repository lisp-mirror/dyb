(in-package :dyb)

(define-easy-handler (channel-user-page :uri "/dyb/channel-users") ()
  (let* ((columns
           (list
            
            (make-instance 'grid-column
                           :name 'entity
                           :header "Entity"
                           :printer 'print-entity-name)
            (make-instance 'grid-column
                           :name 'channel-user-name
                           :header "User Name")
            (make-instance 'grid-column
                           :name 'channel-user-type
                           :header "Social Channel")
            (make-instance 'grid-column
                           :name 'user-id
                           :header "User Id" )
            (make-instance 'grid-column
                           :name 'last-access-token
                           :header "Token" 
                           :printer (lambda (token)
                                      (if token
                                          (if (> (length token) 8)
                                              (subseq token 0 8)
                                              token))))))
         (grid (make-widget 'channel-user-grid :name "channel-user-grid"
                                       :columns columns
                                       :title "Channel User"
                                       :row-object-class 'channel-user)))
    (setf (get-val grid 'grid-links) 
          (list
           (list "edit" "Edit")
           (list "delete" "Delete")
           (list "refresh-profile" "Refresh Profile")))
    (setf (sort-key-function grid)
          (lambda (doc)
            (if (stringp (get-val doc 'channel-user-name))
                (get-val doc 'channel-user-name)
                (write-to-string (get-val doc 'channel-user-name)))))
    
    (render (make-widget 'page :name "channel-user-page")
            :body (render-to-string grid))))
