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
                           :header "User Id" )))
         (grid (make-widget 'channel-user-grid :name "channel-user-gridc"
                                       :columns columns
                                       :edit-inline nil
                                       :title "Channel User"
                                       :row-object-class 'channel-user)))
    (setf (sort-key-function grid)
          (lambda (doc)
            (if (stringp (get-val doc 'channel-user-name))
                (get-val doc 'channel-user-name)
                (write-to-string (get-val doc 'channel-user-name)))))
    
    (render (make-widget 'page :name "channel-user-page")
            :body (render-to-string grid))))
