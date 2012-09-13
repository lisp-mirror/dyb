(in-package #:ems)

(define-easy-handler (search-stream-page :uri "/ems/search-stream") ()
  (let* ((columns
                             (list
                              (make-instance 'grid-column
                                             :name 'search-stream-type
                                             :header "Search Type"
                                             )
                              (make-instance 'grid-column
                                             :name 'search-stream
                                             :header "Search")
                              (make-instance 'grid-column
                                             :name 'search-stream-status
                                             :header "Status"
                                             )))
                           (grid (make-widget 'search-streams-grid 
                                                      :name "search-stream-grid"
                                                      :columns columns
                                                      :edit-inline nil
                                                      :title "Search Streams"
                                                      :row-object-class 'search-stream)))

                      
            
            
                     (render (make-widget 'page :name "search-stream-page")
                             :body (with-html-to-string ()
                                     
                                     (str (render grid))))
                     ))