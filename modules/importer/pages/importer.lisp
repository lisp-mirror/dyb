(in-package :ems)

(define-easy-handler (importer-page :uri "/ems/importer") ()
  (let* ((columns
           (list
            (make-instance 'grid-column
                           :name 'verification-set
                           :header "Set")
            (make-instance 'grid-column
                           :name 'import-type
                           :header "Type")
            (make-instance 'grid-column
                           :name 'original-file-name
                           :header "Original File Name")
            (make-instance 'grid-column
                           :name 'file-name
                           :header "File Name"
                           :printer (lambda (file-name)
                                      (with-html-to-string ()
                                        (:a :href (format nil "/ems/imports/~A" file-name) (str file-name)))))
            (make-instance 'grid-column
                           :name 'user
                           :header "User")
            (make-instance 'grid-column
                           :name 'import-status
                           :header "Status")))
         (grid (make-widget 'importer-grid :name "importer-gridx"
                            :columns columns
                            :edit-inline nil
                            :title "Importer"
                            :row-object-class 'xpack-import)))
    (render (make-widget 'page :name "importer-page")
            :body (render-to-string grid))))
