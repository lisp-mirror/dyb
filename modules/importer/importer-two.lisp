(in-package :ems)

(defclass importer-two-class (widget)
  ())

(defmethod action-handler ((widget importer-two-class))  
   )

(defmethod render ((widget importer-two-class) &key)
  
  (with-html-to-string ()
      (htm
       (:div
       (:table :align "center" :border "1" :id "importer" :class "table_class"
              (:form :method "post" :enctype "multipart/form-data"
                     (:tr "Please map your file columns with standard Data Area columns") (:br)
                     (:tr
                       (:th "Standard System Columns")
                       (:th "File Mapings"))
                       (:th (:input :type "submit" :name "upload" :value "Upload File"))
                     (:br)))))
      (:table :align "center" :border "1"
              (:form :method "post" :enctype "multipart/form-data"
                     (:tr)
                     (:tr)(:br)
                                   (:br)
                                   (:br)))))

(define-easy-handler (importer-two :uri "/dyb/input/importer-two")
    ()
  (let ((page (make-widget 'page :name "importer-two" :title "Importer Map"))
        (body (make-widget 'importer-two-class)))
    (render page :body
            (with-html-output-to-string (*standard-output* nil :indent false)
                (get-parameters*)
                (:br)
                (str (render body))))))