(in-package :dyb)

(defclass importer-class (widget)
  ())

;THREE FUNCTIONS FOR PATH,FILE LOCATION AND TYPE
(defun upload-path ()
  (first (parameter "file-location")))

(defun upload-file (parameter)
  (second parameter))

(defun upload-content-type ()
  (third (parameter "file-location")))

;GETTING THE ENTITY ID
(defun dq-entity-id (eid)
  (query
   (:select 'entity_value :from 'entities
                         :where (:and
                                 (:='id eid)
                                 (:ilike'record-status "Active")))
   :single))


;UPLOADING THE FILE
(defmethod action-handler ((widget importer-class))  
  
  (when (and (parameter "file-location")
             (listp (parameter "file-location")) 
             (string-equal (upload-content-type) "text/csv"))
    (dq-insert-records)
    (destructuring-bind (path file-name content-type)
        (parameter "file-location")
      (rename-file path (format nil "~~/Development/ems.co.za/uploaded/~A.csv" file-name))))
  
  (when (parameter "delete")
    (dq-delete-file)))

(defun dq-delete-file ()
  (query
     (sql-compile
      `(:update 'import
                :set'record_status,"Invalidated"
                :where (:= 'id, (parameter "import-id"))
                ))))


;INSERTING TO THE TABLE

(defun dq-insert-records ()
 (query
   (sql-compile
    `(:insert-into 'import
                   :set
                   'id,(dq-next-sequence "import_id_seq")
                   'new_file_name,(format nil "~A_~A_~A" 
                                          (dq-entity-id 
                                           (dq-entity-return-root (first (context-entities)))) 
                                          (parameter "data-area") 
                                          (current-date-time))
                   'original_file_name,(upload-file (parameter "file-location"))
                   'data_area,(parameter "data-area")
                   'client_id,(dq-entity-return-root (first (context-entities)))
                   'record_status,"Active"
                   'auth_status,""
                   'stamp,(current-date-time)
                   'active-user,(email (current-user))
                   'log-action,"Insert"
                   ))))


(defmethod render ((widget importer-class) &key)
  (let ((rows (query (:select '*
                              :from 'import
                              :where (:ilike'record-status "Active"))
                     :alists))
        (count 0))
   ; (dq-user-enity-selection (context-entities))
        
    (with-html-to-string ()
      (htm
       (:table :align "centre" :border "0"
               (:form :action "" :type "post"
                      (:tr) ;(break "~A" (dq-entity-id (dq-entity-root (first (context-entities)))))
                      (:tr (:b (str (dq-entity-id (dq-entity-return-root (first (context-entities))))))
                           ;(:input :type "submit" :name "select-entity" :value "Set")
                           ))))
      (:table :align "center" :border "0"
              (:form :method "post" :enctype "multipart/form-data"
                     (:tr
                     (:input :type "hidden" :id "d-area" :name "data-area"))
                     (:tr (:select :name "data-area"
                                   (:option :value "Suppliers" "Suppliers")
                                   (:option :value "Biographical" "Biographical")
                                   (:option :value "Procurement" "Procurement")
                                   (:option :value "Job-Titles" "Job-Titles")
                                   (:option :value "Courses" "Courses")
                                   (:option :value "Employee-Courses" "Employee-Courses")))(:br)
                                   (:input :type "file" :name "file-location")(:br)
                                   (:input :type "submit" :name "upload" :value "Upload File") (:br)))
;;;;;;;;;
      (:div
       (:form :method "post" :action "/dyb/importer-two"
              (:table :id "importer" :class "table_class"
                      (:tr
                       (:th "Original File Name")
                       (:th "New File Name")
                       (:th "Delete" )
                       (:th "Map" ))
                      (:input :type "hidden" :id "map" :name "data-area")
                      (dolist (row rows)
                        (incf count)
                        (htm
                         (:tr
                          (:td (str (ffr 'original-file-name row)))
                          (:td (str (ffr 'new-file-name row))) 
                          ;<a href=" " onClick="alert('Message goes here');return false;">Simple Text Link for Alert</a>
                          ;<a href="http://somewhere_else" onclick="return confirm()">
                         ; (:input :type "submit" :name "save-post":value "Save" :onClick "alert(\"Your article has been saved\")")
                          (:td (format nil "/dyb/importer-one?import-id=~A&button-type=delete"  
                                                 (ffr 'id row)) 
                               (:input :type "submit" :name "delete" :value "Delete File"))
                          (:td (format nil "/dyb/importer-one?import-id=~A&button-type=map"  
                                           (ffr 'id row))
                               (:input :type "submit" :name "map" :value "Map File")))))))
       ))))




(define-easy-handler (importer-one :uri "/dyb/importer-one")
    ()
  (let ((page (make-widget 'page :name "importer-one" :title "Importer"))
        (page-body (make-widget 'importer-class)))
    (render page :body
            (with-html-output-to-string (*standard-output* nil :indent false)
                (get-parameters*)
                (:br)
                (str (render page-body))))))

