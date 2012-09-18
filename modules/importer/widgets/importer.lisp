(in-package :ems)

(defclass upload-file (widget)
  ())

(defmethod render ((widget upload-file) &key)
  (with-html
    (:form :action ""
           :method "post"
           :enctype "multipart/form-data"
           (:input :type "hidden" :name "form-id" :value "upload-file-form")
           (:label :for "file" "Select file")
           (:input :type "file" :name "file" :id "file")
           (:input :type "submit" :value "Upload"))))


(defparameter *upload-directory* #p"~/hunchentoot-upload/")

(defun move-file (file new-name)
  (ensure-directories-exist *upload-directory*)
  (nth-value 2 (rename-file file  (format nil "~A~A" *upload-directory* new-name))))

(defun handle-upload (parameters)
  (destructuring-bind (path name application-type) parameters
    (declare (ignore application-type))
    (let* ((new-name (format nil "unprocessed-~A-~A.csv" (random 99999) (get-universal-time)))
           (new-path (move-file path new-name))
          )
      (when (probe-file new-path)
        (make-auth-dispathcer new-name)
        (persist (make-xpack-import "Default"
                                 nil
                                 name
                                 new-name))))))

(defun authorized-dispatcher (dispatch-fn)

  (lambda (request)
    
    (when (current-user)

      (funcall dispatch-fn request))))

(defmethod make-auth-dispathcer (file-name)
  (push
   (authorized-dispatcher (create-static-file-dispatcher-and-handler
                           (format nil "/ems/imports/~A" file-name)
                                        ;(format nil "~A~A" *upload-directory* file-name)
                           (format nil "/home/phil/hunchentoot-upload/~A" file-name)
                           ))
   *dispatch-table*))

(defmethod action-handler ((widget upload-file))
  (when (string-equal (parameter "form-id") "upload-file-form")
      (if (parameter "file")
          (handle-upload (parameter "file")))))

(defclass importer-grid (grid)
  ())

(defmethod get-rows ((grid importer-grid))
  (setf (rows grid)
        (coerce
         (xdb2::sort-docs
          (find-docs 'vector 
                     (lambda (doc)
                       (if (not (string-equal (get-val doc 'doc-status) "Superseded"))
                           doc))
                     (xpack-imports-collection))
          :sort-value-func (lambda (doc)
                             (get-val doc 'import-type))
          :sort-test-func #'string<)
          'list)))

(defun render-import-edit-form (grid row)
  (let ((form (make-widget 'html-framework-form :name "importer-form"
                           :grid-size 12
                           :header "Import"
                           :form-id "import-edit-form"))
        (form-section (make-widget 'form-section
                                   :name "form-section")))
    
      (render form
              :grid grid
              :content
              (with-html-to-string ()
                (render form-section 
                        :label "Verification Set"
                        :input 
                        (with-html-to-string ()
                          (render-edit-field "verification-set" 
                                             (get-val row 'verification-set))))
                (render form-section 
                        :label "Type"
                        :input 
                        (with-html-to-string ()
                          (render-edit-field "import-type" 
                                             (get-val row 'import-type)
                                              :type :select
                                              :data (find-allsorts-for-select "Import Type"))))
              
                (render form-section 
                        :label "Original File Name"
                        :input 
                        (with-html-to-string ()
                          (render-edit-field "original-file-name" 
                                             (get-val row 'original-file-name))))
                (render form-section 
                        :label "File"
                        :input 
                        (with-html-to-string ()
                          (render-edit-field "file-name" 
                                             (get-val row 'file-name))))))))


(defun parse-csv-line (stream)
  (let ((csv-parser:*field-separator* #\|))
    (csv-parser:read-csv-line stream)))

(defun get-some-lines (file &key (lines 0))
  (with-open-file (stream file)
    (values (parse-csv-line stream)
            (loop repeat lines
                  collect (parse-csv-line stream)))))

(defun get-header-line (file)
  (get-some-lines file :lines 1))

(defun render-import-tab (import-tab mapping-tab)
  (let ((tab-box (make-widget 'html-framework-tab-box
                              :name "supplier-branch-tab-box"
                              :header "Branch"
                              :icon "card--pencil")))
    (setf (tabs tab-box)
          (list
             (list 
              "Import"
              (with-html-to-string ()
                (:div :class "section _100"
                      (str import-tab))))
             (list 
              "mapping"
              (with-html-to-string ()
                (:div :class "section _100" 
                      (str mapping-tab))))))
    (render tab-box)))


(defun match-mapping (mappings import-slot)
  (dolist (mapping mappings)
    (if (string-equal import-slot (get-val mapping 'import-slot))
        (return-from match-mapping (get-val mapping 'file-column))))
  )

(defun render-import-mapping-form (grid row )
  (let ((col-headers (get-header-line (format nil "/home/phil/hunchentoot-upload/~A" (get-val row 'file-name))))
        (form (make-widget 'html-framework-form :name "importer-mapping-form"
                           :grid-size 12
                           :header "Import Mapping"
                           :form-id "import-mapping-edit-form"))
        (form-section (make-widget 'form-section
                                   :name "form-section")))
    
      (render form
              :grid grid
              :content
              (with-html-to-string ()
                (dolist (allsort (find-allsorts (format nil "Import Type - ~A" (get-val row 'import-type))))
                  (htm (render form-section 
                               :label (get-val allsort 'description)
                               :input 
                               (with-html-to-string ()
                                 (render-edit-field (get-val allsort 'sort-value) 
                                                    (match-mapping (get-val row 'mapping) (get-val allsort 'sort-value))
                                                    :type :select
                                                    :data col-headers))))))
              )))


(defmethod render-row-editor ((grid importer-grid) row)
  (unless (get-val row 'key)
      (render (make-widget 'upload-file)))
  
  (when (get-val row 'key)
    
    (if (get-val row 'import-type)
        (render-import-tab
         (with-html-to-string () (render-import-edit-form grid row))
         (with-html-to-string () (str (render-import-mapping-form grid row))))
        (render-import-edit-form grid row))))


(defmethod handle-action ((grid importer-grid) (action (eql 'save)))
  (when (string-equal (parameter "form-id") "import-edit-form")
    (persist-from-grid-edit (editing-row grid) 
                            (parameter "file-name"))
    (finish-editing grid))
  
  (when (string-equal (parameter "form-id") "import-mapping-edit-form")
    (let ((old-import (copy (editing-row grid)))
          (import (editing-row grid)))
      (setf (get-val import 'mapping) nil)
      (dolist (parm (post-parameters*))
        
        (unless (find (car parm) (list "form-id" "action" "grid-name" "[object HTMLHtmlElement]") :test 'string-equal)
          (setf (get-val import 'mapping) (append (get-val import 'mapping) (list (make-instance 'xpack-import-mapping :import-slot (car parm) :file-column (cdr parm)))))))
      (if (get-val import 'xid)
          (persist import)
          (persist import :old-object old-import))
      (finish-editing grid))))