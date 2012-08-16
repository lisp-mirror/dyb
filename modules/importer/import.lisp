(in-package :ems)

(defvar *import-types*
  '(("biographical"
     ("company" "mine" "entity" "industry-number" "id-number" "surname" "name"
      "gender" "race" "nationality" "country" "province" "town"
      "occupational-category" "occupational-level" "disabled" "employment-type"
      "job-title" "education-level" "housing-status" "date-of-engagement"
      "date-of-termination" "termination-reason"))
    ("test" ("a1" "b2" "c3"))))

(defun parse-csv-line (stream)
  (let ((csv-parser:*field-separator* #\|))
    (csv-parser:read-csv-line stream)))

(defun write-csv-line (fields stream)
  (let ((csv-parser:*field-separator* #\|))
    (csv-parser:write-csv-line stream fields)))

(defun get-header-and-some-data (file &key (lines 0))
  (with-open-file (stream file)
    (values (parse-csv-line stream)
            (loop repeat lines
                  collect (parse-csv-line stream)))))

(defvar *upload-directory* #p"/tmp/hunchentoot-upload/")

(defvar *uploaded-files* ())

(defun uploaded-file (name)
  (find name *uploaded-files*
        :key #'name :test #'equal))

(defun move-file (file)
  (ensure-directories-exist *upload-directory*)
  (nth-value 2 (rename-file file *upload-directory*)))

(defun make-unique-name (name)
  (loop for i from 0
        for new-name = name then (format nil "~a~~~a" name i)
        unless (uploaded-file new-name)
        return new-name))

(defun handle-upload (parameters)
  (destructuring-bind (path name application-type) parameters
    (declare (ignore application-type))
    (let ((time (get-universal-time))
          (new-path (move-file path))
          (name (make-unique-name name)))
      (when (probe-file new-path)
        (push (make-instance 'file-to-import
                             :name name
                             :path new-path
                             :timestamp time)
              *uploaded-files*)))))

(defun type-selector (upload)
  (let ((selector (make-widget 'select
                               :name "type-selector"
                               :items (mapcar (lambda (type)
                                                (list type (car type)))
                                              *import-types*))))
    (setf (first-item selector) nil)
    (cond ((post-parameter "change-type")
           (setf (file-type upload)
                 (value selector)))
          ((file-type upload)
           (setf (value selector)
                 (file-type upload)))
          (t
           (setf (first-item selector) "Select type")))
    (with-html
      (:form :action ""
             :method "post"
             (:input :type "hidden" :name "change-type" :value "t")
             (:label :for "type-selector" "Type")
             (render selector)
             (:input :type "submit" :value "Change type")))))

(defun mapping-selector (upload)
  (let ((selector (make-widget 'select
                               :name "mapping-selector")))
    (when (file-type upload)
      (setf (items selector)
            (mapcar (lambda (mapping)
                      (list mapping (name mapping)))
                    (select-dao 'mapping (:= 'type
                                             (string (car (file-type upload))))))))
    (setf (first-item selector) nil)
    (cond ((post-parameter "change-mapping")
           (setf (mapping upload)
                 (value selector)))
          ((mapping upload)
           (setf (value selector)
                 (mapping upload)))
          (t
           (setf (first-item selector) "Select mapping")))
    (with-html
      (:form :action ""
             :method "post"
             (:input :type "hidden" :name "change-mapping" :value "t")
             (:label :for "mapping-selector" "Mapping")
             (render selector)
             (:input :type "submit" :value "Change mapping")))))

(defun db-column-selector (row-number columns mapped-value)
  (render
   (make-instance 'select
                  :name (princ-to-string row-number)
                  :first-item "Discard"
                  :items (mapcar (lambda (column)
                                   (list column (string-capitalize column)))
                                 columns)
                  :value mapped-value)))

(defun retrieve-mapping-values (db-columns csv-columns)
  (let ((list (loop for (name . value) in (post-parameters*)
                    for name-id = (ensure-parse-integer name)
                    for value-id = (parse-integer value :junk-allowed t)
                    when (and (integerp name-id) (>= name-id 0)
                              (integerp value-id) (>= value-id 0))
                    collect (list (nth name-id csv-columns)
                                  (string (nth value-id db-columns))))))
    (if (plusp (length list))
        (make-array (list (length list) 2)
                    :initial-contents list)
        :null)))

(defclass mapping ()
  ((type :initarg :type
         :reader mapping-type
         :col-type string)
   (name :initarg :name
         :reader name
         :col-type string)
   (mapping :initarg :mapping
            :accessor mapping
            :initform :null
            :col-type (or db-null text[][])))
  (:metaclass dao-class)
  (:keys name type))

(defun find-mapping (type name)
  (car (select-dao 'mapping
                   (:and (:= 'name name)
                         (:= 'type (string type))))))

(defun retrieve-mapping (type db-columns csv-columns)
  (when (and (parameter "mapping-name")
             (parameter "save"))
    (let* ((name (parameter "mapping-name"))
           (values (retrieve-mapping-values db-columns csv-columns))
           (saved (find-mapping type name)))
      (cond (saved
             (setf (mapping saved) values)
             (update-dao saved)
             saved)
            (t
             (let ((new (make-instance 'mapping
                                       :type (string type)
                                       :name name
                                       :mapping values)))
               (insert-dao new)
               new))))))

(defun mapping-duplicates (mapping)
  (unless (eql mapping :null)
    (let ((hash (make-hash-table :test 'equal)))
      (loop for i below (array-dimension mapping 0)
            for element = (aref mapping i 1)
            if (gethash element hash)
            collect element
            else
            do (setf (gethash element hash) t)))))

(defun mapping-value (csv-column mapping)
  (unless (eql mapping :null)
    (loop for i below (array-dimension mapping 0)
          for element = (aref mapping i 0)
          when (equal element csv-column)
          return (aref mapping i 1))))

(defun edit-mapping (upload &key read-only)
  (type-selector upload)
  (mapping-selector upload)
  (let ((csv-columns (get-header-and-some-data (path upload)))
        (type (file-type upload)))
    (with-html
      (when type
        (let* ((db-columns (cadr type))
               (new-mapping (retrieve-mapping (car type)
                                              db-columns csv-columns))
               (duplicates (and new-mapping
                                (mapping-duplicates (mapping new-mapping)))))
          (cond (duplicates
                 (htm (:p :style "error"
                          "Mapping can't contain duplicate entries.")))
                (new-mapping
                 (setf (mapping upload) new-mapping)
                 (redirect "/ems/import")))
          (htm
           (:form
            :action ""
            :method "post"
            (:label :for "mapping-name" "Name")

            (:input :type "text" :value (or (and (mapping upload)
                                                 (name (mapping upload)))
                                            "name")
                    :name "mapping-name"
                    :id "mapping-name")
            (:input :type "submit" :value "Save mapping"
                    :name "save")
            (:table
             (loop with mapping = (mapping upload)
                   with values = (and mapping (mapping mapping))
                   for i from 0
                   for csv-column in csv-columns
                   for mapped-value = (and values
                                           (mapping-value csv-column values))
                   do
                   (htm
                    (:tr (:th (str csv-column))
                         (:td (if read-only
                                  (str (string-capitalize
                                        (or mapped-value
                                            "Discard")))
                                  (db-column-selector i db-columns
                                                      mapped-value)))
                         (when (member mapped-value duplicates :test #'equal)
                           (htm (:td :style "error"
                                     "Duplicate"))))))))))))))

(defun prepare-mapping (mapping columns)
  (loop for i below (array-dimension mapping 0)
        for csv-column = (aref mapping i 0)
        collect
        (position csv-column columns :test #'equal)))

(defun write-processed-line (line mapping out)
  (write-csv-line (loop for i in mapping
                        collect (nth i line))
                  out))

(defun mapping-header (mapping)
  (loop for i below (array-dimension mapping 0)
        for csv-column = (aref mapping i 1)
        collect csv-column))

(defun %process-file (file)
  (with-open-file (in (path file))
    (with-open-file (out (make-pathname
                          :name (conc (pathname-name (path file))
                                      "-processed")
                          :defaults (path file))
                         :direction :output
                         :if-exists :supersede)
      (let* ((columns (parse-csv-line in))
             (mapping (prepare-mapping (mapping (mapping file)) columns)))
        (write-csv-line (mapping-header (mapping (mapping file))) out)
        (loop for line = (parse-csv-line in)
              when line
              do (write-processed-line line mapping out)
              while (listen in))))))

(defun process-file (file)
  (let ((upload (uploaded-file file)))
    (when (and upload (mapping upload))
      (%process-file upload))))

(defclass file-to-import ()
  ((path :initarg :path
         :reader path)
   (name :initarg :name
         :reader name)
   (timestamp :initarg :timestamp
              :reader timestamp)
   (type :initarg :type
         :initform nil
         :accessor file-type)
   (mapping :initarg :mapping
            :initform nil
            :accessor mapping)))

(define-easy-handler (import-map :uri "/ems/import-map") (file)
  (unless file
    (redirect "/ems/import"))
  (let ((selected (uploaded-file file)))
    (with-html-to-string (:prologue t)
      (:html :xmlns "http://www.w3.org/1999/xhtml" :lang "en"
             (:head (:title "Mapping")
                    (:link :rel "stylesheet" :type "text/css"
                           :href "/css/style.css"))
             (:body
              (cond (selected
		     (edit-mapping selected))
                    (t
                     (htm (:h4 (fmt "File ~a not found" file))))))))))

(define-easy-handler (import-data :uri "/ems/import") (file process)
  (when (consp file)
    (handle-upload file)
    (redirect "/ems/import"))
  (when process
    (process-file process)
    (redirect "/ems/import"))
  (with-html-to-string (:prologue t)
    (:html :xmlns "http://www.w3.org/1999/xhtml" :lang "en"
           (:head (:title "Import")
                  (:link :rel "stylesheet" :type "text/css"
                         :href "/css/style.css"))
           (:body
            (:form :action ""
                   :method "post"
                   :enctype "multipart/form-data"
                   (:label :for "file" "Select file")
                   (:input :type "file" :name "file" :id "file")
                   (:input :type "submit" :value "Upload"))
            (when *uploaded-files*
              (htm
               (:strong "Uploaded files:")
               (:ul
                (loop for file in *uploaded-files*
                      do (htm (:li (str (name file))
                                   (:a :href (conc "/ems/import-map?file="
                                                   (name file))
                                       "Edit mapping")
                                   (when (mapping file)
                                     (htm
                                      (:a :href (conc "/ems/import?process="
                                                      (name file))
                                          "Process")))))))))))))

