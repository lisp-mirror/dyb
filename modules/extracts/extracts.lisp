(in-package :ems)

(create-static-file-dispatcher-and-handler     
                                
                                "/dyb/eish.csv" 
"/var/www/ems.co.za/extracts/phil@dataxware.co.za/biographical.csv")

(defun extract-dir ()
  (let ((dir (format nil "~A/" *extract-dir*)))
    (ensure-directories-exist dir )
    dir))

(defun build-extract-virtual-path (file-name)
  (format nil "/dyb/extracts/~A/~A" (email (current-user)) file-name))

(defun build-extract-path (file-name)
  (format nil "~A~A" (extract-dir) file-name))

(defun write-biographical (docs)
  (with-open-file (stream (build-extract-path "biographical.csv")
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (format stream "~A~%" "Biographical Extract")
    (format stream "~A~%" "entity|industry-number|id-number|surname|name|gender|race|nationality|country|province|town|occupational-category|occupational-level|disabled|employment-type|job-title|education-level|housing-status|date-of-engagement|date-of-termination|termination-reason|date-of-birth")
    (dolist (doc docs)
      (format stream "~% ~A|~A|~A|~A|~A|~A|~A|~A|~A|~A|~A|~A|~A|~A|~A|~A| ~A|~A|~A|~A~%"
              (get-val doc 'entity)
              (get-val doc 'employee-number)
              (get-val doc 'id-number)
              (get-val doc 'surname)
              (get-val doc 'gender)
              (get-val doc 'race)
              (get-val doc 'nationality)
              (get-val doc 'occupational-category)
              (get-val doc 'occupational-level)
              (get-val doc 'disabled)
              (get-val doc 'employment-type)
              (get-val doc 'job-title)	
              (get-val doc 'education-level)
              (get-val doc 'housing-status)
              (get-val doc 'date-of-engagement)
              (get-val doc 'date-of-termination)
              (get-val doc 'termination-reason)
              (get-val doc 'date-of-birth)      
              (get-val doc 'termination-reason)
              (get-val doc 'date-of-birth))

      (dolist (add (get-val doc 'training-attendance))
        (format stream "~A~%" "Training Attendance")
        (format stream "~% ~A|~A|~A|~A|~%" 
                (get-val add 'mentor)
                (get-val add 'status)
                (get-val add 'start-date)
                (get-val add 'end-date)))

      (dolist (add (get-val doc 'addresses))
        (format stream "~A~%" "Addresses")
        (format stream "~% ~A|~A|~A|~A|~%" 
                (get-val add 'address-type)
                (get-val add 'country)
                (get-val add 'province)
                (get-val add 'town)))
      
      (dolist (cont (get-val doc 'employee-ee-strategy))
        (format stream "~A~%" "Employee EE Strategy")
        (format stream "~% ~A|~A|~%" 
                (get-val cont 'strategy)
                (get-val cont 'mentor)
                ))

      (dolist (cont (get-val doc 'contacts))
        (format stream "~A~%" "Contacts")
        (format stream "~% ~A|~A|~A|~A|~A|~%" 
                (get-val cont 'contact-type)
                (get-val cont 'contact-name)
                (get-val cont 'telephone-number)
                (get-val cont 'facsimile-number)
                (get-val cont 'email-address))))))


(defun write-supplier (docs)
  (with-open-file (stream (build-extract-path "suppllier.csv")
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (format stream "~A" "Suppliers Extract")
    (format stream "~A~%" "entity|supplier-name|supplier-reference|narrow-based-bee-classification|multinational|broad-based-bee-classification|hdsa-percentage|black-women-owned")
    (dolist (doc docs)
  
      (format stream "~A|~A|~A|~A|~A|~A|~A|~A|~A|~%"
              (get-val doc 'entity)
              (get-val doc 'verification-set)
              (get-val doc 'supplier-name)
              (get-val doc 'supplier-reference)
              (get-val doc 'narrow-based-bee-classification)
              (get-val doc 'multinational)
              (get-val doc 'broad-based-bee-classification)
              (get-val doc 'hdsa-percentage)
              (get-val doc 'black-women-owned))
  
      (dolist (bran (get-val doc 'branches))
        (format stream "~% ~A" 
                (get-val bran 'branch-name))
        (dolist (b-add (get-val bran 'addresses))
          (format stream "~% ~A|~A|~A|~A|~%" 
                  (get-val b-add 'address-type)
                  (get-val b-add 'country)
                  (get-val b-add 'province)
                  (get-val b-add 'town)))

        (dolist (b-add (get-val bran 'contacts))
          (format stream "~% ~A|~A|~A|~A|~A|~%" 
                  (get-val b-add 'contact-type)
                  (get-val b-add 'contact-name)
                  (get-val b-add 'telephone-number)
                  (get-val b-add 'facsimile-number)
                  (get-val b-add 'email-address)))))))
  

(defun write-procurement-transactions ()
  (with-open-file (stream (build-extract-path "procurement-transactions.csv")
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    ))



(defun write-training-intervention (docs)
  (with-open-file (stream (build-extract-path "Training-Interventions.csv")
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (format stream "~A" "Training-Interventions Extract")
    (dolist (doc docs)
     (format stream "~%~A|~A|~A|~A|~A|~A|~A|~%"
              (get-val doc 'entity)
              (get-val doc 'category)
              (get-val doc 'course-name)
              (get-val doc 'mentorship-p)
              (get-val doc 'entry-level)
              (get-val doc 'accredited-p)
              (get-val doc 'length)))))


(defun write-mines (docs)
  (with-open-file (stream (build-extract-path "Mines.csv")
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (format stream "~A" "Mines Extract")
    (dolist (doc docs)
      (format stream "~%~A|~A|~A|~A|~A|~A|~A|~A|~A|~%"
              (get-val doc 'entity)
              (get-val doc 'mining-right-reference-number)
              (get-val doc 'mining-right-date)
              (get-val doc 'commodity)
              (get-val doc 'lof-start-date)
              (get-val doc 'lof-end-date)
              (get-val doc 'financial-year-end-month)
              (get-val doc 'latitude)
              (get-val doc 'longitude)))
    
    (dolist (doc docs)
      (format stream "~A~%" "Hard To Fill Vacancy")
      (format stream "~% ~A|~A|~A|~A|~%" 
              (get-val doc 'reason)
              (get-val doc 'strategy)
              (get-val doc 'start-date)
              (get-val doc 'end-date)))

    (dolist (doc docs)
      (format stream "~A~%" "Mine Municipalities")
      (format stream "~% ~A|~%" 
              (get-val doc 'local-municipality)
              ))))


(defun write-job-title (docs)
  (with-open-file (stream (build-extract-path "job-titles.csv")
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (format stream "~A" "Job Titles Extract")

    (dolist (doc docs)
      (format stream "~%~A|~A|~A|~A|~A|~A~%"
              (get-val doc 'entity)
              (get-val doc 'job-title)
              (get-val doc 'description)
              (get-val doc 'core-mining)
              (get-val doc 'occupational-level)
              (get-val doc 'hard-to-fill-vacancy)))))

(defun write-training-intervention-attendance (docs)
    (with-open-file (stream (build-extract-path "write-training-intervention-attendance.csv")
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (dolist (doc docs)
      (format stream "~A" "Training Interventions Attendance")
      (format stream "~%~A|~A|~A|~%"
              (get-val doc 'training-intervention-id)
              (get-val doc 'mentor)
              (get-val doc 'status)))))

(defun authorized-dispatcher (dispatch-fn)
  (lambda (request)
    ;;TODO: Add security check.
    (funcall dispatch-fn request)))

;;/var/www/ems.co.za/extracts/phil@dataxware.co.za/biographical.csv 
;;--- /dyb/extracts/biographical.csv

(defun register-extract-file (file-name)
  (push (authorized-dispatcher (create-static-file-dispatcher-and-handler 
                                (build-extract-virtual-path file-name)
                                (build-extract-path file-name)
                                 )) 
        *dispatch-table*))


(defun make-path-dir (file-name)  
  (with-html-to-string ()    
    
    (register-extract-file file-name)
    (htm (str "Download the extracted file:  ") 
         (:a :href (build-extract-virtual-path file-name) 
             (str (string-capitalize file-name))))))

(defun call-document (type)
  (dolist (doc (coerce type 'list))
    (when (string-equal type "1")
      (format t "~% ~A ~A ~A ~A ~A ~A ~A ~A ~A ~A ~A ~A ~A ~A ~A ~A ~A ~A ~A ~A ~A~%"
              (get-val doc 'entity)
              (get-val doc 'employee-number)
              (get-val doc 'id-number)
              (get-val doc 'surname)
              (get-val doc 'gender)
              (get-val doc 'race)
              (get-val doc 'nationality)
              (get-val doc 'occupational-category)
              (get-val doc 'occupational-level)
              (get-val doc 'disabled)
              (get-val doc 'employment-type)
              (get-val doc 'job-title)	
              (get-val doc 'education-level)
              (get-val doc 'housing-status)
              (get-val doc 'date-of-engagement)
              (get-val doc 'date-of-termination)
              (get-val doc 'termination-reason)
              (get-val doc 'date-of-birth)      
              (get-val doc 'effective-std)
              (get-val doc 'termination-reason)
              (get-val doc 'date-of-birth)))))


(define-easy-handler (extracts :uri "/dyb/extract-data")
    ()
  (render (make-widget 'page :name "extracts-page"
                       :title "Extracts")
          :body
          (with-html-to-string ()
            (htm   
             (:form :action "" :method "post" 
                    (render (make-widget 'select :name "xextract-select" 
                                         :items '((1 "Biographical")
                                                  (2 "Biographical Address")
                                                  (3 "Biographical Contacts")
                                                  (4 "Training Intervention")
                                                  (5 "Training Intervention Attendance")
                                                  (6 "Procurement Transactions")
                                                  (7 "Supplier")
                                                  (8 "Supplier Address")
                                                  (9 "Supplier Contacts")
                                                  (10 "Job Title"))))
                                        
                    
                    (:input :type "text" :name "search-value" :value nil)
                    (:input :type "submit"))
             (:br)             
             (:br)

             (cond ((string-equal (parameter "xextract-select") "1")
                    (break "~A" (call-document (parameter "xextract-select")))
                    (str (make-path-dir "biographical.csv")))

                   ((string-equal (parameter "xextract-select") "2")
                    
                    (str (make-path-dir "biographical-address.csv")))

                   ((string-equal (parameter "xextract-select") "3")
                    (extract-biographical-contacts (build-extract-path "biographical-contacts.csv"))
                    (str (make-path-dir "biographical-contacts.csv")))

                   ((string-equal (parameter "xextract-select") "4")
                    (extract-training-intervention (build-extract-path "training-intervention.csv"))
                    (str (make-path-dir "training-intervention.csv")))

                   ((string-equal (parameter "xextract-select") "5")
                    (extract-training-intervention-attendance (build-extract-path "training-intervention-attendance.csv"))
                    (str (make-path-dir "training-intervention-attendance.csv")))

                   ((string-equal (parameter "xextract-select") "6")
                    (extract-procurement-transactions (build-extract-path "procurement-transactions.csv"))
                    (str (make-path-dir "procurement-transactions.csv")))

                   ((string-equal (parameter "xextract-select") "7")
                    (extract-supplier (build-extract-path "supplier.csv"))
                    (str (make-path-dir "supplier.csv")))

                   ((string-equal (parameter "xextract-select") "8")
                    (extract-supplier-address (build-extract-path "supplier-address.csv"))
                    (str (make-path-dir "supplier-address.csv")))

                   ((string-equal (parameter "xextract-select") "9")
                    (extract-supplier-contacts (build-extract-path "supplier-contacts.csv"))                    
                    (str (make-path-dir "supplier-contacts.csv")))

                   ((string-equal (parameter "xextract-select") "10")
                    (extract-job-title (build-extract-path "job-title.csv"))
                    (str (make-path-dir "job-title.csv"))))
             (:br)             
             (:br)
             ))))
