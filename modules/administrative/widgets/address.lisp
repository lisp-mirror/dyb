(in-package :dyb)

(defclass address-grid (grid)
  ())

(defmethod get-rows ((grid address-grid))
; (break "~A" "Pong")
  (let ((docs))
    (dolist (doc (coerce (biographicals) 'list))
      (when (match-context-entities doc)
        (dolist (docx (coerce (get-val doc 'addresses) 'list))
          (setf (get-val docx 'user) (get-val doc 'employee-number))
          (setf (get-val docx 'key) (print-entity-name (get-val doc 'entity)))
          (setf docs (append docs (list docx)))) ;(break "~A" "Ping")
        ))
    (coerce docs 'vector))
  )


(defmethod export-csv ((grid address-grid))
  (let* ((data (grid-filtered-rows grid)))
    (when data      
      (with-output-to-string (stream)

        (format stream "Addresses Export Filter :~A    Search :~A    Date:~A ~%Context:~A~%~%"  
                (or (get-val grid 'grid-filter ) "") (or  (get-val grid 'search-term ) "")  (current-date-time) (print-context) )
             
        (loop 
           for doc across data
           do ;(break "~A"bio-doc)
             (format stream "~A|~A|~A|~A|~A|~A~%"
                       (get-val doc 'key)
                       (get-val doc 'user)
                       (get-val doc 'address-type)
                       (get-val (get-val doc 'country-town) 'country)
                       (get-val (get-val doc 'country-town) 'province)
                       (get-val (get-val doc 'country-town) 'town)))))))

(defmethod render-row-editor ((grid address-grid) row)
  (finish-editing grid))

(defmethod handle-action ((grid address-grid) (action (eql :save)))
  (finish-editing grid))
