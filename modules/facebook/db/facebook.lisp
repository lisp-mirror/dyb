(in-package :dyb)

(defclass facebook-insight (doc)
  ((insight-name 
    :accessor insight-name :initarg :insight-name
    :documentation "")
   (title 
    :accessor title :initarg :title
    :documentation "")
   (description 
    :accessor description :initarg :description
    :documentation "")
   (period 
    :accessor period :initarg :period
    :documentation ""))
  (:metaclass storable-class))

(defun facebook-insight-collection ()
  (get-collection (system-db) "facebook-insights"))

(defun facebook-insights ()
  (docs (facebook-insight-collection)))

(defun get-facebook-insight-by-id (id)
  (get-doc (facebook-insight-collection) id
           :element 'id))

(defun get-facebook-insight-by-name (name)
  (find-doc  
   (facebook-insight-collection)
    :test (lambda (doc)
            (string-equal name (get-val doc 'insight-name)))))

(defmethod doc-collection ((doc facebook-insight))
  (facebook-insight-collection))

(defun make-facebook-insight (insight-name title description period)
  (make-instance 'facebook-insight
                 :key insight-name
                 :insight-name insight-name 
                 :title title
                 :period period
                 :description description))

(unless (facebook-insight-collection)
  (add-collection (system-db) "facebook-insights" 
                  :collection-class 'dyb-collection
                  :load-from-file-p t))

(defclass facebook-insight-value (doc)
  ((channel-user :initarg :channel-user
                 :accessor channel-user)
   (insight
    :accessor insight :initarg :insight
    :documentation "")
   (value
    :accessor value :initarg :value
    :documentation "")
   (end-time :initarg :end-time))
  (:metaclass storable-class))

(defun facebook-insight-value-collection ()
  (get-collection (system-db) "facebook-insight-values"))

(defun facebook-insight-values ()
  (docs (facebook-insight-value-collection)))

(defun get-facebook-insight-value-by-id (id)
  (get-doc (facebook-insight-value-collection) id
           :element 'id))

(defun get-facebook-insight-value (channel-user insight end-time)
  (find-doc  
   (facebook-insight-value-collection)
    :test (lambda (doc)
            (and 
             (equal (id channel-user) (id (get-val doc 'channel-user)))
             (string-equal (get-val (get-val doc 'insight) 'insight-name) 
                           (get-val insight 'insight-name))
             (equal end-time (get-val doc 'end-time) )))))

(defun get-facebook-insight-values (channel-user insight start-time end-time)
  (find-doc  
   (facebook-insight-value-collection)
    :test (lambda (doc)
            (when (equal (id channel-user) (id (get-val doc 'channel-user)))
                (when (string-equal (get-val (get-val doc 'insight) 'insight-name) 
                               (get-val insight 'insight-name))
                  
                    (when (and 
                         (<= (universal-to-gmt-0 start-time)  
                             (universal-to-gmt-0 (get-val doc 'end-time)))
                         (>= (universal-to-gmt-0 end-time) 
                             (universal-to-gmt-0 (get-val doc 'end-time))))
                      
                        doc))))))



(defmethod doc-collection ((doc facebook-insight-value))
  (facebook-insight-value-collection))

(defun make-facebook-insight-value (channel-user insight value end-time)
  (make-instance 'facebook-insight-value
                 :key (list (id channel-user) (id insight) end-time)
                 :channel-user channel-user
                 :insight insight
                 :value value
                 :end-time end-time))

(unless (facebook-insight-value-collection)
  (add-collection (system-db) "facebook-insight-values" 
                  :collection-class 'dyb-collection
                  :load-from-file-p t))

#|

(dolist (insight (coerce (facebook-insights) 'list))
       (format t "======================================================================~%")
       (format t "~A~%" (get-val insight 'insight-name))
       (format t "======================================================================~%")
       (format t "~A~%" (get-val insight 'description)))

(let ((channel-user (get-channel-user-by-user-name "eMSdigital"))) 
       (dolist (insight (coerce (facebook-insights) 'list))
         (when (string-equal (get-val insight 'insight-name) "page_fan_adds")
             (format t "===================================================================~%")
             (format t "~A~%" (get-val insight 'insight-name))
             (format t "===================================================================~%")
             (dolist (value (find-docs 'list
                                       (lambda (doc)
                                         (string-equal 
                                          (get-val (get-val doc 'insight) 'insight-name) 
                                          (get-val insight 'insight-name)))
                                       (facebook-insight-value-collection)))
               (when (equal (id channel-user) (id (get-val value 'channel-user)))
                 (format t "~A ~A~%" (format-universal-date-time (get-val value 'end-time))
                         (get-val value 'value)))))))
|#