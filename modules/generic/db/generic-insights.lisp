(in-package :dyb)

(defclass generic-insight-value (doc)
  ((entity 
    :initarg :entity
    :accessor entity)
   (channel-user 
    :initarg :channel-user
    :accessor channel-user)
   (insight
    :accessor insight :initarg :insight
    :documentation "Name of insight.")
   (value
    :accessor value :initarg :value
    :documentation "This can be a compound value")
   (end-time 
    :initarg :end-time
    :documentation "The time stamp for when the insight was valid."))
  (:metaclass storable-class))

(defun generic-insight-value-collection ()
  (get-collection (system-db) "generic-insight-values"))

(defun generic-insight-values ()
  (docs (generic-insight-value-collection)))

(defun get-generic-insight-value-by-id (id)
  (get-doc (generic-insight-value-collection) id
           :element 'id))



(defun get-generic-insight-value (channel-user insight end-time)
  (find-doc  
   (generic-insight-value-collection)
    :test (lambda (doc)
            (and 
             (equal (id channel-user) (id (get-val doc 'channel-user)))
             (string-equal (get-val doc 'insight) 
                           insight)
             (equal (universal-date-strip-time end-time) 
                    (universal-date-strip-time (get-val doc 'end-time)) )))))

(defun get-generic-insight-values (channel-user insight start-time end-time)
  (find-docs  
   'list
   (lambda (doc)
           (when (and
                  (equal (id channel-user) (id (get-val doc 'channel-user)))
                  (string-equal (get-val doc 'insight) 
                           insight)
                  (<= start-time (get-val doc 'end-time))
                  (>= end-time (get-val doc 'end-time)))
             doc))
   (generic-insight-value-collection)))

(defmethod doc-collection ((doc generic-insight-value))
  (generic-insight-value-collection))

(defun make-generic-insight-value ( channel-user insight value end-time)
  (make-instance 'generic-insight-value                 
                 :key (list  (id channel-user) insight end-time)
               
                 :channel-user channel-user
                 :insight insight
                 :value value
                 :end-time end-time))

(unless (generic-insight-value-collection)
  (add-collection (system-db) "generic-insight-values" 
                  :collection-class 'dyb-collection
                  :load-from-file-p t))

(defun update-generic-insight (channel-user insight-name value)
  (let* ((end-time (universal-today))
         (dup (get-generic-insight-value 
               channel-user 
               insight-name end-time)))
    (when (or (not dup) (not (get-val dup 'value)))
      (when dup
        (setf (get-val dup 'value) value)
        (setf (get-val dup 'end-time) end-time)
        (persist dup))
      (unless dup
        (persist (make-generic-insight-value                                
                  channel-user 
                  insight-name
                  value
                  end-time))))))

(defun get-last-insight-date (channel-user insight-name)
  (let ((date 0))
    (find-docs 'list 
               (lambda (doc)
                 (if (string-equal (get-val (get-val doc 'channel-user) 
                                            'channel-user-name) 
                                   (get-val channel-user 'channel-user-name))
                     (if (string-equal (get-val doc 'insight) insight-name)
                         (if (get-val doc 'end-time)
                             (if (> (get-val doc 'end-time)
                                    date)
                                 (setf date (get-val doc 'end-time)))))))
               (generic-insight-value-collection))
    date))