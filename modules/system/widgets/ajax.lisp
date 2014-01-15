(in-package :dyb)

;; (defclass ajax-widget (widget)
;;   ((css-class :initarg :css-class
;;               :initform nil
;;               :accessor css-class))
;;   (:metaclass widget-class)
;;   (:include-js "/appjs/ajax.js"))

(defajax cl-ajax-render (script-name widget-id &rest args)
  (declare (ignore args))
  (setf (slot-value *request* 'script-name) script-name)
  (wfx::map-dom #'wfx::synq-dom)
  (wfx::map-dom #'wfx::synq-data)
  (wfx::map-dom-events)
  (setf (content-type*) "text/json")
  (let ((widget (get-widget widget-id :script-name script-name)))
    (when widget
      (json:encode-json-to-string
       (list (render-to-string widget :from-ajax t)
             (deferred-js))))))

(defajax table (script-name widget-id &rest args)
  (declare (ignore args))
  (setf (content-type*) "text/json")
  (let ((widget (get-widget widget-id :script-name script-name)))
    (when widget
      (setf (slot-value *request* 'script-name) script-name)
      (process-data-table widget))))
