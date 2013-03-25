(in-package :dyb)

(defclass ajax-widget (widget)
  ((css-class :initarg :css-class
              :initform nil
              :accessor css-class))
  (:metaclass widget-class)
  (:include-js "/appjs/ajax.js"))

(defvar *in-ajax-request* nil)

(defmethod render :around ((widget ajax-widget) &key from-ajax)
  (if from-ajax
      (let ((*in-ajax-request* t))
        (call-next-method))
      (with-html
        (:div :id (name widget)
              :class (css-class widget)
              (call-next-method)))))

(defajax cl-ajax-render (script-name widget-id &rest args)
  (declare (ignore args))
  (setf (slot-value *request* 'script-name) script-name)
  (wfx::map-dom #'wfx::update-dom)
  (wfx::map-dom #'synq-widget-data)
  (wfx::map-dom #'action-handler)
  (setf (content-type*) "text/json")
  (let ((widget (get-widget widget-id :script-name script-name)))
    (when widget
      (json:encode-json-to-string
       (list (render-to-string widget :from-ajax t)
             (deferred-js))))))

(defun js-render (widget &rest args-scripts)
  (format nil "ajax_render(~s, ~s~@[, [~{~a~^,~}]~])"
          (script-name*)
          (if (typep widget 'widget)
              (name widget)
              widget)
          args-scripts))

(defun js-render-form-values (widget form-name
                              &rest args-scripts)
  (format nil "ajax_render(~s, ~s, get_form_values(~s)~@[.concat([~{~a~^,~}])~])"
          (script-name*)
          (name widget)
          form-name
          args-scripts))

(defgeneric process-data-table (widget))

(defajax table (script-name widget-id &rest args)
  (declare (ignore args))
  (setf (content-type*) "text/json")
  (let ((widget (get-widget widget-id :script-name script-name)))
    (when widget
      (setf (slot-value *request* 'script-name) script-name)
      (process-data-table widget))))
