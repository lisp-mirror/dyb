(in-package :dyb)

(defclass reg-form (widget)
  ((fields :initarg :fields
           :initform nil
           :accessor fields)
   (state :initarg :state
          :initform nil
          :accessor state) 
   (error-messages :initarg :error-messages
                   :initform nil
                   :accessor error-messages))
  (:metaclass widget-class))

(defmethod render ((form reg-form) &key)
  (loop for field in (fields form)
        do (apply #'render-reg-field form field)))

(defun render-form-input (form name type description)
  (let ((current-value (form-field-value form name)))
    (with-html
      (case type
        ((:password :text)
         (htm
          (:input :type (string-downcase type)
                  :name name
                  :value current-value)))
        (:textarea
         (htm
          (:textarea :name name
                     (esc current-value))))
        (:checkbox
         (htm
          (:label :class "checkbox"
                  (:input :type "checkbox"
                          :name name
                          :checked (and current-value
                                        "t"))
                  (str description))))
        (:radio
         (loop for options in description
               do
               (destructuring-bind (text &key value default)
                   options
                 (htm
                  (:label :class "radio"
                          (:input :type "radio"
                                  :name name
                                  :value value
                                  :checked
                                  (or (and default
                                           (not current-value))
                                      (equal current-value value)))
                          (str text))))))
        (:select
         (with-html
           (:select :name name
                    (:option)
                    (loop for (value . description) in description
                          do
                          (htm
                           (:option :value value
                                    :selected (equal current-value value)
                                    (esc (or description
                                             value))))))))))))

(defun render-reg-field (form label &key name type required
                                         html check
                                         description)
  (declare (ignore check required))
  (let ((error (field-error form name)))
    (with-html
      (:div :class "control-group"
            (:label :class "control-label"
                    (esc label))
            (:div :class "controls"
                  (if html
                      (str html)
                      (render-form-input form name type description))
                  (when error
                    (htm (:div :class "field-error"
                                 (str error)))))))))

(defun field-error (form field)
  (cdr (assoc field (error-messages form) :test #'equal)))

(defun set-field-error (form field error)
  (let ((cons (assoc field (error-messages form) :test #'equal)))
    (if cons
        (setf (cdr cons) error)
        (push (cons field error) (error-messages form)))))

(defun remove-field-error (form field)
  (alexandria:removef (error-messages form)
                      field
                      :test #'equal
                      :key #'car))

(defun validate-reg-field (form &key name type (required t)
                                     html check description)
  (declare (ignore html check))
  (let ((value (form-field-value form name)))
    (cond ((and required (empty-p value))
           (set-field-error form name "This field is required")
           nil)
          ((and (eq type :select)
                (not (assoc value description :test #'equal)))
           (set-field-error form name "Invalid value.")
           nil)
          (t
           (remove-field-error form name)
           t))))

(defun validate-reg-form (form)
  (loop for field in (fields form)
        for valid = (apply #'validate-reg-field form (cdr field))
        for result = valid then (and result valid)
        finally (return result)))

(defun form-fill-state (form)
  (setf (state form)
        (loop for field in (fields form)
              for name = (getf (cdr field) :name)
              collect (cons name (post-parameter name)))))

(defun form-field-value (form field)
  (cdr (assoc field (state form) :test #'equal)))
