(in-package :dyb)

(defvar *debug-errors* t)
(defvar *logging* nil)

(defclass dyb-acceptor (easy-acceptor)
  ((started :initarg :started
            :initform nil
            :accessor started)))

(defmethod start :after ((acceptor dyb-acceptor))
  (setf (started acceptor) t))

(defmethod acceptor-log-access :around ((acceptor dyb-acceptor)
                                        &key &allow-other-keys)
  (when *logging*
    (call-next-method)))

(defvar *permissions* ())

(defun check-permission (uri &optional sub-permission)
  (let ((user (current-user)))
    (cond ((null user)
           nil)
          ((super-user-p user)
           t)
          (t
           (loop for (permitted-uri . sub-permissions) in (permissions user)
                 do
                 (cond ((not (equal permitted-uri uri)))
                       ((or (null sub-permission)
                            (member sub-permission sub-permissions
                                    :test #'equal))
                        (return t))
                       (t 
                        (return))))))))

(defun check-permission-or-error (uri &optional sub-permissions)
  (or (check-permission uri sub-permissions)
      (signal 'permission-denied)))

(defmacro add-permission (name permissions)
  (check-type name string)
  `(load-time-value (setf (alexandria:assoc-value *permissions* ,name
                                                  :test #'equal)
                          ',permissions)))

(defmacro define-easy-handler (description lambda-list &body body)
  (when (atom description)
    (setq description (list description)))
  (destructuring-bind (name &rest args
                       &key uri for-everyone permissions
                       &allow-other-keys) description
    `(hunchentoot:define-easy-handler (,name ,@args :allow-other-keys t) ,lambda-list
       ,@(when (and uri (not for-everyone))
           `((add-permission ,uri ,permissions)
             (check-permission-or-error ,uri)
             ))
       ,@body)))

;;TODO: should this be here and should the error page not be done in the framework of
;;of the default page.
(defclass error-page (widget)
  ())

(defmethod render ((widget error-page) &key condition)
  (let ((title (frmt "Error - ~a" (script-name*))))
    (render (make-widget 'page :name "error" :title title)
            :body
            (with-html-to-string ()
              (:div :class "error-description"
                    (:strong :style "color: red;"
                             "Error: ")
                    (princ condition))))))

(defmethod hunchentoot:maybe-invoke-debugger ((condition error))
  (if *debug-errors*
      (invoke-debugger condition)
      (throw 'error condition)))

(defun call-with-error-handling (function)
  (if *debug-errors*
      (funcall function)
      (let ((condition
              (catch 'error
                (funcall function))))
        (if (typep condition 'error)
            (render (make-widget 'error-page)
                      :condition condition)
            condition))))

(defmacro with-error-handling (&body body)
  `(call-with-error-handling
    (lambda () ,@body)))

(defclass permission-denied-page (error-page)
  ())

(defmethod render ((widget permission-denied-page) &key)
  (let ((title (format nil "Access denied - ~a" (script-name*))))
    (render (make-widget 'page :name "permission-error"
                               :title title)
            :body
            (with-html-to-string ()
              (:div :class "permission-error"
                    (esc title))))))

(define-condition permission-denied ()
  ())

(defun call-with-permissions (function)
  (handler-case (funcall function)
    (permission-denied ()
      (render (make-widget 'permission-denied-page)))))

(defmacro with-permissions (&body body)
  `(call-with-permissions
    (lambda () ,@body)))

(defmethod handle-request :before ((acceptor dyb-acceptor) request)
  (unless (equal (script-name*) "/dyb/login")
    (unless (equal (session-value 'current-uri) (request-uri*))
      (setf (session-value 'previous-uri) (session-value 'current-uri)
            (session-value 'previous-page) (session-value 'current-page)))
    (setf (session-value 'current-uri) (request-uri*)
          (session-value 'current-page) (script-name*))))

(defvar *widget-parameters*)

(defun redirect-ajax-to-login ()
  (json:encode-json-to-string
   '(nil
     "window.location='/dyb/login';")))

(defmethod handle-request :around ((acceptor dyb-acceptor) request)
  (let ((script-name (script-name request)))
   (cond ((or (current-user)
              (equal script-name "/dyb/login")
              (alexandria:starts-with-subseq "/dyb/s/" script-name))
          (let (*print-pretty*
                (*widget-parameters* nil))
            (with-error-handling
              (with-permissions
                (call-next-method)))))
         ((alexandria:starts-with-subseq "/dyb/ajax/" script-name)
          (redirect-ajax-to-login))
         (t
          (setf (session-value 'redirect-after-login)
                script-name)
          (redirect "/dyb/login"))))) 

