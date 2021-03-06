(in-package :dyb)

(setf *random-state* (make-random-state t))

(defclass dyb-acceptor (site-acceptor)
  ())

(defclass dyb-theme (widget-theme)
  ())

(defvar *dyb-theme* (make-instance 'dyb-theme))

(defvar *dx-acceptor*
  (make-dx-site 'dyb-acceptor
                :port 8090
                :site-url "/dyb/"
                :debug-errors-p t
                :theme *dyb-theme*))

(defmacro define-easy-handler (description lambda-list &body body)
  `(define-dx-handler *dx-acceptor* ,description ,lambda-list
     ,@body))

(defmacro defajax (name lambda-list &body body)
  `(define-dx-ajax *dx-acceptor* ,name ,lambda-list
     ,@body))

(defmethod render-error-page ((acceptor dyb-acceptor) &key condition)
  (let ((title (frmt "Error - ~a" (script-name*))))
    (render (make-widget 'page :name "error" :title title)
            :body
            (with-html-string
              (:div :class "error-description"
                    (:strong :style "color: red;"
                             "Error: ")
                    (esc (princ-to-string condition)))))))

(defmethod render-permission-denied-page ((acceptor dyb-acceptor) &key)
  (let ((title (format nil "Access denied - ~a" (script-name*))))
    (render (make-widget 'page :name "permission-error"
                               :title title)
            :body
            (with-html-string
              (:div :class "permission-error"
                    (esc title))))))

(defmethod login-not-required :around ((access dyb-acceptor) script-name)
  (or (equal script-name "/dyb/registration")
      (call-next-method)))
;;;

(defparameter *installation* "Live Serve");;"Live Serve"

(defparameter *site-url* "http://dxw.co.za/")

(if (string-equal *installation* "Live Serve")
    (setf *site-url* "http://dxw.co.za/")
    (setf *site-url* "http://local.dataxware.co.za/"))

(defmethod handle-request :around ((acceptor dyb-acceptor) request)
  (let ((script-name (script-name request)))
    (if (alexandria:starts-with-subseq "/dyb/s/" script-name)
        (shorten-url)
        (call-next-method))))

(defparameter *log-deleted-objects* nil)
