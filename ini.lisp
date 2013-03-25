(in-package :dyb)

(setf *random-state* (make-random-state t))

(defclass dyb-acceptor (site-acceptor)
  ())

(defvar *acceptor*
  (make-dx-site 'dyb-acceptor
                :port 8090
                :site-url "/dyb/"
                :debug-errors-p t))

(defmacro define-easy-handler (description lambda-list &body body)
  `(define-dx-handler *acceptor* ,description ,lambda-list
     ,@body))

(defmacro defajax (name lambda-list &body body)
  `(define-dx-ajax *acceptor* ,name ,lambda-list
     ,@body))

(defmethod render-error-page ((acceptor dyb-acceptor) &key condition)
  (let ((title (frmt "Error - ~a" (script-name*))))
    (render (make-widget 'page :name "error" :title title)
            :body
            (with-html-to-string ()
              (:div :class "error-description"
                    (:strong :style "color: red;"
                             "Error: ")
                    (esc (princ-to-string condition)))))))

(defmethod render-permission-denied-page ((acceptor dyb-acceptor) &key)
  (let ((title (format nil "Access denied - ~a" (script-name*))))
    (render (make-widget 'page :name "permission-error"
                               :title title)
            :body
            (with-html-to-string ()
              (:div :class "permission-error"
                    (esc title))))))

;;;

(defparameter *installation* "Local Machine");;"Live Serve"

(defparameter *site-url* "http://dxw.co.za/")

(if (string-equal *installation* "Live Serve")
    (setf *site-url* "http://dxw.co.za/")
    (setf *site-url* "http://local.dataxware.co.za/"))

(defmethod handle-request :around ((acceptor dyb-acceptor) request)
  (let ((script-name (script-name request)))
    (if (alexandria:starts-with-subseq "/dyb/s/" script-name)
        (shorten-url)
        (call-next-method))))

