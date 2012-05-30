(in-package #:ems)

(defparameter *app* nil)

;;; "ems" goes here. Hacks and glory await!

(defun current-user ()
  (session-value 'user))

(defclass page (widget)
  ())

(defmethod render ((page page) &key body header footer)
  (when (or (current-user) (equal (script-name*) "/ems/index"))
        (with-html-output (*standard-output*)
          (str "<!doctype html>")
          (:html 
           (:body 
            (if header
                (htm (:header (str header))))
            (:div 
             (str body))
            (if footer
                (htm (:footer (str footer))))))))
  (unless (or (current-user) (equal (script-name*) "/ems/index"))
    
        (redirect "/ems/index")))


(defclass ems-page (page)
  ())

(defmethod render ((page ems-page) &key body)

  (with-html-output (*standard-output*)
      (render (make-widget 'peach-page :name (widgy-name page "ems-page"))
              :body (with-html-output-to-string (*standard-output*)
                      (str body)))))