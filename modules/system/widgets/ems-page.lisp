(in-package :dyb)

(defclass ems-page (html-framework-page)
  ((alert :initarg :alert
             :initform nil
             :accessor alert))
  (:metaclass widget-class)
  (:include-css "/appcss/site_style.css")
  (:include-js 
   "/appjs/common.js"
   "/appjs/checkbox-list.js"
   "/appjs/validation.js"
   "/tinymce/jscripts/tiny_mce/tiny_mce.js"))

(register-widget *dyb-theme* 'page 'ems-page)

(defmethod render ((widget ems-page) &key body)
  (let ( ;;(menu (make-widget 'tree-menu :name "menu"))
	(title (or (title widget) (name widget)))
        (html-framework-page (make-widget 'html-framework-page 
                                          :name (widgy-name widget "html-framework-page")))
        (page-alert-box (make-instance 'alert-box :name "page-alert-box")))

    (setf (slot-value html-framework-page 'info-panel) (info-panel widget))
    (setf (slot-value html-framework-page 'title) title)
    (setf (slot-value html-framework-page 'author) "DATA X-WARE; info@dataxware.co.za")
    (setf (slot-value html-framework-page 'key-words) "Dig Your Brand,Digital Marketing,Social")

    (log-entry
     (name widget) 
     "Passed"
     "Entered the page.")
    (render
     html-framework-page
     :body
     (with-html-string
       (:div ;;:class "page-header"
        (:h2 (str title)))
                   
       (when (slot-val widget 'alert)
         (str (render page-alert-box 
                      :alert-type (first (slot-val widget 'alert))
                      :alert (second (slot-val widget 'alert)))))
       (str body)))))
