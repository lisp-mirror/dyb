(in-package :ems)

(defun context-enties-display ()
  (let ((display ""))
    (dolist (entity
              (get-entity-relationships-ordered 
               (get-val (current-user) 'last-root) 
               (get-val (current-user) 'last-context)))
      (setf display (concatenate 'string display entity "\|")))))

(defclass page (peach-page)
  ((alert :initarg :alert
             :initform nil
             :accessor alert))
  (:metaclass widget-class)
  (:include-css "/css/ems/site_style.css")
  (:include-js "/js/ems/common.js"
               "/js/ems/checkbox-list.js"
               "/js/ems/validation.js"
               "/tinymce/jscripts/tiny_mce/tiny_mce.js"))


(defmethod render ((widget page) &key body)
  (let ( ;;(menu (make-widget 'tree-menu :name "menu"))
	(title (or (title widget) (name widget)))
        (peach-page (make-widget 'peach-page :name (widgy-name widget "peach-pagex")))
        (page-alert-box (make-instance 'alert-box :name "page-alert-box")))

    (setf (slot-value peach-page 'info-panel) (info-panel widget))
    (setf (slot-value peach-page 'title) title)
    (setf (slot-value peach-page 'author) "DATA X-WARE; info@dataxware.co.za")
    (setf (slot-value peach-page 'key-words) "Social and Labour plan")

    ;;(setf (slot-value menu 'menu-items) *global-menu*)
    ;;(setf (slot-value menu 'shortcut-items) *menu-shortcut-items*)
    ;;(setf (slot-value menu 'logo-image) "/images/x-packs-logo-small.png")
    ;;(setf (slot-value menu 'home-url) "/ems/home-page")
    
    (log-entry
     (name widget) 
     "Passed"
     "Entered the page.")
    (render
     peach-page
     :body
     (with-html-to-string ()
       (:div :id "main_content"
             (:h2 :class "grid_12" (str title))
             (:div :class "clean")
             (:div :class "grid_12"
                   (when (slot-val widget 'alert)
                     (str (render page-alert-box 
                                  :alert-type (first (slot-val widget 'alert))
                                  :alert (second (slot-val widget 'alert)))))
                      
                   (str body))
             (:div :class "clear")
             )
       (:div :class "push ")

       ))))
