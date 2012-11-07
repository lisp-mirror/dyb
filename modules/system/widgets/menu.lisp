(in-package :dyb)

(defclass tree-menu (widget)
  ((tree :initarg :tree
          :accessor tree)
   (logo-image :initarg :logo-image
               :accessor logo-image)
   (menu-items :initarg :menu-items
               :accessor :menu-items)
   (shortcut-items :initarg :shortcut-items
                   :accessor shortcut-items)
   (home-url :initarg :home-url
             :accessor :home-url))
  (:metaclass widget-class)
  (:include-js "/js/menu.js"))

(defmethod action-handler ((widget tree-menu))
  (let ((parameter (parameter "quick-menu-name")))
      (when parameter
	(loop for (short-cut url) in (slot-val widget 'shortcut-items)
	      when (search parameter short-cut)
	      do (redirect url)))))

(defmethod render ((widget tree-menu) &key)
  (with-html
    (:div
     :id "content"
     (:a :href (slot-val widget 'home-url)
         (:img :src (slot-val widget 'logo-image) ))
     (:br)
     (:form :method "post" :action "" :name "quick-menu"
            "Quick Menu"
            (:br)
            (:input :type "text" :name "quick-menu-name"))
     (:ul :id "site-menu"
          :class "js_tree"
          (:p (:a :href "javascript: expandAll();" "open all")
              "|"
              (:a :href "javascript: collapseAll();" "close all"))
          (fmt
           "<script type=\"text/javascript\">~
afterLoad.push(function(){addMenu(~a)})</script>"
           (json:encode-json-to-string (slot-val widget 'menu-items)))))))