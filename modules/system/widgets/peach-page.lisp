(in-package :ems)

(defclass alert-box (widget)
  ())

;;TODO: Implement rest of functionality, like hide, nopadding etc
(defmethod render ((widget alert-box) &key alert-type alert)
  "alert-type: info,success,error,warning,note"
  (with-html
    (:div :class  (format nil "alert ~A" alert-type)
          (:span :class "icon")
          (:span :class "hide" "x")                             
          (:span (esc alert)))))

(defclass peach-box (widget)
  ((grid-size :initarg :grid-size
              :initform nil
              :accessor grid-size)
   (header :initarg :header
           :initform nil
           :accessor header)
   (header-content :initarg :header-content
                   :initform nil
                   :accessor header-content)
   (icon :initarg :icon
         :initform nil
         :accessor icon)
   (icon-size :initarg :icon-size
              :initform 16
              :accessor icon-size) 
   (content :initarg :content
            :initform nil
            :accessor content)
   (collapsible :initarg :collapsible
                :initform t
                :accessor collapsible)))

(defmethod render ((box peach-box) &key)
  (with-html-to-string ()
    (:div :class (format nil "grid_~A" (grid-size box))
          (:div :class "box" :id (name box)
                (:div :class "header"
                      (when (icon box)
                        (make-icon (icon box)
                                   :size (icon-size box)))
                      (:h3 (esc (header box)))
                      (when (collapsible box)
                        (htm (:div :class "collapse")))
                      (when (header-content box)
                        (str (header-content box))))
                (:div :class "content no-padding"
                      (str (content box)))))))

(defclass peach-form (widget)
  ((grid-size :initarg :grid-size
              :initform 6
              :accessor grid-size)
   (header :initarg :header
           :accessor header)
   (form-id :initarg :form-id
            :accessor form-id)
   (grid-name :initarg :grid-name
            :accessor grid-name)))

(defmethod render ((widget peach-form) &key content)
  (with-html
    (:div :class (format nil "grid_~A" (get-val widget 'grid-size))
          (:div :class "box"
                (:div :class "header"
                      (:img :src "" :alt "" :width 16 :height 16)
                      (:h3 (str (get-val widget 'header)))
                      (:div :class "collapse"))
                (:form :name (get-val widget 'form-id) :id (get-val widget 'form-id) 
                       :class "validate" :method "post"
                       (:input :type "hidden" :name "form-id" 
                               :value (get-val widget 'form-id))
                       
                       (if (get-val widget 'grid-name)
                           (htm (:input :type "hidden" :name "grid-name" 
                                        :value (get-val widget 'grid-name))))

                       (:div :class "content no-padding"
                             
                             (str content))
                       (:div :class "actions"
                             (:div :class "actions-left"
                                   (:input :name "action" :type "button" :value "Cancel"
                                            :onclick (format nil
                                       "javascript:~@[if(confirm(\"~a\"))~]~
document.getElementById(\"~A\").submit();"
                                       "Any input will be lost! Proceed with cancel?" (format nil "~A-cancel" (get-val widget 'form-id)))))
                             
                             (:div :class "actions-right"
                                   (:input :name "action" :type "submit" :value "Save"))))
                (:form :type "post" :name (format nil "~A-cancel" (get-val widget 'form-id)) :id (format nil "~A-cancel" (get-val widget 'form-id))
                       (:input :type "hidden" :name "action" :value "Cancel")
                       (if (get-val widget 'grid-name)
                           (htm (:input :type "hidden" :name "grid-name" 
                                        :value (get-val widget 'grid-name)))))
                ))))

(defclass form-section (widget)
  ((section-size :initarg :section-size
                 :initform 100
                 :accessor section-size)))

(defmethod render ((widget form-section) &key label input)
  (with-html
    (:div :class (format nil "section _~A" (get-val widget 'section-size)) 
          (:label 
           (str label))
          (:div 

           (str input)
           ))))

(defclass peach-tab-box (peach-box)
  ((tabs :initarg :tabs
         :initform nil
         :accessor tabs)
   (body-content :initarg :body-content
                 :initform nil
                 :accessor body-content)))

(defmethod render ((widget peach-tab-box) &key)
  (setf (header-content widget)
        (with-html-string
          (:div :class "tabs"
                (:ul
                 (loop for (title) in (tabs widget)
                       for i from 0
                       do
                       (htm
                        (:li
                         (:a :href (format nil "#~a-tab-~a" (name widget) i)
                             (esc title)))))))))
  (setf (content widget)
        (with-html-string
          (loop for (nil content) in (tabs widget)
                for i from 0
                do
                (htm
                 (:div :class "tab-content"
                       :id (format nil "~a-tab-~a" (name widget) i)
                       (str content))))
          (str (body-content widget))))
  (call-next-method)
  (defer-js (format nil "$('#~a').createTabs();" (name widget))))


(defclass peach-header (widget)
    ((info-panel :initarg :info-panel
               :initform nil
               :accessor info-panel)))

(defmethod render ((widget peach-header) &key)
  (with-html
    (:div :id "header_toolbar"
          (:div :class "container_12"
                           
                (:h1 :class "grid_8" (str "Francos - Services Management System"))
                "<!-- Start of right part -->"
                (:div :class "grid_4"
                      (:div :class "toolbar_small"
                            (:div :class "toolbutton"
                                  ;;(:span "3") ;;message count must be displayed here
                                  (:img :src "/img/icons/16x16/sign-post.png" :width "16" :height "16" :alt "mail"))
                            (:div :class "toolbox"
                                  (:span :class "arrow")
                                  (:h3 "Info & Selection Panel") ;;these must be created dynamicly
                                  (:div :class "clear")
                                             

                                  (if (slot-val  widget 'info-panel)
                                      (str (render (slot-val  widget 'info-panel))))
                                  (:div :class "clear")
                                  (:a :class "inbox" :href "/ems/context" "Go to context &raquo;")))
                      (:div :class "toolbar_large"
                            (:div :class "toolbutton"
                                  (:div :class "toolicon"
                                        (:img :src "/img/icons/16x16/user.png" :width "16" :height "16" :alt "user"))
                                  (:div :class "toolmenu"
                                        (:div :class "toolcaption"
                                              (:span "System"))
                                        (:div :class "dropdown" :width "100%"
                                              (:ul 
                                               (:li 
                                                (:a :href "/ems/context" "Settings"))
                                               (:li 
                                                (:a :href "/ems/dashboard" "Dashboard"))
                                               (:li 
                                                (:a :href "/ems/users" "Users"))
                                               (:li 
                                                (:a :href "/ems/permissions" "Permissions"))
                                               (:li 
                                                (:a :href "/ems/entities" "Entities"))
                                               (:li 
                                                (:a :href "/ems/all-sorts" "Allsorts"))
                                               (:li 
                                                (:a :href "/ems/context" "Context"))
                                               (:li 
                                                (:a :href "/ems/importer" "Importer"))
                                               (:li 
                                                (:a :href "/ems/logout" "Logout"))))))))))
    (:nav :id "header_main"
          (:div :class "container_12"
                (:ul :id "nav_main"
    
                     (:li 
                      (:a :href "#"
                          (:img :src "/img/icons/25x25/dark/cog-5.png" :width "25" :height "25" :alt "")
                          "Framework")
                      (:ul
                       (:li 
                        (:a :href "/ems/clients" "Clients"))
                       (:li 
                        (:a :href "/ems/companies" "Companies"))                     
                       (:li 
                        (:a :href "/ems/service-users" "Service Users"))
                       
                       ))
                     (:li 
                      (:a :href "#"
                          (:img :src "/img/icons/25x25/dark/keyboard.png" :width "25" :height "25" :alt "")
                          "Inbox")
                      (:ul
                       (:li 
                        (:a :href "/ems/generic" "Inbox"))))
                     (:li 
                      (:a :href "#"
                          (:img :src "/img/icons/25x25/dark/chart-3.png" :width "25" :height "25" :alt "")
                          "Reports")
                      (:ul
                       (:li 
                        (:a :href "/ems/dashboard" "??????"))
                         )))) )
               
    (:div :id "nav_sub")))

(defclass bare-peach (widget)
  ((title :initarg :title
           :initform ""
           :accessor title)
   (author :initarg :author
           :initform ""
           :accessor author)
    (description :initarg :description
                 :initform ""
                 :accessor description)
    (classification :initarg :classification
                    :initform ""
                    :accessor classification)
    (key-words :initarg :key-words
               :initform ""
               :accessor key-words)
   (body-class :initarg :body-class
               :initform nil
               :accessor body-class))
  (:metaclass widget-class))


(defmethod render ((widget bare-peach) &key body styling bottom-java-script)
  (let ((title (or (title widget) (name widget))))
    (with-html-to-string ()
      "<!doctype html>"
       "<!-- paulirish.com/2008/conditional-stylesheets-vs-css-hacks-answer-neither/ -->"
       "<!--[if lt IE 7]> <html class=\"no-js lt-ie9 lt-ie8 lt-ie7\" lang=\"en\"> <![endif]-->"
       "<!--[if IE 7]>    <html class=\"no-js lt-ie9 lt-ie8\" lang=\"en\"> <![endif]-->"
       "<!--[if IE 8]>    <html class=\"no-js lt-ie9\" lang=\"en\"> <![endif]-->"
       "<!-- Consider adding a manifest.appcache: h5bp.com/d/Offline -->"
       "<!--[if gt IE 8]><!--> <html class=\"no-js\" lang=\"en\"> <!--<![endif]-->"
       (:head
        (:meta :charest "utf-8")
        (:link :rel "dns-prefetch" :href "//fonts.googleapis.com")
       
        "<!-- Use the .htaccess and remove these lines to avoid edge case issues. More info: h5bp.com/i/378 -->"
        (:meta :http-equiv "X-UA-Compatible" :content "IE=edge,chrome=1")
  
        (:title (esc title))
        (:meta :name "description" :content "")

        "<!-- Mobile viewport optimized: j.mp/bplateviewport -->"
        (:meta :name "viewport" :content "width=device-width,initial-scale=1")
        
        "<!-- More ideas for your <head> here: h5bp.com/d/head-Tips -->"
        (:meta :name "keywords" :content (get-val widget 'key-words))
        (:meta :name "author" :content (get-val widget 'author))
        (:meta :name "classification" :content (get-val widget 'classification))
        (:meta :name "description" :content (get-val widget 'description))
  
        "<!-- Place favicon.ico and apple-touch-icon.png in the root directory: mathiasbynens.be/notes/touch-icons -->"
        "<!-- CSS -->"
        (:link :rel "stylesheet" :href "/css/960gs/fluid.css") "<!-- 960.gs Grid System -->"


        "<!-- The HTML5-Boilerplate CSS styling -->"
        (:link :rel "stylesheet" :href "/css/h5bp/normalize.css") "<!-- RECOMMENDED: H5BP reset styles -->"
        (:link :rel "stylesheet" :href "/css/h5bp/non-semantic.helper.classes.css") "<!-- RECOMMENDED: H5BP helpers (e.g. .clear or .hidden) -->"
        (:link :rel "stylesheet" :href "/css/h5bp/print.styles.css") "<!-- OPTIONAL: H5BP print styles -->"
       
        "<!-- The main styling -->"
        (:link :rel "stylesheet" :href "/css/sprites.css") "<!-- STRONGLY RECOMMENDED: Basic sprites (e.g. buttons, jGrowl) -->"
        (:link :rel "stylesheet" :href "/css/content.css") "<!-- REQUIRED: Content styling -->"
        (:link :rel "stylesheet" :href "/css/typographics.css") " <!-- REQUIRED: Typographics -->"
        (:link :rel "stylesheet" :href "/css/ie.fixes.css") "<!-- OPTIONAL: Fixes for IE7 -->"
        (:link :rel "stylesheet" :href "/css/sprite.forms.css") "<!-- SPRITE: Forms styling -->"
       

        (if styling 
            (str styling))

        "<!-- Styling of JS plugins -->"
        (:link :rel "stylesheet" :href "/css/external/jquery-ui-1.8.16.custom.css") "<!-- PLUGIN: jQuery UI styling -->"


        " <!-- All JavaScript at the bottom, except this Modernizr build.
       Modernizr enables HTML5 elements & feature detects for optimal performance.
       Create your own custom Modernizr build: www.modernizr.com/download/ -->"
        (:script :src "/js/libs/modernizr-2.0.6.min.js")
 
        (str (page-include-css))
        (str (page-include-js))
        (str (page-include-bits)))
       (:body :class  (slot-val widget 'body-class)

        "<!-- Prompt IE 6 users to install Chrome Frame. Remove this if you support IE 6.
       chromium.org/developers/how-tos/chrome-frame-getting-started -->"
        
        "<!--[if lt IE 7]><p class=chromeframe>Your browser is <em>ancient!</em> <a href=\"http://browsehappy.com/\">Upgrade to a different browser</a> or <a href=\"http://www.google.com/chromeframe/?redirect=true\">install Google Chrome Frame</a> to experience this site.</p><![endif]-->"

        (str body)
              
        "<!-- JavaScript at the bottom for fast page loading -->"

              " <!-- Grab Google CDN's jQuery, with a protocol relative URL fall back to local if offline -->"
              (:script :src "//ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js")
              (:script "window.jQuery || document.write('<script src=\"/js/libs/jquery-1.7.1.min.js\"><\\/script>')" )
              

              "<script src=\"//ajax.googleapis.com/ajax/libs/jqueryui/1.8.16/jquery-ui.min.js\"></script>"
              "<script>window.jQuery.ui || document.write('<script src=\"/js/libs/jquery-ui-1.8.16.min.js\"><\\/script>')</script>"
              (str bottom-java-script)
              "<!-- scripts concatenated and minified via build script -->
                <script defer src=\"/js/plugins.js\"></script> <!-- REQUIRED: Different own jQuery plugnis -->"
              "<script defer src=\"/js/script.js\"></script> <!-- REQUIRED: Generic scripts -->"

              "<script defer>$(document).ready(function() {"
              (loop for i in (getf *widget-parameters* :javascript-defer)
                    do (str i)
                    (str ";"))
              "});</script>"

              "<!-- Prompt IE 6 users to install Chrome Frame. Remove this if you want to
                support IE 6.
                chromium.org/developers/how-tos/chrome-frame-getting-started -->"
              
              (:script :type "text/javascript"
                       :src "/js/ems/ajax.js")

              "<!--[if lt IE 7 ]>"
              "<script defer "
              "src=\"//ajax.googleapis.com/ajax/libs/chrome-frame/1.0.3/CFInstall.min.js\"></script>"
              "<script defer>window.attachEvent('onload',function(){CFInstall.check({mode:'overlay'})})</script>"
              "<![endif]-->")
       (str "</html>"))))

(defclass peach-page (widget)
  ((title :initarg :title
          :initform ""
          :accessor title)
   (author :initarg :author
           :initform ""
           :accessor author)
   (description :initarg :description
                :initform ""
                :accessor description)
   (classification :initarg :classification
                   :initform ""
                   :accessor classification)
   (key-words :initarg :key-words
              :initform ""
              :accessor key-words)
   (info-panel :initarg :info-panel
               :initform nil
               :accessor info-panel)
   (header :initarg :header
           :initform nil
           :accessor header))
  (:metaclass widget-class))


(defmethod render ((widget peach-page) &key body)
  (let ((page (make-widget 'bare-peach :name "peach-page"))
        (title (or (title widget) (name widget))))
 
    (setf (slot-value page 'title) title)
    (setf (slot-value page 'author) (slot-value widget 'author))
    (setf (slot-value page 'description) (slot-value widget 'description))
    (setf (slot-value page 'classification) (slot-value widget 'classification))
    (setf (slot-value page 'key-words) (slot-value widget 'key-words))
    (render page 
            :styling 
            (with-html-output-to-string (*standard-output* nil :indent t)
              (:link :rel "stylesheet" :href "/css/header.css") "<!-- REQUIRED: Header styling -->"
              (:link :rel "stylesheet" :href "/css/navigation.css") "<!-- REQUIRED: Navigation styling -->"
              (:link :rel "stylesheet" :href "/css/footer.css") "<!-- REQUIRED: Footer styling -->"           
              (:link :rel "stylesheet" :href "/css/sidebar.css") " <!-- OPTIONAL: Sidebar -->"
              (:link :rel "stylesheet" :href "/css/sprite.tables.css") "<!-- SPRITE: Tables styling -->"

              )
            :body
            (with-html-output-to-string (*standard-output* nil :indent t)
              (:div :id "height-wrapper"
                    (:header 
                     (if (get-val widget 'header)
                         (if (stringp (slot-val widget 'header))
                             (str (slot-val widget 'header))
                             (render (slot-val widget 'header)))
                         (render (make-widget 'peach-header :name "top-menu" 
                                                            :info-panel (slot-val widget 'info-panel)))))
                    
                    (:div :role "main" :class "container_12" :id "content-wrapper"
                          (str body))
                    (:div :class "clear")
                    (:div :class "push"))
        
              (:footer 
               (:div :class "container_12"
                     (str "Copyright &copy; 2012 Data X-Ware, all rights reserved.")
                     (:div :id "button_bar"
                           (:ul 
                            (:li
                             (:span (:a :href "/ems/dashboard" "Dashboard")))
                            (:li
                             (:span (:a :href "/ems/settings" "Settings"))))))))
            :bottom-java-script
            "<script defer src=\"/js/mylibs/jquery.ba-resize.js\"></script> <!-- RECOMMENDED when using sidebar: page resizing -->
             <script defer src=\"/js/mylibs/jquery.easing.1.3.js\"></script> <!-- RECOMMENDED: box animations -->
             <script defer src=\"/js/mylibs/jquery.validate.js\"></script>
             <script defer src=\"/js/mylibs/jquery.chosen.js\"></script>
             <script defer src=\"/js/mylibs/jquery.placeholder.js\"></script>
             <script defer src=\"/js/mylibs/jquery.ui.touch-punch.js\"></script> <!-- RECOMMENDED: touch compatibility -->
             <script defer src=\"/js/mylibs/jquery.checkbox.js\"></script>
             <script defer src=\"/js/mylibs/jquery.dataTables.js\"></script>")))


(defclass special-peach-page (widget)
  ((title :initarg :title
          :initform ""
          :accessor title)
   (author :initarg :author
           :initform ""
           :accessor author)
   (description :initarg :description
                :initform ""
                :accessor description)
   (classification :initarg :classification
                   :initform ""
                   :accessor classification)
   (key-words :initarg :key-words
              :initform ""
              :accessor key-words)
   (info-panel :initarg :info-panel
               :initform nil
               :accessor info-panel)
   (header :initarg :header
           :initform nil
           :accessor header))
  (:metaclass widget-class))


(defmethod render ((widget special-peach-page) &key body)
  (let ((page (make-widget 'bare-peach :name "peach-page"))
        (title (or (title widget) (name widget))))
    (setf (slot-value page 'title) title)
    (setf (slot-value page 'author) (slot-value widget 'author))
    (setf (slot-value page 'description) (slot-value widget 'description))
    (setf (slot-value page 'classification) (slot-value widget 'classification))
    (setf (slot-value page 'key-words) (slot-value widget 'key-words))
    (setf (slot-value page 'body-class) "special_page")
    (render page
            :styling 
            (with-html-to-string ()
              (:link :rel "stylesheet" :href "/css/special-page.css"))
            :body
            (with-html-to-string ()
              (:div :class "top"
                    (:div :class "gradient")
                    (:div :class "white")
                    (:div :class "shadow"))
              (:div :class "content"
                    (:h1 (str title))
                    (:div :class "background")
                    (:div :class "wrapper"
                          (str body)
                          (:div :class "shadow"))))
            :bottom-java-script
            (with-html-to-string ()
              "<script defer src=\"/js/mylibs/jquery.validate.js\"></script>
                <script defer src=\"/js/mylibs/jquery.jgrowl.js\"></script>
                <script defer src=\"/js/mylibs/jquery.checkbox.js\"></script>"))))

