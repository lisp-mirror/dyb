(in-package :ems)

(defclass html-framework-box (widget)
  ((grid-size :initarg :grid-size
               :initform 4
         :accessor grid-size)
   (header :initarg :header
           :accessor header)))



(defmethod render ((widget html-framework-box) &key content actions)
  (with-html-output-to-string (*standard-output* nil :indent t)
    (:div :class (format nil "grid_~A" (get-val widget 'grid-size))
          (:div :class "box"
                (:div :class "header"
                      (:img :src "" :alt "" :width 16 :height 16)
                      (:h3 (str (get-val widget 'header)))
                      (:span))
                (:div :class "content"
                      (str content))
                (:div :class "actions"
                      (str actions)
                      )))))


(defclass html-framework-form (widget)
  ((grid-size :initarg :grid-size
              :initform 4
         :accessor grid-size)
   (header :initarg :header
           :accessor header)))

(defmethod render ((widget html-framework-form) &key content)
  (with-html-output-to-string (*standard-output* nil :indent t)
    (:div :class (format nil "grid_~A" (get-val widget 'grid-size))
          (:div :class "box"
                (:div :class "header"
                      (:img :src "" :alt "" :width 16 :height 16)
                      (:h3 (str (get-val widget 'header)))
                      (:span))
                (:form :class "validate" :method "post"
                       (:div :class "content no-padding"
                             (str content))
                       (:div :class "actions"
                             (:div :class "actions-left"
                                   (:input :name "reset" :type "reset"))
                             (:div :class "actions-right"
                                   (:input :name "submit" :type "submit")))
                       

                       )))))

(defclass form-section (widget)
  (
   (section-size :initarg :section-size
                 :initform 100
                 :accessor section-size))
  )

(defmethod render ((widget form-section) &key label input)
  (with-html-output-to-string (*standard-output* nil :indent t)
    (:div :class (format nil "section _~A" (get-val widget 'section-size)) 
          (:label 
           (str label))
          (:div 
           (str input)
           ))))


(defclass html-framework-page (widget)
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
              :accessor key-words))
  (:metaclass widget-class))


(defmethod render ((widget html-framework-page) &key body)
  (let ((title (or (title widget) (name widget))))
 
    (with-html-output-to-string (*standard-output* nil :indent t)
      
       (str "<!doctype html>")
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

        "<!-- Mobile viewport optimized: h5bp.com/viewport -->"
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
        (:link :rel "stylesheet" :href "/css/header.css") "<!-- REQUIRED: Header styling -->"
        (:link :rel "stylesheet" :href "/css/navigation.css") "<!-- REQUIRED: Navigation styling -->"
        (:link :rel "stylesheet" :href "/css/content.css") "<!-- REQUIRED: Content styling -->"
        (:link :rel "stylesheet" :href "/css/footer.css") "<!-- REQUIRED: Footer styling -->"
        (:link :rel "stylesheet" :href "/css/typographics.css") " <!-- REQUIRED: Typographics -->"
        (:link :rel "stylesheet" :href "/css/ie.fixes.css") "<!-- OPTIONAL: Fixes for IE7 -->"

        (:link :rel "stylesheet" :href "/css/sprite.forms.css") "<!-- SPRITE: Forms styling -->"
        (:link :rel "stylesheet" :href "/css/sprite.lists.css") "<!-- SPRITE: Lists styling -->"
        (:link :rel "stylesheet" :href "/css/icons.css") "<!-- Icons -->"


        "<!-- Styling of JS plugins -->"
        (:link :rel "stylesheet" :href "/css/external/jquery-ui-1.8.16.custom.css") "<!-- PLUGIN: jQuery UI styling -->"
	
        " <!-- All JavaScript at the bottom, except this Modernizr build.
       Modernizr enables HTML5 elements & feature detects for optimal performance.
       Create your own custom Modernizr build: www.modernizr.com/download/ -->"
        (:script :src "/js/libs/modernizr-2.0.6.min.js")
 
        (str (page-include-css))
        (str (page-include-js))
        (str (page-include-bits))
        )
       (:body

        "<!-- Prompt IE 6 users to install Chrome Frame. Remove this if you support IE 6.
       chromium.org/developers/how-tos/chrome-frame-getting-started -->"
        
        "<!--[if lt IE 7]><p class=chromeframe>Your browser is <em>ancient!</em> <a href=\"http://browsehappy.com/\">Upgrade to a different browser</a> or <a href=\"http://www.google.com/chromeframe/?redirect=true\">install Google Chrome Frame</a> to experience this site.</p><![endif]-->"
        (:div :id "height-wrapper"

              (:header 
               (:div :id "header_toolbar"
                     (:div :class "container_12"
                                   
                           (:h1 :class "grid_8" (str "XPACKS V2 - The social responsibility management and reporting tool."))
                           "<!-- Start of right part -->"
                           (:div :class "grid_4"
                                 (:div :class "toolbar_small"
                                       (:div :class "toolbutton"
                                             (:span "3") ;;message count must be displayed here
                                             (:img :src "/img/icons/16x16/mail.png" :width "16" :height "16" :alt "mail"))
                                       (:div :class "toolbox"
                                             (:span :class "arrow")
                                             (:h3 "Your Messages") ;;these must be created dynamicly
                                             (:ul :class "mail"
                                                  (:li 
                                                   (:a :href "#"
                                                       (:strong "10.15") "Need your help!" (:small "From: John")))
                                                  (:li 
                                                   (:a :href "#"
                                                       (:strong "10.15") "Need your help!" (:small "From: John")))
                                                  (:li 
                                                   (:a :href "#"
                                                       (:strong "10.15") "Need your help!" (:small "From: John"))))
                                             (:a :class "inbox" :href "#" "Go to inbox &raquo;")))
                                 (:div :class "toolbar_large"
                                       (:div :class "toolbutton"
                                             (:div :class "toolicon"
                                                   (:img :src "/img/icons/16x16/user.png" :width "16" :height "16" :alt "user"))
                                             (:div :class "toolmenu"
                                                   (:div :class "toolcaption"
                                                         (:span "Administrator"))
                                                   (:div :class "dropdown"
                                                         (:ul 
                                                          (:li 
                                                           (:a :href "#" "Settings"))
                                                          (:li 
                                                           (:a :href "#" "Logout")))))))))
                              
                     )
               (:nav :id "header_main"
                     (:div :class "container_12"
                           (:ul :id "nav_main"
                                (:li 
                                 (:a :href "#"
                                     (:img :src "/img/icons/25x25/dark/computer-imac.png" :width 25 :height 25 :alt "")
                                     "Dasboard")
                                 (:ul
                                  (:li 
                                   (:a :href "/ems/dashboard" (str "Dashboard")))
                                  
                                  ))
                                (:li 
                                 (:a :href "#"
                                     (:img :src "/img/icons/25x25/dark/computer-imac.png" :width 25 :height 25 :alt "")
                                     "Inbox")
                                 (:ul
                                  (:li 
                                   (:a :href "/ems/combined-inbox" (str "All Channels Inbox")))
                                  (:li 
                                   (:a :href "/ems/facebook-inbox" (str "Facebook")))
                                  (:li 
                                   (:a :href "/ems/google-twitter-inbox" (str "Twitter")))
                                  (:li 
                                   (:a :href "/ems/google-plus-inbox" (str "Google+")))
                                  (:li 
                                   (:a :href "/ems/linkedin-inbox" (str "LinkedIn")))
                                  
                                  ))
                                (:li 
                                 (:a :href "#"
                                     (:img :src "/img/icons/25x25/dark/computer-imac.png" :width 25 :height 25 :alt "")
                                     "Analytics")
                                 (:ul
                                  (:li 
                                   (:a :href "/ems/google-analytics" (str "Google Analytics")))
                                  (:li 
                                   (:a :href "/ems/google-facebook-analytics" (str "Facebook")))
                                  (:li 
                                   (:a :href "/ems/google-twitter-analytics" (str "Twitter")))
                                  (:li 
                                   (:a :href "/ems/google-plus-analytics" (str "Google+")))
                                  (:li 
                                   (:a :href "/ems/google-linkedin-analytics" (str "LinkedIn")))
                                  
                                  ))
                                (:li 
                                 (:a :href "#"
                                     (:img :src "/img/icons/25x25/dark/user-comment.png" :width "25" :height "25" :alt "")
                                     "Users")
                                 (:ul
                                  (:li 
                                   (:a :href "/ems/system-users" "System Users"))
                                  (:li 
                                   (:a :href "/ems/service-users" "Twitter Users"))
                                  (:li 
                                   (:a :href "/ems/service-users" "Facebook Users"))
                                  (:li 
                                   (:a :href "/ems/service-users" "Google Users"))
                                  (:li 
                                   (:a :href "/ems/service-users" "Google Analytics Users"))))))
                     )
               
               (:div :id "nav_sub")
               )

              (:div :role "main" :class "container_12" :id "content-wrapper"
                    (:div :id "main_content"
                          (:h2 :class "grid_12" (str title))
                          (:div :class "clean")
                          (str body)
                          )
                    (:div :class "clear"))
              
              )
        
        (:footer 
         (:div :class "container_12"
               (str "Copyright &copy; 2012 Data X-Ware, all rights reserved.")
               (:div :id "button_bar"
                     (:ul 
                      (:li
                       (:span (:a :href "/ems/dashboard" "Dashboard")))
                      (:li
                       (:span (:a :href "/ems/settings" "Settings")))
                      ))))
              
        "<!-- JavaScript at the bottom for fast page loading -->"

              " <!-- Grab Google CDN's jQuery, with a protocol relative URL fall back to local if offline -->"
              (:script :src "//ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js")
              (:script "window.jQuery || document.write('<script src=\"/js/libs/jquery-1.7.1.min.js\"><\\/script>')" )
              

              "<script src=\"//ajax.googleapis.com/ajax/libs/jqueryui/1.8.16/jquery-ui.min.js\"></script>"
              "<script>window.jQuery.ui || document.write('<script src=\"/js/libs/jquery-ui-1.8.16.min.js\"><\\/script>')</script>"
              "<!-- scripts concatenated and minified via build script -->
		<script defer src=\"/js/plugins.js\"></script> <!-- REQUIRED: Different own jQuery plugnis -->
		<script defer src=\"/js/mylibs/jquery.ba-resize.js\"></script> <!-- RECOMMENDED when using sidebar: page resizing -->
		<script defer src=\"/js/mylibs/jquery.easing.1.3.js\"></script> <!-- RECOMMENDED: box animations -->
		<script defer src=\"/js/mylibs/jquery.ui.touch-punch.js\"></script> <!-- RECOMMENDED: touch compatibility -->
		<script defer src=\"js/mylibs/jquery.chosen.js\"></script>
		<script defer src=\"js/mylibs/jquery.validate.js\"></script>
		<script defer src=\"/js/script.js\"></script> <!-- REQUIRED: Generic scripts -->
		<!-- end scripts -->"

		" <script>
			$(window).load(function() {
				$('#accordion').accordion();
				$(window).resize();
			});
		</script>"

              "<!-- Prompt IE 6 users to install Chrome Frame. Remove this if you want to
		support IE 6.
		chromium.org/developers/how-tos/chrome-frame-getting-started -->"
              
              "<!--[if lt IE 7 ]>"
              "<script defer "
              "src=\"//ajax.googleapis.com/ajax/libs/chrome-frame/1.0.3/CFInstall.min.js\"></script>"
              "<script defer>window.attachEvent('onload',function(){CFInstall.check({mode:'overlay'})})</script>"
              "<![endif]-->"
              
              )
       (str "</html>")
       )))