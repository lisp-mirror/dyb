(in-package :ems)

(defclass html5-page (widget)
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


(defmethod render ((widget html5-page) &key body header footer)
  (let ((title (or (title widget) (name widget))))
 
    (with-html-to-string ()
      
       (str "<!doctype html>")
       "<!-- paulirish.com/2008/conditional-stylesheets-vs-css-hacks-answer-neither/ -->"
       (str "<!--[if lt IE 7]> <html class=\"no-js lt-ie9 lt-ie8 lt-ie7\" lang=\"en\"> <![endif]-->")
       (str "<!--[if IE 7]>    <html class=\"no-js lt-ie9 lt-ie8\" lang=\"en\"> <![endif]-->")
       (str "<!--[if IE 8]>    <html class=\"no-js lt-ie9\" lang=\"en\"> <![endif]-->")
       (str "<!-- Consider adding a manifest.appcache: h5bp.com/d/Offline -->")
       (str "<!--[if gt IE 8]><!--> <html class=\"no-js\" lang=\"en\"> <!--<![endif]-->")

       (:head
        (:meta :charest "utf-8")

        "<!-- Use the .htaccess and remove these lines to avoid edge case issues. More info: h5bp.com/i/378 -->"
        (:meta :http-equiv "X-UA-Compatible" :content "IE=edge,chrome=1")
  
        (:title (esc title))
        (:meta :name "description" :content "")

        "<!-- Mobile viewport optimized: h5bp.com/viewport -->"
        (:meta :name "viewport" :content "width=device-width")
        
        "<!-- Place favicon.ico and apple-touch-icon.png in the root directory: mathiasbynens.be/notes/touch-icons -->"

        (:link :rel "stylesheet" :href "/css/style.css")

        "<!-- More ideas for your <head> here: h5bp.com/d/head-Tips -->"
        (:meta :name "keywords" :content (slot-val widget 'key-words))
        (:meta :name "author" :content (slot-val widget 'author))
        (:meta :name "classification" :content (slot-val widget 'classification))
        (:meta :name "description" :content (slot-val widget 'description))
  
        " <!-- All JavaScript at the bottom, except this Modernizr build.
       Modernizr enables HTML5 elements & feature detects for optimal performance.
       Create your own custom Modernizr build: www.modernizr.com/download/ -->"
        (:script :src "/js/libs/modernizr-2.5.2.min.js")
 
        (str (page-include-css))
        (page-include-js)
        (str (page-include-bits))
        )
       (:body

        "<!-- Prompt IE 6 users to install Chrome Frame. Remove this if you support IE 6.
       chromium.org/developers/how-tos/chrome-frame-getting-started -->"
  
        (str "<!--[if lt IE 7]><p class=chromeframe>Your browser is <em>ancient!</em> <a href=\"http://browsehappy.com/\">Upgrade to a different browser</a> or <a href=\"http://www.google.com/chromeframe/?redirect=true\">install Google Chrome Frame</a> to experience this site.</p><![endif]-->")

        (if header
            (htm (:header (str header))))

        (:div :role "main"
              (str body))

        (if footer
            (htm (:footer (str footer))))
        
        "<!-- JavaScript at the bottom for fast page loading -->"

          " <!-- Grab Google CDN's jQuery, with a protocol relative URL"; fall back to local if offline -->"
          (:script :src "//ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js")
          (:script "window.jQuery || document.write('<script src=\"/js/libs/jquery-1.7.1.min.js\"><\/script>')" )
         
          "<!-- scripts concatenated and minified via build script -->"
          (:script :src "/js/plugins.js")
          (:script :src "/js/script.js")
        
          
          "<!-- end scripts -->"

        )
       (str "</html>")
       )))