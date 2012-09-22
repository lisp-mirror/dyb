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

(defclass html-framework-box (widget)
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

(defmethod render ((box html-framework-box) &key)
  (with-html
    (:div :class "widget-block"
          (:div :class "widget-head"
                (:h3 (esc (header box))))
          (:div :class "widget-content"
                (:div :class "widget-box"
                      (str (content box)))))))

(defclass html-framework-form (widget)
  ((grid-size :initarg :grid-size
              :initform 6
              :accessor grid-size)
   (header :initarg :header
           :accessor header)
   (form-id :initarg :form-id
            :accessor form-id)
   (grid-name :initarg :grid-name
            :accessor grid-name)
   (parent-grid :initarg :parent-grid
                :accessor parent-grid)))

(defmethod render ((widget html-framework-form) &key content grid)
  (with-html
    (:div :class "nonboxy-widget"
          (:div :class "widget-head"
                (:h3 (str (get-val widget 'header))))
          (:div :class "widget-content"
                (:div :class "widget-box"
                      (:form :name (get-val widget 'form-id)
                             :id (get-val widget 'form-id)
                             :class "form-horizontal well"
                             :method "post"
                             :onsubmit "return false;"
                             (:fieldset
                              (:input :type "hidden" :name "form-id"
                                      :value (get-val widget 'form-id))

                              (if (get-val widget 'grid-name)
                                  (htm (:input :type "hidden" :name "grid-name"
                                               :value (get-val widget 'grid-name))))

                              (str content)

                              (:div :class "form-actions"
                                    (:button
                                     :class "btn btn-info"
                                     :onclick
                                     (format nil
                                             "if($(\"#~a\").valid()){~a}"
                                             (get-val widget 'form-id)
                                             
                                             (js-render-form-values 
                                              (editor grid)
                                              (get-val widget 'form-id)
                                              (js-pair "grid-name" (name grid))
                                              (js-pair "action" "save")))
                                     "Save")
                                    (:button :class "btn btn-warning"
                                             :onclick
                                             (js-render (editor grid)
                                                        (js-pair "grid-name" (name grid))
                                                        (js-pair "action" "cancel"))
                                             "Cancel")))))))))

(defclass form-section (widget)
  ((section-size :initarg :section-size
                 :initform 100
                 :accessor section-size)))

(defmethod render ((widget form-section) &key label input label-for)
  (with-html
    (:div :class "control-group"
          (:label :class "control-label" :for label-for 
                  (str label))
          (:div :class "controls"
                (str input)))))

(defclass html-framework-tab-box (html-framework-box)
  ((tabs :initarg :tabs
         :initform nil
         :accessor tabs)
   (body-content :initarg :body-content
                 :initform nil
                 :accessor body-content)))

(defmethod render ((widget html-framework-tab-box) &key)
  (with-html
    (:div :class "box-tab"
          (:div :class "tabbalbe tabs-right"
                (:ul :class "nav nav-tabs"
                     (loop for (title) in (tabs widget)
                           for i from 0
                           do
                           (htm
                            (:li
                             (:a :data-toggle "tab" 
                                 :href (format nil "#~a-tab-~a" (name widget) i)
                                 (esc title))))))
                (:div :class "tab-content"
                      (loop for (nil content) in (tabs widget)
                            for i from 0
                            do
                            (htm
                             (:div :class "tab-pane"
                                   :id (format nil "~a-tab-~a" (name widget) i)
                                   (str content)))))))))


(defclass html-framework-header (widget)
    ((info-panel :initarg :info-panel
               :initform nil
               :accessor info-panel)))

(defmethod render ((widget html-framework-header) &key)
  (with-html
    (:div :class "navbar navbar-fixed-top"
          (:div :class "navbar-inner top-nav"
                (:div :class "container-fluid"
                      (:div :class "branding"
                            (:div :class "logo"
                                  (:img :style "width:50%;" :src "/appimg/dyb-logo.png")))
                      (:ul :class "nav pull-right"
                           (:li :class "dropdown"
                                (:a :class "dropdown-toggle" :href "#" :data-toggle "dropdown"
                                    (str (get-val (current-user) 'email))
                                    (:span :class "alert-noty" (str "25"))
                                    (:i :class "white-icons admin_user")
                                    (:b :class "caret"))
                                (:ul :class "dropdown-menu"
                                     (:li
                                      (:a :href "/ems/dashboard" "Dashboard"))
                                     (:li
                                      (:a :href "/ems/users" "Users"))
                                     (:li
                                      (:a :href "/ems/country-town" "Country/Town"))
                                     (:li
                                      (:a :href "/ems/permissions" "Permissions"))
                                     (:li
                                      (:a :href "/ems/root-entities" "Root Entities"))
                                     (:li
                                      (:a :href "/ems/entities" "Entities"))
                                     (:li
                                      (:a :href "/ems/all-sorts" "Allsorts"))
                                     (:li
                                      (:a :href "/ems/context" "Context"))
                                     (:li
                                      (:a :href "/ems/importer" "Importer"))
                                     (:li :class "divider")
                                     (:li
                                      (:a :href "/ems/logout"
                                          (:i :class "icon-off")
                                          (str "Logout")))))))))))

(defclass bare-html-framework (widget)
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


(defmethod render ((widget bare-html-framework) &key body styling bottom-java-script)
  (let ((title (or (title widget) (name widget))))
    (with-html-string
      "<!doctype html>
       <html lang=\"en\">"
      (:head
       (:meta :charest "utf-8")
       (:link :rel "dns-prefetch" :href "//fonts.googleapis.com")
       
       ;; Use the .htaccess and remove these lines to avoid edge case issues. More info: h5bp.com/i/378
       (:meta :http-equiv "X-UA-Compatible" :content "IE=edge,chrome=1")

       (:title (esc title))
       (:meta :name "description" :content "")

       ;; Mobile viewport optimized: j.mp/bplateviewport
       (:meta :name "viewport" :content "width=device-width,initial-scale=1.0")

       ;; More ideas for your <head> here: h5bp.com/d/head-Tips
       (:meta :name "keywords" :content (get-val widget 'key-words))
       (:meta :name "author" :content (get-val widget 'author))
       (:meta :name "classification" :content (get-val widget 'classification))
       (:meta :name "description" :content (get-val widget 'description))

       (:link :rel "stylesheet" :href "/css/bootstrap.css")
       (:link :rel "stylesheet" :href "/css/bootstrap-responsive.css")
       (:link :rel "stylesheet" :href "/css/prettify.css")
       (:link :rel "stylesheet" :href "/css/jquery-ui-1.8.16.custom.css")

       (:link :rel "stylesheet" :href "/css/styles.css")
       (:link :rel "stylesheet" :href "/css/icons-sprite.css")
       (:link :rel "stylesheet" :type "text/css" :href "http://ajax.googleapis.com/ajax/libs/jqueryui/1.8.11/themes/base/jquery-ui.css")
   
       (:link :rel "stylesheet" :type "text/css" :href "http://static.jquery.com/ui/css/demo-docs-theme/ui.theme.css")

    


       (:link :id "themes" :rel "stylesheet" :href "/css/theme-blue.css")
       (:style 
        "div.ui-datepicker{
 font-size:12px;
}
")

       "<script type=\"text/javaScript\">
function timedRefresh(timeoutPeriod) {
var okToRefresh = confirm(\"Do you really want to refresh the page?\");
if (okToRefresh)
	{
			setTimeout(\"window.location.reload(true);\",timeoutPeriod);
	}

}
</script>"

       "<!--[if IE 7]>
       <link rel= \" stylesheet\" type=\"text/css\" href=\"/css/ie/ie7.css\" />
       <![endif]-->
       <!--[if IE 8]>
       <link rel=\"stylesheet\" type=\"text/css\" href=\"/css/ie/ie8.css\" />
       <![endif]-->
       <!--[if IE 9]>
       <link rel=\"stylesheet\" type=\"text/css\" href=\"/css/ie/ie9.css\" />
       <![endif]-->"

       "<!--fav and touch icons -->"
       (:link :rel "shortcut icon" :href "/ico/favicon.ico")

       (if styling
           (str styling))

       (page-include-css)
       (page-include-js)
       (page-include-bits)
       )

      (:body ;;:onload "timedRefresh(30000)"
       (str body)





      "<!-- javascript
       ================================================== -->
       <!-- Placed at the end of the document so the pages load faster -->"
#|      (:script :src "/js/jquery.js")
                                                   (:script :src "/js/jquery-ui-1.8.16.custom.min.js")
                                                   (:script :src "/js/bootstrap.min.js")
                                                   (:script :src "/js/prettify.js")
                                                   (:script :src "/js/jquery.sparkline.min.js")
                                                   (:script :src "/js/accordion.jquery.js")
                                                   (:script :src "/js/jquery.nicescroll.min.js")
                                                   |#

"<script src=\"/js/jquery.js\"></script>
      <script type='text/javascript' src=\"http://ajax.googleapis.com/ajax/libs/jqueryui/1.8.11/jquery-ui.min.js\"></script>
      <script type='text/javascript' src=\"https://ajax.googleapis.com/ajax/libs/jquery/1.7.2/jquery.min.js\"></script>

<script src=\"/js/jquery-ui-1.8.16.custom.min.js\"></script>
<script src=\"/js/bootstrap.js\"></script>
<script src=\"/js/prettify.js\"></script>
<script src=\"/js/jquery.sparkline.min.js\"></script>
<script src=\"/js/jquery.nicescroll.min.js\"></script>
<script src=\"/js/accordion.jquery.js\"></script>
<script src=\"/js/smart-wizard.jquery.js\"></script>
<script src=\"/js/vaidation.jquery.js\"></script>
<script src=\"/js/jquery-dynamic-form.js\"></script>
<script src=\"/js/fullcalendar.js\"></script>
<script src=\"/js/raty.jquery.js\"></script>
<script src=\"/js/jquery.noty.js\"></script>
<script src=\"/js/jquery.cleditor.min.js\"></script>
<script src=\"/js/data-table.jquery.js\"></script>
<script src=\"/js/TableTools.min.js\"></script>
<script src=\"/js/ColVis.min.js\"></script>
<script src=\"/js/plupload.full.js\"></script>
<script src=\"/js/elfinder/elfinder.min.js\"></script>
<script src=\"/js/chosen.jquery.js\"></script>
<script src=\"/js/uniform.jquery.js\"></script>
<script src=\"/js/jquery.tagsinput.js\"></script>
<script src=\"/js/jquery.colorbox-min.js\"></script>
<script src=\"/js/check-all.jquery.js\"></script>
<script src=\"/js/inputmask.jquery.js\"></script>
<script src=\"http://bp.yahooapis.com/2.4.21/browserplus-min.js\"></script>
<script src=\"/js/plupupload/jquery.plupload.queue/jquery.plupload.queue.js\"></script>
<script src=\"/js/excanvas.min.js\"></script>
<script src=\"/js/jquery.jqplot.min.js\"></script>
<script src=\"/js/chart/jqplot.highlighter.min.js\"></script>
<script src=\"/js/chart/jqplot.cursor.min.js\"></script>
<script src=\"/js/chart/jqplot.dateAxisRenderer.min.js\"></script>
<script src=\"/js/custom-script.js\"></script>"




;;" <script src=\"js/jquery.noty.js\"></script>"
;;" <script src=\"/js/custom-script.js\"></script>"

       "<!-- html5.js for IE less than 9 -->
       <!--[if lt IE 9]>
       <script src=\" http://html5shim.googlecode.com/svn/trunk/html5.js\" ></script>
       <![endif]-->
       <!-- css3-mediaqueries.js for IE less than 9 -->
       <!--[if lt IE 9]>
       <script src=\"http://css3-mediaqueries-js.googlecode.com/svn/trunk/css3-mediaqueries.js\"></script>
       <![endif]-->"


       (str bottom-java-script)


"<script>
	$(function() {

		var name = $( \"#name\" ),
			email = $( \"#email\" ),
			password = $( \"#password\" ),
			allFields = $( [] ).add( name ).add( email ).add( password );
			

		
		$( \"[dialog-form]\" ).dialog({
			autoOpen: false,
			height: 300,
			width: 350,
			modal: true,
			buttons: {
				\"Create an account\": function() {
			
		//put save here
						$( this ).dialog( \"close\" );
					
				},
				Cancel: function() {
					$( this ).dialog( \"close\" );
				}
			},
			close: function() {
				allFields.val( \"\" ).removeClass( \"ui-state-error\" );
			}
		});

		$( \"#create-user\" )
			.button()
			.click(function() {
				$( \"#dialog-form\" ).dialog( \"open\" );
			});
	});
	</script>"

       (:script "$(\"#inline-datepicker\").datepicker({
   onSelect: function(dateText, inst) { 
      var dateAsString = dateText; //the first parameter of this function
      var dateAsObject = $(this).datepicker( 'getDate' ); //the getDate method
      //alert (dateText);
      //$(\"#inline-datepicker\").submit();
      window.location.replace(\"/ems/generic-scheduler\");

   }
});
")
       
       (:script :defer t :src "/appjs/ajax.js")       
       (:script (str (deferred-js)))

       )

       " <script src=\"/js/respond.min.js\"></script>
         <script src=\"/js/ios-orientationchange-fix.js\"></script>"


      "</html>")))

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
              :accessor key-words)
   (info-panel :initarg :info-panel
               :initform nil
               :accessor info-panel)
   (header :initarg :header
           :initform nil
           :accessor header))
  (:metaclass widget-class))


(defmethod render ((widget html-framework-page) &key body)
  (let ((page (make-widget 'bare-html-framework :name "html-framework-page"))
        (title (or (title widget) (name widget))))

    (setf (slot-value page 'title) title)
    (setf (slot-value page 'author) (slot-value widget 'author))
    (setf (slot-value page 'description) (slot-value widget 'description))
    (setf (slot-value page 'classification) (slot-value widget 'classification))
    (setf (slot-value page 'key-words) (slot-value widget 'key-words))
    (render page
            :styling ""


            :body
            (with-html-output-to-string (*standard-output* nil :indent t)
              (if (get-val widget 'header)
                  (if (stringp (slot-val widget 'header))
                      (str (slot-val widget 'header))
                      (render (slot-val widget 'header)))
                  (render (make-widget 'html-framework-header :name "top-menu"
                                       :info-panel (slot-val widget 'info-panel))))
              
              (:div :id "sidebar" :style "overflow-y:hidden;cursor:-moz-grab;"
                    :tabindex "5000"
                    
                    (:ul :class "side-nav accordion_mnu collapsible"
                         (:li 
                          (:a :href "/ems/dashboard"
                              (:span :class "white-icons computer_imac")
                              (str "Dashboard")))
                         (:li
                          (:a :href "#"
                              (:span :class "white-icons computer_imac")
                              "Framework")
                          (:ul :class "acitem" :style "display:none;"
                               (:li 
                                (:a :href "/ems/clients" 
                                    (:span :class "sidenav-icon"
                                           (:span :class "sidenav-link-color"))
                                    "Clients"))
                               (:li 
                                (:a :href "/ems/companies" 
                                    (:span :class "sidenav-icon"
                                           (:span :class "sidenav-link-color"))
                                    "Companies"))                     
                               (:li 
                                (:a :href "/ems/service-users" 
                                    (:span :class "sidenav-icon"
                                           (:span :class "sidenav-link-color"))
                                    "Service Users"
                                    ))
                               (:li
                                (:a :href "/ems/search-stream" 
                                    (:span :class "sidenav-icon"
                                           (:span :class "sidenav-link-color"))
                                    "Search Streams"))
                               ))
                         (:li
                          (:a :href "#"
                              (:span :class "white-icons mail")
                              "Inbox")
                          (:ul :class "acitem" :style "display:none;"

                               (:li
                                (:a :href "/ems/generic" "Inbox"))
                       
                               
                       
                               
                               ))
                         (:li
                          (:a :href "#"
                              (:span :class "white-icons month_calendar")
                              "Scheduler")
                          (:ul :class "acitem" :style "display:none;"

                                                     
                               (:li
                                (:a :href "/ems/generic-scheduler" "Scheduler"))
                       
                               
                               ))
                         (:li
                          (:a :href "#"
                              (:span :class "white-icons magnifying_glass")
                              "Search Streams")
                          (:ul :class "acitem" :style "display:none;"

                               (:li
                                (:a :href "/ems/search-stream" "Search Streams"))
                       
                               
                       
                               (:li
                                (:a :href "/ems/search-stream-feedback" "Search Stream Data"))
                               ))
                         )
                    (:div :id "side-accordion"
                          (:div :class "accordion-group"
                                (:div :class "accordion-header"
                                      (:a :class "accordion-toggle" :href "#collapseOne"
                                          :data-parent "#side-accordion"
                                          :data-toggle "collapse"
                                          (:i :class "nav-icon month_calender")
                                          (str "Todays Event")))
                                (:div :class "accordion-content"
                                      (:div :id "inline-datepicker" :altField "shit" ;:style "font-size:8px;"
                                           
                                            :value "12-02-2012"
                                            (:intput :id "shit" :type "hidden" ;;:onchange "(alert 'shit');";; "document.getElementById(\"shit\").submit();"
                                                     ))))))
              
              (:div :id "main-content"
                    (:div :class "container-fluid"
                          
                          (str body))))
            :bottom-java-script
            "")))


(defclass special-html-framework-page (widget)
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


(defmethod render ((widget special-html-framework-page) &key body)
  (let ((page (make-widget 'bare-html-framework :name "html-framework-page"))
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
