(in-package :dyb)

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

(defclass html-framework-box (box)
  ((grid-size :initarg :grid-size
              :initform nil
              :accessor grid-size)))

(register-widget *dyb-theme* 'box 'html-framework-box)

(defmethod render ((box html-framework-box) &key)
  (with-html
    (:div :class (if (grid-size box)
                     (format nil "widget-block span-~A" (grid-size box))
                     "widget-block")
          (:div :class "widget-head"
                (:h5 (esc (header box)))
                (when (header-content box)
                  (str (header-content box))))
          (:div :class "widget-content"
                (:div :class "widget-box"
                      (str (content box)))))))


(defclass html-simple-framework-form (widget)
  ((grid-size :initarg :grid-size
              :initform 6
              :accessor grid-size)
   (header :initarg :header
           :initform nil
           :accessor header)
   (action :initarg :action
           :initform nil
           :accessor action)
   (action-title :initarg :action-title
                 :initform nil
                 :accessor action-title)
   (form-id :initarg :form-id
            :initform nil
            :accessor form-id)
   (ajax-render-widget :initarg :ajax-render-widget
                       :initform nil
                       :accessor ajax-render-widget)))

(defmethod render ((widget html-simple-framework-form) &key content)
  (with-html
    (:div :class (format nil "nonboxy-widget span-~A" (grid-size widget))
          (when (header widget)
            (htm
             (:div :class "widget-head"
                   (:h5 (esc (header widget))))))
          (:div :class "widget-content"
                (:form :name (form-id widget)
                       :id (form-id widget)
                       :class "form-horizontal well"
                       :method "post"
                       :onsubmit "return false;"
                       (:fieldset
                        (:input :type "hidden" :name "form-id"
                                :value (form-id widget))
                        (str content)
                        (:div :class "form-actions"
                              (:button
                               :class "btn btn-info"
                               :onclick
                               (format nil
                                       "if($(\"#~a\").valid()){~a};"
                                       (form-id widget)
                                       (js-render-form-values
                                        (or (ajax-render-widget widget)
                                            widget)
                                        (form-id widget)
                                        (js-pair "action" (action widget))))
                               (esc (action-title widget))))))))))

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
                :accessor parent-grid)
   (form-data :initarg :form-data
              :initform nil
              :accessor form-data)
   (ajax-submit :initarg :ajax-submit
                :initform t
                :accessor ajax-submit)))

(defmethod render ((widget html-framework-form) &key content grid
                                                     buttons)
  (with-html
    (:div :class "nonboxy-widget"
          (:div :class "widget-head"
                (:h3 (str (get-val widget 'header))))
          (:div :class "widget-content"
                (:div :class "widget-box"
                      (:form :name (get-val widget 'form-id)
                             :id (get-val widget 'form-id)
                             :class "form-horizontal well"
                             :action (when (ajax-submit widget)
                                       "")
                             :method "post"
                             :onsubmit (when (ajax-submit widget)
                                         "return false;")
                             :enctype (and (form-data widget)
                                           "multipart/form-data")
                             (:fieldset
                              (:input :type "hidden" :name "form-id"
                                      :value (get-val widget 'form-id))
                              (unless (ajax-submit widget)
                                (htm
                                 (:input :type "hidden" :name "action"
                                         :value "save")
                                 (:input :type "hidden" :name "grid-name"
                                         :value (name grid))))
                              (if (get-val widget 'grid-name)
                                  (htm (:input :type "hidden" :name "grid-name"
                                               :value (get-val widget 'grid-name))))

                              (str content)
                              
                              (:div :class "form-actions"
                                    (if buttons
                                        (str buttons)
                                        (with-html
                                          (:button
                                           :class "btn btn-info"
                                           :onclick
                                           (format nil
                                                   "if($(\"#~a\").valid()){~a}"
                                                   (get-val widget 'form-id)
                                                   (if (ajax-submit widget)
                                                       (js-render-form-values
                                                        (editor grid)
                                                        (get-val widget 'form-id)
                                                        (js-pair "grid-name" (name grid))
                                                        (js-pair "action" "save"))
                                                       ""))
                                           "Save")
                                          (:button :class "btn btn-warning"
                                                   :onclick
                                                   (format nil "event.preventDefault(); ~a"
                                                           (js-render (editor grid)
                                                                      (js-pair "grid-name" (name grid))
                                                                      (js-pair "action" "cancel")))
                                                   "Cancel")))))))))))

(defclass dyb-form-section (widget)
  ((section-size :initarg :section-size
                 :initform 100
                 :accessor section-size)))

(register-widget *dyb-theme* 'form-section 'dyb-form-section)

(defmethod render ((widget dyb-form-section) &key label input label-for)
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
          (:div :class "tabbable tabs-right"
                (:ul :class "nav nav-tabs"
                     (loop for (title) in (tabs widget)
                           for i from 0
                           do
                           (htm
                            (:li :class (when (zerop i)
                                          "active")
                             (:a :data-toggle "tab"
                                 :href (format nil "#~a-tab-~a" (name widget) i)
                                 (esc title))))))
                (:div :class "tab-content"
                      (loop for (nil content) in (tabs widget)
                            for i from 0
                            do
                            (htm
                             (:div :class (if (zerop i)
                                              "tab-pane active"
                                              "tab-pane")
                                   :id (format nil "~a-tab-~a" (name widget) i)
                                   (str content))))))
          (str (body-content widget)))))

(defun context-enties-display ()
  (let ((display ))
    (dolist (entity
              (entity-list))
      (setf display (concatenate 'string display 
                                 (if display 
                                     "\|"
                                     "")  
                                 (second entity) )))
    (format nil "(~A)" display)))

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
                                  (:a :target "_blank" :href "http://digyourbrand.com" 
                                      (:img :style "" :src "/appimg/dyb-logo.png"))))
                      
                      (:div :class "branding"
                            (:h3 (:span :style "color:white" (str (context-enties-display)))))
                      
                     #| (:a :class "btn btn-inverse pull-right"
                          :href "http://app.digyourbrand.co.za/dyb/generic-scheduler?action=new&grid-name=generic-actions-gridx"
                          (:i :class "white-icons pencil")
                          "Compose")
                      |#
                      (:div :class "branding"
                            (:a

                                :href "javascript:{ajax_render(\"/dyb/generic-scheduler\",%20\"generic-actions-gridx-editor\",%20[[\"grid-name\",%20\"generic-actions-gridx\"],[\"action\",%20\"new\"]])}"
                                ;;"/dyb/generic-scheduler?action=new&grid-name=generic-actions-gridx" 
                                :class "btn btn-primary"
                              
                                (:i :class "white-icons pencil")
                                " Compose "
                                        ;(:b :class "caret")
                                          
                                ))
                      (:div :class "branding"
                            (:a :target "_blank"
                                :href "http://digyourbrand.com/support/" 
                                :class "btn btn-primary"
                                
                                (:i :class "nav-icon light_bulb")
                                " Support "
                                        ;(:b :class "caret")
                                ))
                      (when (read-only-p (system-db))
                        (htm
                         (:span :class "branding alert alert-danger"
                                (:b "The DB is read-only. All changes will be lost."))))
                      (:ul :class "nav pull-right"

                           (:li :class "dropdown"
                                (:a :class "dropdown-toggle" :href "#" :data-toggle "dropdown"
                                    
                                    (str (get-val (current-user) 'email))
                                    ;;(:span :class "alert-noty" (str "0"))
                                    (:i :class "white-icons admin_user")
                                    (:b :class "caret"))
                                (:ul :class "dropdown-menu"
                                     
                                     (if (check-permission "/dyb/clients")
                                         (htm (:li
                                               (:a :href "/dyb/clients" "Clients"))))
                                     (if(check-permission "/dyb/companies")
                                        (htm (:li
                                              (:a :href "/dyb/companies" "Companies"))))
                                     (if (check-permission "/dyb/channel-users" )
                                         (htm (:li
                                               (:a :href "/dyb/channel-users" 
                                                   "Channel Users"))))

                                     (if (check-permission "/dyb/users")
                                         (htm (:li
                                               (:a :href "/dyb/users" "Users"))))
                                 
                                     (if (check-permission "/dyb/permissions")
                                         (htm (:li
                                               (:a :href "/dyb/permissions" "Permissions"))))
                            
                                     (if (check-permission "/dyb/entities")
                                         (htm (:li
                                               (:a :href "/dyb/entities" "Entities"))))
                                     (if (check-permission "/dyb/all-sorts")
                                         (htm (:li
                                               (:a :href "/dyb/all-sorts" "Allsorts"))))
                                     (if (check-permission "/dyb/manual-updates")
                                         (htm (:li
                                               (:a :href "/dyb/manual-updates" "Manual Stream Updates"))))
                                     (if (check-permission "/dyb/error-log")
                                         (htm (:li
                                               (:a :href "/dyb/error-log" "Error Log"))))
                                     (if (check-permission "/dyb/context")
                                         (htm (:li
                                               (:a :href "/dyb/context" "Context"))))

                                     (:li :class "divider")
                                     (:li
                                      (:a :href "/dyb/logout"
                                          (:i :class "icon-off")
                                          (str "Logout"))))))
                      (:button :data-target ".nav-collapse" :data-toggle "collapse" :class "btn btn-navbar" :type "button" 
                               (:span :class "icon-bar")
                               (:span :class "icon-bar")
                               (:span :class "icon-bar"))
                      (:div :class "nav-collapse in collapse mobile-menu" 
                            :style "height:auto;"
                            (:ul :class "nav"
                                 (:li :class "dropdown"
                                      (:a :href "/dyb/dashboard" :class "dropdown-toggle"
                                          :data-toggle "dropdown"
                                          (:i :class "nav-icon expos??")
                                          " Dashboard "
                                        ;(:b :class "caret")
                                          
                                          )
                                      )
                                 (:li :class "dropdown"
                                      (:a :href "/dyb/generic" :class "dropdown-toggle"
                                          :data-toggle "dropdown"
                                          (:i :class "nav-icon mail")
                                          " Inbox "
                                        ;(:b :class "caret")
                                          
                                          )
                                      )
                                 (:li :class "dropdown"
                                      (:a :href "/dyb/generic-scheduler" :class "dropdown-toggle"
                                          :data-toggle "dropdown"
                                          (:i :class "nav-icon month_calendar")
                                          " Scheduler "
                                        ;(:b :class "caret")
                                          
                                          )
                                      )
                                 (:li :class "dropdown"
                                      (:a :href "/dyb/generic-scheduler?action=new&grid-name=generic-actions-gridx"
                                          :class "dropdown-toggle"
                                          :data-toggle "dropdown"
                                          (:i :class "nav-icon create_write")
                                          " Compose "
                                        ;(:b :class "caret")
                                          
                                          )
                                      )
                                 (:li :class "dropdown"
                                      (:a :href "/dyb/tasks" :class "dropdown-toggle"
                                          :data-toggle "dropdown"
                                          (:i :class "nav-icon laptop")
                                          " Tasks "
                                        ;(:b :class "caret")
                                          
                                          )
                                      )
                                 (:li :class "dropdown"
                                      (:a :href "/dyb/search-stream" :class "dropdown-toggle"
                                          :data-toggle "dropdown"
                                          (:i :class "nav-icon magnifying_glass")
                                          " Search Streams "
                                        ;(:b :class "caret")
                                          
                                          )
                                      )
                                #|
                                 (:li :class "dropdown"
                                      (:a :target "_blank"
                                          :href "http://digyourbrand.com/support/" 
                                          :class "dropdown-toggle"
                                          :data-toggle "dropdown"
                                          (:i :class "nav-icon light_bulb")
                                          " Support "
                                        ;(:b :class "caret")
                                          
                                          )
                                      )
                                 |#
                                 ))
                      )))))

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

       " <link href=\"/css/jquery.jqplot.css\" rel=\"stylesheet\">"
       "<link rel=\"stylesheet\" type=\"text/css\" href=\"js/timeentry/jquery.timeentry.css\"> "

       (:link :id "themes" :rel "stylesheet" :href "/css/theme-blue.css")
       (:style
        "div.ui-datepicker {
 font-size:12px;
}
")
       
       (page-include-css)

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

<script type=\"text/javascript\" src=\"/js/timeentry/jquery.timeentry.js\"></script>

    <script src=\"/js/excanvas.min.js\"></script>




<script src=\"/js/custom-script.js\"></script>"


(page-include-js)

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

       (:script "$('#inline-datepicker').datepicker({
   dateFormat: 'dd M yy',
   onSelect: function(dateText, inst) {
      window.location.replace('/dyb/generic-scheduler?date=' + dateText);

   }
});
")
       (:script :defer t :src "/appjs/ajax.js")
       (:script (str (deferred-js))))

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
                          (:a :href "/dyb/dashboard"
                              (:span :class "white-icons expos??")
                              (str " Dashboard")))

                         (:li
                          (:a :href "/dyb/generic"
                              (:span :class "white-icons mail")
                              " Inbox")
                          )
                         (:li
                          (:a :href "/dyb/generic-scheduler"
                              (:span :class "white-icons month_calendar")
                              " Scheduler"))
                         #| (:li
                         (:a :href "/dyb/generic-scheduler?action=new&grid-name=generic-actions-gridx"
                         (:span :class "white-icons create_write")
                         " Quick Post")) |#
                         (:li
                          (:a :href "/dyb/tasks"
                              (:span :class "white-icons laptop")
                              " Tasks"))
                         
                         (:li
                          (:a :href "/dyb/search-stream"
                              (:span :class "white-icons magnifying_glass")
                              " Search Streams")
                         #| (:ul :class "acitem" :style "display:none;"

                         (:li
                          (:a :href "/dyb/search-stream" "Search Streams"))



                         (:li
                          (:a :href "/dyb/search-stream-feedback" "Search Stream Data"))
                         )|#
                          )
                         )
                    (:div :id "side-accordion"
                          (:div :class "accordion-group"
                                (:div :class "accordion-header"
                                      (:a :class "accordion-toggle" :href "#collapseOne"
                                          :data-parent "#side-accordion"
                                          :data-toggle "collapse"
                                          (:i :class "nav-icon month_calender")
                                          "Content Calendar"))
                                (:div :class "accordion-content"
                                      (:div :id "inline-datepicker" )))))

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
            (with-html-string
              (:link :rel "stylesheet" :href "/css/special-page.css"))
            :body
            (with-html-string
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
            (with-html-string
              "<script defer src=\"/js/mylibs/jquery.validate.js\"></script>
                <script defer src=\"/js/mylibs/jquery.jgrowl.js\"></script>
                <script defer src=\"/js/mylibs/jquery.checkbox.js\"></script>"))))
;;;

(defclass dyb-icon (icon)
  ())

(register-widget *dyb-theme* 'icon 'dyb-icon)

(defmethod render ((icon dyb-icon) &key)
  ;; (with-slots (icon-name alt title width height) icon
  ;;   (with-html
  ;;     (:img :src (frmt "/img/icons/packs/fugue/~ax~a/~a.png"
  ;;                      width height icon-name)
  ;;           :alt alt
  ;;           :title title
  ;;           :width width
  ;;           :height height)))
  )
