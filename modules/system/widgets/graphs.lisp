(in-package :dyb)

(defclass graph (ajax-widget)
  ((data :initarg :data
         :initform nil
         :accessor data)
   (title :initarg :title
          :initform nil
          :accessor title))
  (:include-js "/appcss/site_style.css"
   "/js/jplot/jquery.jqplot.min.js"
   "/js/jplot/plugins/jqplot.highlighter.min.js"
   "/js/jplot/plugins/jqplot.cursor.min.js"
   "/js/jplot/plugins/jqplot.barRenderer.min.js"
   "/js/jplot/plugins/jqplot.pointLabels.min.js"
   "/js/jplot/plugins/jqplot.dateAxisRenderer.min.js"
   "/js/jplot/plugins/jqplot.pieRenderer.min.js"
   "/js/jplot/plugins/jqplot.donutRenderer.min.js"
   "/js/jplot/plugins/jqplot.categoryAxisRenderer.min.js"
   "/js/jplot/plugins/jqplot.logAxisRenderer.min.js"
   "/js/jplot/plugins/jqplot.canvasTextRenderer.min.js"
   "/js/jplot/plugins/jqplot.canvasAxisTickRenderer.min.js")
  (:metaclass widget-class))

(defclass line-graph (graph)
  ((series :initarg :series
           :initform nil
           :accessor series)
   (series-defaults :initarg :series-defaults
           :initform nil
           :accessor series-defaults)
   (x :initarg :x
              :initform nil
              :accessor x)
   (y :initarg :y
              :initform nil
              :accessor y)
   (grid :initarg :grid
                 :initform nil
                 :accessor grid)
   (highlighter :initarg :highlighter
                :initform nil
                :accessor highlighter)
   (legend :initarg :legend
           :initform nil
           :accessor legend)
   (cursor :initarg :cursor
           :initform nil
           :accessor cursor)
   (data-lables :initarg :data-lables
           :initform nil
           :accessor data-lables))
  (:metaclass widget-class))



(defun json-getf (plist key &optional json-key)
  (let* ((default (load-time-value (gensym)))
         (value (getf plist key default)))
    (unless (eq value default)
      (json:encode-object-member (or json-key key)
                                 value))))

(defun json-encode-key (key value)
  (when value
     (json:encode-object-member key value)))

(defun json-encode-literal-key (key value)
  (when value
     (json:as-object-member (key)
       (princ value json:*json-output*))))

(defmacro with-json-object (&body body)
  `(with-output-to-string (json:*json-output*)
     (json:with-object ()
       ,@body)))

(defun format-graph-series-marker-options (options)

  )

(defun format-graph-series (series)
  (with-output-to-string (json:*json-output*)
    (json:with-array ()
      (loop for options in series
            do
            (json:as-array-member ()
              (json:with-object ()
                (json-getf options :label)
                (json-getf options :fill)
                (json-getf options :show-marker)
                (if (getf options :marker-options)
                    (json-encode-key "markerOptions"
                                     (with-json-object
                                       (json-getf (getf options :marker-options) :show)
                                       (json-getf (getf options :marker-options) :style)
                                       (json-getf (getf options :marker-options) :size))))))))))

(defparameter *graph-renderer-types*
  '(:date "$.jqplot.DateAxisRenderer"
    :log "$.jqplot.LogAxisRenderer"
    :pie "$.jqplot.PieRenderer"
    :bar "$.jqplot.BarRenderer"
    :category "$.jqplot.CategoryAxisRenderer"))

(defun format-graph-axis-tick-options (options)
  (with-json-object
    (json-getf options :format-string)))

(defun format-graph-series-renderer-options-animation (options)

  (with-json-object
    (json-getf options :show)))

 (defun format-graph-series-renderer-options (options)

  (with-json-object
    (if (getf options :start-angle)
        (json-getf options :start-angle))
    (if (getf options :slice-margin)
        (json-getf options :slice-margin))
    (if (getf options :show-data-labels)
        (json-getf options :show-data-labels))
    (if (getf options :smooth)
        (json-getf options :smooth))

    (if (getf options :animation)
        (json-encode-key "animation"
                         (format-graph-series-renderer-options-animation 
                          (getf options :animation))))))




(defun format-graph-series-defaults (options)
  (with-json-object
    (if (getf options :renderer)
        (json-encode-literal-key :renderer
                                 (getf *graph-renderer-types* (getf options :renderer))))
    (if (getf options :show)
        (json-getf options :show))
    (if (getf options :xaxis)
        (json-getf options :xaxis))
    (if (getf options :yaxis)
        (json-getf options :yaxis))
    (if (getf options :line-width)
        (json-getf options :line-width))
    (if (getf options :shadow)
        (json-getf options :shadow))
    (if (getf options :renderer-options)
        (json-encode-key :renderer-options
                         
                         (with-json-object 
                           (json-getf (getf options :renderer-options) :show-data-labels))
                         ))
    #|(if (getf options :renderer-options)
        (json-encode-key :renderer-options
                         (format-graph-series-renderer-options 
                          (getf options :renderer-options))))
    |#
    ))

(defun format-graph-axis (options)
  (with-json-object
    (json-encode-literal-key :renderer
                             (getf *graph-renderer-types* 
                                   (or  (getf options :renderer) (getf options :type) )))
    (json-getf options :min)
    (json-getf options :max)
    (json-getf options :ticks)
    (json-getf options :tick-interval)
    (if (getf options :tick-options)
        (json-encode-key "tick-options"
                                 (format-graph-axis-tick-options 
                                  (getf options :tick-options))))))

(defun format-graph-grid (options)
  (with-json-object
    (json-getf options :background-color :background)
    (json-getf options :draw-border)
    (json-getf options :shadow)
    (json-getf options :grid-line-color)
    (json-getf options :grid-line-width)))

(defun format-graph-highlighter (options)
  (with-json-object
    (json-getf options :show)
    (json-getf options :size-adjust)
    (json-getf options :tooltip-offset)))

(defun format-graph-legend (options)
  (with-json-object

    (if (getf options :show)
        (json-getf options :show)
        "false")
    (json-getf options :placement)))

(defun format-graph-cursor (options)
  (with-json-object
    (json-getf options :show)
    ))

(defmethod render ((widget line-graph) &key)
  (defer-js
      (format
       nil
       "
$.jqplot._noToImageButton = true;

var plot1 = $.jqplot(\"~A\", ~a, {
  title: ~s,
  seriesColors: ~a,
  series: ~a,
  axes: {xaxis: ~a,yaxis: ~a },
  grid: ~a,
  highlighter: ~a,
  legend: ~a,
  cursor: ~a,
  seriesDefaults: 
    ~a
    ,
    /*markerOptions: {
      show: true, // wether to show data point markers.
      //style: 'filledCircle', // circle, diamond, square, filledCircle.
      // filledDiamond or filledSquare.
      lineWidth: 2, // width of the stroke drawing the marker.
      size: 10, // size (diameter, edge length, etc.) of the marker.
      color: '#ff8a00', // color of marker, set to color of line by default.
      shadow: true, // wether to draw shadow on marker or not.
      shadowAngle: 45, // angle of the shadow. Clockwise from x axis.
      shadowOffset: 1, // offset from the line of the shadow,
      shadowDepth: 3, // Number of strokes to make when drawing shadow. Each stroke
      // offset by shadowOffset from the last.
      shadowAlpha: 0.07 // Opacity of the shadow
    }*/

  
 /*
axesDefaults: {
rendererOptions: {
 
baselineWidth: 1.5,
baselineColor: '#444444',
drawBaseline: false
}

}
*/
});

"
       (name widget)
       (if (data widget)
           (json:encode-json-to-string (data widget))
           "[[null]]")
       (title widget)
       (if (series widget)
           (json:encode-json-to-string (mapcar (lambda (x) (getf x :color))
                                               (series widget)))
           "null")
       (format-graph-series (series widget))
       (format-graph-axis (x widget))
       (format-graph-axis (y widget))
       (format-graph-grid (grid widget))
       (format-graph-highlighter (highlighter widget))
       (format-graph-legend (legend widget))
       (format-graph-cursor (cursor widget))
       (if (string-equal (name widget) "engagementgraph")
           (format nil " {
        renderer: $.jqplot.PieRenderer, 
        rendererOptions: {
        showDataLabels: \"true\", 
        sliceMargin: 4,
        dataLabels: ~A 
        }
        }" (data-lables widget))
           (format-graph-series-defaults (series-defaults widget))))))

 (defparameter *graph-data*
   '((("2012-07-26" 39) ("2012-07-27" 39) ("2012-07-28" 39) ("2012-07-29" 39)
      ("2012-07-30" 39) ("2012-07-31" 39) ("2012-08-01" 39) ("2012-08-02" 39)
      ("2012-08-03" 39) ("2012-08-04" 39) ("2012-08-05" 39) ("2012-08-06" 39)
      ("2012-08-07" 42) ("2012-08-08" 42) ("2012-08-09" 42) ("2012-08-10" 42)
      ("2012-08-11" 42) ("2012-08-12" 42) ("2012-08-13" 42) ("2012-08-14" 42)
      ("2012-08-15" 42) ("2012-08-16" 42) ("2012-08-17" 42) ("2012-08-18" 42)
      ("2012-08-19" 42) ("2012-08-20" 42) ("2012-08-21" 42) ("2012-08-22" 42)
      ("2012-08-23" 42) ("2012-08-24" 42) ("2012-08-25" 42) ("2012-08-26" 42)
      ("2012-08-27" 42) ("2012-08-28" 42) ("2012-08-29" 42) ("2012-08-30" 42)
      ("2012-08-31" 42) ("2012-09-01" 42) ("2012-09-02" 42) ("2012-09-03" 42)
      ("2012-09-04" 42) ("2012-09-05" 42) ("2012-09-06" 42) ("2012-09-07" 42)
      ("2012-09-08" 42) ("2012-09-09" 42) ("2012-09-10" 42) ("2012-09-11" 42)
      ("2012-09-12" 42) ("2012-09-13" 42) ("2012-09-14" 42) ("2012-09-15" 42)
      ("2012-09-16" 42) ("2012-09-17" 42) ("2012-09-18" 42) ("2012-09-19" 42)
      ("2012-09-20" 42) ("2012-09-21" 42) ("2012-09-22" 42) ("2012-09-23" 42)
      ("2012-09-24" 42) ("2012-09-25" 42) ("2012-09-26" 42))
     (("2012-07-26" 295) ("2012-07-27" 295) ("2012-07-28" 294) ("2012-07-29" 294)
      ("2012-07-30" 295) ("2012-07-31" 295) ("2012-08-01" 296) ("2012-08-02" 296)
      ("2012-08-03" 296) ("2012-08-04" 296) ("2012-08-05" 296) ("2012-08-06" 295)
      ("2012-08-07" 295) ("2012-08-08" 295) ("2012-08-09" 295) ("2012-08-10" 294)
      ("2012-08-11" 294) ("2012-08-12" 294) ("2012-08-13" 294) ("2012-08-14" 294)
      ("2012-08-15" 294) ("2012-08-16" 294) ("2012-08-17" 294) ("2012-08-18" 294)
      ("2012-08-19" 293) ("2012-08-20" 293) ("2012-08-21" 293) ("2012-08-22" 293)
      ("2012-08-23" 291) ("2012-08-24" 291) ("2012-08-25" 291) ("2012-08-26" 291)
      ("2012-08-27" 291) ("2012-08-28" 291) ("2012-08-29" 291) ("2012-08-30" 291)
      ("2012-08-31" 308) ("2012-09-01" 308) ("2012-09-02" 310) ("2012-09-03" 310)
      ("2012-09-04" 310) ("2012-09-05" 310) ("2012-09-06" 310) ("2012-09-07" 310)
      ("2012-09-08" 310) ("2012-09-09" 310) ("2012-09-10" 310) ("2012-09-11" 309)
      ("2012-09-12" 309) ("2012-09-13" 309) ("2012-09-14" 309) ("2012-09-15" 309)
      ("2012-09-16" 309) ("2012-09-17" 309) ("2012-09-18" 309) ("2012-09-19" 309)
      ("2012-09-20" 309) ("2012-09-21" 309) ("2012-09-22" 309) ("2012-09-23" 310)
      ("2012-09-24" 310) ("2012-09-25" 310) ("2012-09-26" 310))
     (("2012-07-26" 22) ("2012-07-27" 22) ("2012-07-28" 22) ("2012-07-29" 22)
      ("2012-07-30" 22) ("2012-07-31" 22) ("2012-08-01" 22) ("2012-08-02" 22)
      ("2012-08-03" 22) ("2012-08-04" 22) ("2012-08-05" 22) ("2012-08-06" 22)
      ("2012-08-07" 22) ("2012-08-08" 22) ("2012-08-09" 22) ("2012-08-10" 22)
      ("2012-08-11" 22) ("2012-08-12" 22) ("2012-08-13" 22) ("2012-08-14" 22)
      ("2012-08-15" 22) ("2012-08-16" 22) ("2012-08-17" 22) ("2012-08-18" 22)
      ("2012-08-19" 22) ("2012-08-20" 22) ("2012-08-21" 22) ("2012-08-22" 22)
      ("2012-08-23" 22) ("2012-08-24" 22) ("2012-08-25" 22) ("2012-08-26" 22)
      ("2012-08-27" 22) ("2012-08-28" 22) ("2012-08-29" 22) ("2012-08-30" 22)
      ("2012-08-31" 22) ("2012-09-01" 22) ("2012-09-02" 22) ("2012-09-03" 22)
      ("2012-09-04" 22) ("2012-09-05" 22) ("2012-09-06" 22) ("2012-09-07" 22)
      ("2012-09-08" 22) ("2012-09-09" 22) ("2012-09-10" 22) ("2012-09-11" 22)
      ("2012-09-12" 22) ("2012-09-13" 22) ("2012-09-14" 22) ("2012-09-15" 22)
      ("2012-09-16" 22) ("2012-09-17" 22) ("2012-09-18" 22) ("2012-09-19" 22)
      ("2012-09-20" 22) ("2012-09-21" 22) ("2012-09-22" 22) ("2012-09-23" 22)
      ("2012-09-24" 22) ("2012-09-25" 22) ("2012-09-26" 22))
     (("2012-07-26" 356) ("2012-07-27" 356) ("2012-07-28" 355) ("2012-07-29" 355)
      ("2012-07-30" 356) ("2012-07-31" 356) ("2012-08-01" 357) ("2012-08-02" 357)
      ("2012-08-03" 357) ("2012-08-04" 357) ("2012-08-05" 357) ("2012-08-06" 356)
      ("2012-08-07" 359) ("2012-08-08" 359) ("2012-08-09" 359) ("2012-08-10" 358)
      ("2012-08-11" 358) ("2012-08-12" 358) ("2012-08-13" 358) ("2012-08-14" 358)
      ("2012-08-15" 358) ("2012-08-16" 358) ("2012-08-17" 358) ("2012-08-18" 358)
      ("2012-08-19" 357) ("2012-08-20" 357) ("2012-08-21" 357) ("2012-08-22" 357)
      ("2012-08-23" 355) ("2012-08-24" 355) ("2012-08-25" 355) ("2012-08-26" 355)
      ("2012-08-27" 355) ("2012-08-28" 355) ("2012-08-29" 355) ("2012-08-30" 355)
      ("2012-08-31" 372) ("2012-09-01" 372) ("2012-09-02" 374) ("2012-09-03" 374)
      ("2012-09-04" 374) ("2012-09-05" 374) ("2012-09-06" 374) ("2012-09-07" 374)
      ("2012-09-08" 374) ("2012-09-09" 374) ("2012-09-10" 374) ("2012-09-11" 373)
      ("2012-09-12" 373) ("2012-09-13" 373) ("2012-09-14" 373) ("2012-09-15" 373)
      ("2012-09-16" 373) ("2012-09-17" 373) ("2012-09-18" 373) ("2012-09-19" 373)
      ("2012-09-20" 373) ("2012-09-21" 373) ("2012-09-22" 373) ("2012-09-23" 374)
      ("2012-09-24" 374) ("2012-09-25" 374) ("2012-09-26" 374))))


 (define-easy-handler (ajax-graph-page :uri "/dyb/test-graph-ajax") ()
   (let ((page (make-widget 'html-framework-page
                            :name "ajax-graph-test"))
         (widget (make-widget
                  'line-graph :name "chart6"
                  :data *graph-data*)))
     (setf (series widget)
           '((:color "#00FFFF"
              :label "TW"
              :style "diamond"
              )
             (:color "#0000FF"
              :label "FB"
              :style "filledSquare"
              :size 10)
             (:color "#04B45F"
              :label "LNK"
              :style "circle")
             (:color "#d44703"
              :label "Network"
              :style "x")))
     (setf (title widget) "Foo")
     (setf (x widget)
           '(:type :date
             :min "2012-07-26"
             :max "2012-9-20"
             :tick-interval "7 days"))
     (setf (y widget)
           '(:type :log))
     (setf (grid widget)
           '(:background "#fff"
             :draw-border nil
             :shadow nil
             :grid-line-color "#666666"
             :grid-line-width 1))
     (setf (highlighter widget)
           '(:show t :size-adjust 1 :tooltip-offset 9))
     (setf (legend widget)
           '(:show t :placement :outside))
     (setf (series-defaults widget) 
           '(:smooth "true"
             :renderer-options
              (:animation 
               (:show "true"))))
     (render page
             :body
             (with-html-string 
               (:a :href
                   (js-link 
                    (js-render widget
                               (js-pair "some-param" "testing")
                               (js-pair "some-param1" "fuck")))
                   (make-icon "card--pencil"
                              :title "Show Graph"))

               (render widget)))))
