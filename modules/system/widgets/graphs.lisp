(in-package #:ems)

(defclass line-graph (ajax-widget)
  ()
  (:metaclass widget-class)
  (:include-js "/appcss/site_style.css"
                "/js/jquery.jqplot.min.js"
                "/js/chart/jqplot.highlighter.min.js"
                "/js/chart/jqplot.cursor.min.js"
                "/js/chart/jqplot.barRenderer.min.js"
                "/js/chart/jqplot.pointLabels.min.js"
                "/js/chart/jqplot.dateAxisRenderer.min.js"
                "/js/chart/jqplot.pieRenderer.min.js"
                "/js/chart/jqplot.donutRenderer.min.js"
                "/js/chart/jqplot.categoryAxisRenderer.min.js"
                "/js/chart/jqplot.logAxisRenderer.min.js"
                "/js/chart/jqplot.canvasTextRenderer.min.js"
                "/js/chart/jqplot.canvasAxisTickRenderer.min.js"
                ))


(defmethod render ((widget line-graph) &key)

  (with-html
    "")

  (defer-js
(format nil
        "
$.jqplot._noToImageButton = true;

var twitFollowers = [[\"2012-07-26\",39],[\"2012-07-27\",39],[\"2012-07-28\",39],[\"2012-07-29\",39],[\"2012-07-30\",39],[\"2012-07-31\",39],[\"2012-08-01\",39],[\"2012-08-02\",39],[\"2012-08-03\",39],[\"2012-08-04\",39],[\"2012-08-05\",39],[\"2012-08-06\",39],[\"2012-08-07\",42],[\"2012-08-08\",42],[\"2012-08-09\",42],[\"2012-08-10\",42],[\"2012-08-11\",42],[\"2012-08-12\",42],[\"2012-08-13\",42],[\"2012-08-14\",42],[\"2012-08-15\",42],[\"2012-08-16\",42],[\"2012-08-17\",42],[\"2012-08-18\",42],[\"2012-08-19\",42],[\"2012-08-20\",42],[\"2012-08-21\",42],[\"2012-08-22\",42],[\"2012-08-23\",42],[\"2012-08-24\",42],[\"2012-08-25\",42],[\"2012-08-26\",42],[\"2012-08-27\",42],[\"2012-08-28\",42],[\"2012-08-29\",42],[\"2012-08-30\",42],[\"2012-08-31\",42],[\"2012-09-01\",42],[\"2012-09-02\",42],[\"2012-09-03\",42],[\"2012-09-04\",42],[\"2012-09-05\",42],[\"2012-09-06\",42],[\"2012-09-07\",42],[\"2012-09-08\",42],[\"2012-09-09\",42],[\"2012-09-10\",42],[\"2012-09-11\",42],[\"2012-09-12\",42],[\"2012-09-13\",42],[\"2012-09-14\",42],[\"2012-09-15\",42],[\"2012-09-16\",42],[\"2012-09-17\",42],[\"2012-09-18\",42],[\"2012-09-19\",42],[\"2012-09-20\",42],[\"2012-09-21\",42],[\"2012-09-22\",42],[\"2012-09-23\",42],[\"2012-09-24\",42],[\"2012-09-25\",42],[\"2012-09-26\",42]];
;
var faceFollowers = [[\"2012-07-26\",295],[\"2012-07-27\",295],[\"2012-07-28\",294],[\"2012-07-29\",294],[\"2012-07-30\",295],[\"2012-07-31\",295],[\"2012-08-01\",296],[\"2012-08-02\",296],[\"2012-08-03\",296],[\"2012-08-04\",296],[\"2012-08-05\",296],[\"2012-08-06\",295],[\"2012-08-07\",295],[\"2012-08-08\",295],[\"2012-08-09\",295],[\"2012-08-10\",294],[\"2012-08-11\",294],[\"2012-08-12\",294],[\"2012-08-13\",294],[\"2012-08-14\",294],[\"2012-08-15\",294],[\"2012-08-16\",294],[\"2012-08-17\",294],[\"2012-08-18\",294],[\"2012-08-19\",293],[\"2012-08-20\",293],[\"2012-08-21\",293],[\"2012-08-22\",293],[\"2012-08-23\",291],[\"2012-08-24\",291],[\"2012-08-25\",291],[\"2012-08-26\",291],[\"2012-08-27\",291],[\"2012-08-28\",291],[\"2012-08-29\",291],[\"2012-08-30\",291],[\"2012-08-31\",308],[\"2012-09-01\",308],[\"2012-09-02\",310],[\"2012-09-03\",310],[\"2012-09-04\",310],[\"2012-09-05\",310],[\"2012-09-06\",310],[\"2012-09-07\",310],[\"2012-09-08\",310],[\"2012-09-09\",310],[\"2012-09-10\",310],[\"2012-09-11\",309],[\"2012-09-12\",309],[\"2012-09-13\",309],[\"2012-09-14\",309],[\"2012-09-15\",309],[\"2012-09-16\",309],[\"2012-09-17\",309],[\"2012-09-18\",309],[\"2012-09-19\",309],[\"2012-09-20\",309],[\"2012-09-21\",309],[\"2012-09-22\",309],[\"2012-09-23\",310],[\"2012-09-24\",310],[\"2012-09-25\",310],[\"2012-09-26\",310]];

var linkFollowers = [[\"2012-07-26\",22],[\"2012-07-27\",22],[\"2012-07-28\",22],[\"2012-07-29\",22],[\"2012-07-30\",22],[\"2012-07-31\",22],[\"2012-08-01\",22],[\"2012-08-02\",22],[\"2012-08-03\",22],[\"2012-08-04\",22],[\"2012-08-05\",22],[\"2012-08-06\",22],[\"2012-08-07\",22],[\"2012-08-08\",22],[\"2012-08-09\",22],[\"2012-08-10\",22],[\"2012-08-11\",22],[\"2012-08-12\",22],[\"2012-08-13\",22],[\"2012-08-14\",22],[\"2012-08-15\",22],[\"2012-08-16\",22],[\"2012-08-17\",22],[\"2012-08-18\",22],[\"2012-08-19\",22],[\"2012-08-20\",22],[\"2012-08-21\",22],[\"2012-08-22\",22],[\"2012-08-23\",22],[\"2012-08-24\",22],[\"2012-08-25\",22],[\"2012-08-26\",22],[\"2012-08-27\",22],[\"2012-08-28\",22],[\"2012-08-29\",22],[\"2012-08-30\",22],[\"2012-08-31\",22],[\"2012-09-01\",22],[\"2012-09-02\",22],[\"2012-09-03\",22],[\"2012-09-04\",22],[\"2012-09-05\",22],[\"2012-09-06\",22],[\"2012-09-07\",22],[\"2012-09-08\",22],[\"2012-09-09\",22],[\"2012-09-10\",22],[\"2012-09-11\",22],[\"2012-09-12\",22],[\"2012-09-13\",22],[\"2012-09-14\",22],[\"2012-09-15\",22],[\"2012-09-16\",22],[\"2012-09-17\",22],[\"2012-09-18\",22],[\"2012-09-19\",22],[\"2012-09-20\",22],[\"2012-09-21\",22],[\"2012-09-22\",22],[\"2012-09-23\",22],[\"2012-09-24\",22],[\"2012-09-25\",22],[\"2012-09-26\",22]];

var currentNetwork = [[\"2012-07-26\",356],[\"2012-07-27\",356],[\"2012-07-28\",355],[\"2012-07-29\",355],[\"2012-07-30\",356],[\"2012-07-31\",356],[\"2012-08-01\",357],[\"2012-08-02\",357],[\"2012-08-03\",357],[\"2012-08-04\",357],[\"2012-08-05\",357],[\"2012-08-06\",356],[\"2012-08-07\",359],[\"2012-08-08\",359],[\"2012-08-09\",359],[\"2012-08-10\",358],[\"2012-08-11\",358],[\"2012-08-12\",358],[\"2012-08-13\",358],[\"2012-08-14\",358],[\"2012-08-15\",358],[\"2012-08-16\",358],[\"2012-08-17\",358],[\"2012-08-18\",358],[\"2012-08-19\",357],[\"2012-08-20\",357],[\"2012-08-21\",357],[\"2012-08-22\",357],[\"2012-08-23\",355],[\"2012-08-24\",355],[\"2012-08-25\",355],[\"2012-08-26\",355],[\"2012-08-27\",355],[\"2012-08-28\",355],[\"2012-08-29\",355],[\"2012-08-30\",355],[\"2012-08-31\",372],[\"2012-09-01\",372],[\"2012-09-02\",374],[\"2012-09-03\",374],[\"2012-09-04\",374],[\"2012-09-05\",374],[\"2012-09-06\",374],[\"2012-09-07\",374],[\"2012-09-08\",374],[\"2012-09-09\",374],[\"2012-09-10\",374],[\"2012-09-11\",373],[\"2012-09-12\",373],[\"2012-09-13\",373],[\"2012-09-14\",373],[\"2012-09-15\",373],[\"2012-09-16\",373],[\"2012-09-17\",373],[\"2012-09-18\",373],[\"2012-09-19\",373],[\"2012-09-20\",373],[\"2012-09-21\",373],[\"2012-09-22\",373],[\"2012-09-23\",374],[\"2012-09-24\",374],[\"2012-09-25\",374],[\"2012-09-26\",374]];

var plot1 = $.jqplot(\"~A\", [twitFollowers, faceFollowers, linkFollowers, currentNetwork], {
  seriesColors: [\"#00FFFF\", \"#0000FF\",\"#04B45F\", \"#d44703\"],
  title: '',
  highlighter: {
    show: true,
    sizeAdjust: 1,
    tooltipOffset: 9
    },
  grid: {
    background: '#fff',
    drawBorder: false,
    shadow: false,
    gridLineColor: '#666666',
    gridLineWidth: 1
    },
  legend: {
    show: true,
    placement: 'outside'
    },
  seriesDefaults: {
    rendererOptions: {
      smooth: true,
      animation: {
        show: true
        }
    },
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

  },
series: [
{
//fill: true,
label: 'TW Followers',
markerOptions: { style:'dimaond' }
},
{
label: 'FB Followers',
 markerOptions: { style:\"filledSquare\", size:10 }
},
{
label: 'LNK Followers',
 markerOptions: { style:\"circle\" }
},
{
label: 'Cur Network',
showLine:false,
markerOptions: { size: 7, style:\"x\" }

}
],
axesDefaults: {
rendererOptions: {
 
baselineWidth: 1.5,
baselineColor: '#444444',
drawBaseline: false
}
},
axes: {
xaxis: {
renderer: $.jqplot.DateAxisRenderer,
tickRenderer: $.jqplot.CanvasAxisTickRenderer,
tickOptions: {
formatString: \"%b %e\",
angle: -30,
textColor: '#dddddd'
},
min: \"2012-07-26\",
max: \"2012-9-26\",
tickInterval: \"7 days\",
drawMajorGridlines: false
},
yaxis: {
renderer: $.jqplot.LogAxisRenderer,
pad: 0,
rendererOptions: {
minorTicks: 1
},
tickOptions: {
formatString: \"%'d\",
showMark: false
}
}
}
});
"
        (name widget))))


(define-easy-handler (ajax-graph-page :uri "/ems/test-graph-ajax") ()
  (let ((page (make-widget 'html-framework-page
                           :name "ajax-graph-test"))
        (widget (make-widget 'line-graph :name "chart6")))
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