(in-package #:ems)

(defclass line-graph (ajax-widget)
())

(defmethod render ((widget line-graph) &key)

  (with-html
    "")

  (defer-js
(format nil
        "
$.jqplot._noToImageButton = true;
var prevYear = [[\"2011-08-01\",398], [\"2011-08-02\",255.25], [\"2011-08-03\",263.9], [\"2011-08-04\",154.24],
[\"2011-08-05\",210.18], [\"2011-08-06\",109.73], [\"2011-08-07\",166.91], [\"2011-08-08\",330.27], [\"2011-08-09\",546.6],
[\"2011-08-10\",260.5], [\"2011-08-11\",330.34], [\"2011-08-12\",464.32], [\"2011-08-13\",432.13], [\"2011-08-14\",197.78],
[\"2011-08-15\",311.93], [\"2011-08-16\",650.02], [\"2011-08-17\",486.13], [\"2011-08-18\",330.99], [\"2011-08-19\",504.33],
[\"2011-08-20\",773.12], [\"2011-08-21\",296.5], [\"2011-08-22\",280.13], [\"2011-08-23\",428.9], [\"2011-08-24\",469.75],
[\"2011-08-25\",628.07], [\"2011-08-26\",516.5], [\"2011-08-27\",405.81], [\"2011-08-28\",367.5], [\"2011-08-29\",492.68],
[\"2011-08-30\",700.79], [\"2011-08-31\",588.5], [\"2011-09-01\",511.83], [\"2011-09-02\",721.15], [\"2011-09-03\",649.62],
[\"2011-09-04\",653.14], [\"2011-09-06\",900.31], [\"2011-09-07\",803.59], [\"2011-09-08\",851.19], [\"2011-09-09\",2059.24],
[\"2011-09-10\",994.05], [\"2011-09-11\",742.95], [\"2011-09-12\",1340.98], [\"2011-09-13\",839.78], [\"2011-09-14\",1769.21],
[\"2011-09-15\",1559.01], [\"2011-09-16\",2099.49], [\"2011-09-17\",1510.22], [\"2011-09-18\",1691.72],
[\"2011-09-19\",1074.45], [\"2011-09-20\",1529.41], [\"2011-09-21\",1876.44], [\"2011-09-22\",1986.02],
[\"2011-09-23\",1461.91], [\"2011-09-24\",1460.3], [\"2011-09-25\",1392.96], [\"2011-09-26\",2164.85],
[\"2011-09-27\",1746.86], [\"2011-09-28\",2220.28], [\"2011-09-29\",2617.91], [\"2011-09-30\",3236.63]];
var currYear = [[\"2011-08-01\",796.01], [\"2011-08-02\",510.5], [\"2011-08-03\",527.8], [\"2011-08-04\",308.48],
[\"2011-08-05\",420.36], [\"2011-08-06\",219.47], [\"2011-08-07\",333.82], [\"2011-08-08\",660.55], [\"2011-08-09\",1093.19],
[\"2011-08-10\",521], [\"2011-08-11\",660.68], [\"2011-08-12\",928.65], [\"2011-08-13\",864.26], [\"2011-08-14\",395.55],
[\"2011-08-15\",623.86], [\"2011-08-16\",1300.05], [\"2011-08-17\",972.25], [\"2011-08-18\",661.98], [\"2011-08-19\",1008.67],
[\"2011-08-20\",1546.23], [\"2011-08-21\",593], [\"2011-08-22\",560.25], [\"2011-08-23\",857.8], [\"2011-08-24\",939.5],
[\"2011-08-25\",1256.14], [\"2011-08-26\",1033.01], [\"2011-08-27\",811.63], [\"2011-08-28\",735.01], [\"2011-08-29\",985.35],
[\"2011-08-30\",1401.58], [\"2011-08-31\",1177], [\"2011-09-01\",1023.66], [\"2011-09-02\",1442.31], [\"2011-09-03\",1299.24],
[\"2011-09-04\",1306.29], [\"2011-09-06\",1800.62], [\"2011-09-07\",1607.18], [\"2011-09-08\",1702.38],
[\"2011-09-09\",4118.48], [\"2011-09-10\",1988.11], [\"2011-09-11\",1485.89], [\"2011-09-12\",2681.97],
[\"2011-09-13\",1679.56], [\"2011-09-14\",3538.43], [\"2011-09-15\",3118.01], [\"2011-09-16\",4198.97],
[\"2011-09-17\",3020.44], [\"2011-09-18\",3383.45], [\"2011-09-19\",2148.91], [\"2011-09-20\",3058.82],
[\"2011-09-21\",3752.88], [\"2011-09-22\",3972.03], [\"2011-09-23\",2923.82], [\"2011-09-24\",2920.59],
[\"2011-09-25\",2785.93], [\"2011-09-26\",4329.7], [\"2011-09-27\",3493.72], [\"2011-09-28\",4440.55],
[\"2011-09-29\",5235.81], [\"2011-09-30\",6473.25]];
var plot1 = $.jqplot(\"~A\", [prevYear, currYear], {
seriesColors: [\"#6a8c9b\", \"#d44703\"],
title: 'Monthly TurnKey Revenue',
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
markerOptions: {
show: true, // wether to show data point markers.
style: 'filledCircle', // circle, diamond, square, filledCircle.
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
}
},
series: [
{
//fill: true,
label: '2010'
},
{
label: '2011'
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
min: \"2011-08-01\",
max: \"2011-09-30\",
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
formatString: \"$%'d\",
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