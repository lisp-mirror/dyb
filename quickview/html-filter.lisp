(in-package :ems-quickview)

(defvar *allowed-tags*
  '("p" "div" "span" "a"
    "b" "strong" "em"
    "ul" "li"  "ol"))

(defclass filter-sink (chtml::sink)
  ())

(defun make-filter-sink ()
  (make-instance 'filter-sink
                 :encoding "UTF-8"
                 :ystream
                 (runes:make-rod-ystream
                  :encoding (runes:find-output-encoding "UTF-8"))))

(defun find-pt-child (name pt)
  (find name (chtml:pt-children pt)
        :key #'chtml:pt-name))

(defun parse-html (string)
  (find-pt-child :body
                 (chtml:parse string nil)))

(defun filter-html (string)
  (chtml:serialize-pt
   (parse-html string)
   (make-filter-sink)))

(defmethod hax:start-element :around ((sink filter-sink) name attributes)
  (when (find name *allowed-tags*
              :test #'equalp)
    (call-next-method)))

(defmethod hax:end-element :around ((sink filter-sink) name)
  (when (find name *allowed-tags*
              :test #'equalp)
    (call-next-method)))
