(in-package :ems-quickview)

(defmethod make-color (spec (type (eql :html)))
  (and spec
       (fmt "#~a" spec)))

(defvar *html-h-alignment*
  '(:center nil
    :left "left"
    :right "right"
    :justify "justify"))

(defmethod h-alignment (spec (type (eql :html)))
  (and spec
       (getf *html-h-alignment* spec)))

(defvar *html-v-alignment*
  '(:center nil
    :top "top"
    :bottom "bottom"
    :baseline "baseline"))

(defmethod v-alignment (spec (type (eql :html)))
  (and spec
       (getf *html-v-alignment* spec)))

(defmethod render-element ((table table) (type (eql :html)))
  (with-html
    (:table :class (html-class table)
      (when (title table)
        (htm (:caption (esc (title table)))))
      (render-objects (rows table) type))))

(defmethod render-element ((row row) (type (eql :html)))
  (with-html
    (:tr
     (render-objects (cells row) type))))

(defun css-properties (&rest args)
  (let ((string
          (with-output-to-string (str)
            (loop for (property value) on args by #'cddr
                  when value
                  do (format str "~(~a~):~a;" property value)))))
    (unless (equal string "")
      string)))

(defun make-vertical-text (string)
  (fmt "~{~c~^<br />~}" (coerce string 'list)))

(defmacro html-cell (cell)
  `(with-html
     (,(ecase cell
         (header :th)
         (cell :td))
      :rowspan (and (/= (row-span ,cell) 1)
                    (row-span ,cell))
      :colspan (and (/= (col-span ,cell) 1)
                    (col-span ,cell))
      :style (css-properties
              :background-color (make-color (bg-color ,cell) :html)
              :text-align (h-alignment (h-align ,cell) :html)
              :vertical-align (v-alignment (v-align ,cell) :html))
      (render-objects (text ,cell) type))))

(defmethod render-element ((header header) (type (eql :html)))
  (html-cell header))

(defmethod render-element ((cell cell) (type (eql :html)))
  (html-cell cell))

(defmethod render-element ((document document) type)
  (render-objects (elements document) type))

(defmethod process-text-style ((text text) (type (eql :html)))
  (css-properties :font-size (size text)
                  :font-weight (weight text)
                  :font-style (style text)))

(defmethod render-element ((text text) (type (eql :html)))
  (let ((style (process-text-style text type)))
    (if (plusp (length style))
        (with-html
          (:span :style style
                 (esc (text text))))
        (with-html
          (esc (text text))))))

(defmethod render-element ((paragraph paragraph) (type (eql :html)))
  (with-html
    (:p
     :style (css-properties
             :text-align (h-alignment (h-align paragraph) :html))
     (render-objects (elements paragraph) type))))

(defmethod render-element ((element html) (type (eql :html)))
  (princ (rendered-string element)))

(defmethod render-element ((element pdf) (type (eql :html))))

(defmethod render-element ((heading heading) (type (eql :html)))
  (check-type (size heading) (integer 1 6))
  (with-html
    (format t "<h~d>~a</h~@*~d>" (size heading) (escape (text heading)))))

(defmethod render-element ((page new-page) (type (eql :html))))

(defmethod render-element ((line new-line) (type (eql :html)))
  (with-html (:br)))

(defmethod render-element ((link link) (type (eql :html)))
  (with-html
    (:a :href (url link)
        :target
        (ecase (target link)
          (:blank "_blank")
          ((nil)))
        (render-element (text link) type))))

(defmethod render-element ((image image) (type (eql :html)))
  (with-html
    (:img :src (path image))))
