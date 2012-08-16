(in-package :ems-quickview)

;; (defvar *font-directory*
;;   (merge-pathnames "fonts/" #.(make-pathname :name nil :type nil
;;                                              :defaults
;;                                              (or *compile-file-truename*
;;                                                  *load-truename*))))

;; (defun get-font (name)
;;   (let ((ttf (make-pathname :name name
;;                             :type "ttf"
;;                             :defaults *font-directory*))
;;         (ufm (make-pathname :name name
;;                             :type "ufm"
;;                             :defaults *font-directory*)))
;;     (pdf:load-ttu-font ufm ttf)))

(defvar *pdf-margins* #(10 10 10 10))
(defvar *pdf-table-border* 0.5)
(defvar *pdf-table-padding* 1)
(defvar *pdf-table-cell-padding* 1)
(defvar *pdf-heading-sizes* #(0 22 20 18 16 14 12))

;; (defvar *pdf-font* (get-font "DejaVuSans"))
;; (defvar *pdf-font-italic* (get-font "DejaVuSans-Oblique"))
;; (defvar *pdf-font-bold* (get-font "DejaVuSans-Bold"))
;; (defvar *pdf-font-bold-italic* (get-font "DejaVuSans-BoldOblique"))

(defvar *pdf-font* "Helvetica")
(defvar *pdf-font-italic* "Helvetica-Oblique")
(defvar *pdf-font-bold* "Helvetica-Bold")
(defvar *pdf-font-bold-italic* "Helvetica-BoldOblique")

(defvar *pdf-max-page* 0)
(defvar *pdf-image-path*
  (asdf:system-relative-pathname 'ems "public/"))

;;; Did I tell you that I hate cl-pdf and cl-typesetting?
;; (defmethod pdf::get-char-metrics (char-or-code (font pdf::ttu-font-metrics) encoding)
;;   (aref (pdf::encoding-vector font)
;;         (if (characterp char-or-code) (char-code char-or-code) char-or-code)))

;; (defmethod pdf:encoding ((font pdf::ttu-font-metrics))
;;   pdf::*unicode-encoding*)

;; (defmethod pdf:hyphen-char ((font pdf::ttu-font-metrics))
;;   #\-)

(defmacro with-pdf ((file) &body body)
  `(locally
       ;; muffle that annoying stack allocation note
       #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
       (let ((tt::*default-font-size* 10.0)
             (*pdf-max-page* 0)
             (ref ',(gensym)))
         (tt:with-style (:font-size 10.0)
           (tt:with-document ()
             (tt:find-ref-point ref)
             ,@body
             (tt:write-document ,file))))))

(defun time-stamp ()
  (local-time:format-timestring
   nil
   (local-time:universal-to-timestamp (get-universal-time))
   :format
   '((:day 2) " " :short-month " " :year " "
     (:hour 2) ":" (:min 2) ":" (:sec 2))))

(defun draw-pdf-pages (text document)
  (let ((time-stamp (time-stamp)))
    (with-slots (header) document
      (tt:draw-pages
       text
       :orientation (orientation document)
       :break :always
       :margins (map 'vector #'+ *pdf-margins* #(0 95 0 22))
       :header-top (and header (elt *pdf-margins* 1))
       :header (and header
                    (tt:compile-text (:font-size 10)
                      (render-element header :pdf)))
       :footer-bottom (elt *pdf-margins* 3)
       :footer
       (lambda (pdf:*page*)
         (tt:compile-text (:font-size 10)
           (tt:hbox (:align :left :adjustable-p t)
             (tt:put-string time-stamp)
             :hfill
             (tt:format-string "~d of ~d"
                               pdf:*page-number*
                               (alexandria:maxf *pdf-max-page*
                                                pdf:*page-number*)))))))))

(defun propagate-doc-header (document)
  (loop for element in (elements document)
        when (typep element 'document)
        do (setf (document-header element)
                 (document-header document))
           (propagate-doc-header element)))

(defun copy-object (object &rest initargs)
  (let* ((class (class-of object))
         (new (allocate-instance class)))
    (loop for slotd in (c2mop:class-slots class)
          when (c2mop:slot-boundp-using-class class object slotd)
          do (setf (c2mop:slot-value-using-class class new slotd)
                   (c2mop:slot-value-using-class class object slotd)))
    (apply #'reinitialize-instance new initargs)))

(defun next-orientation (elements)
  (loop for element in elements
        while (typep element 'document)
        do
        (case (orientation element)
          (:next)
          (:inherit
           (return (find-sub-document-orientation element)))
          ((:landscape :portrate)
           (return (orientation element))))))

(defun find-sub-document-orientation (document)
  (loop for element in (elements document)
        thereis
        (and (typep element 'document)
             (case (orientation element)
               (:inherit
                (find-sub-document-orientation element))
               ((:landscape :portrate)
                (orientation element))))))

(defun merge-documents (document)
  (loop with orientation = (orientation document)
        for (element . rest) on (elements document)
        and current-orientation = orientation
        then (and (typep element 'document) (orientation element))
        append (cond ((not (typep element 'document))
                      (list element))
                     ((eq orientation (orientation element))
                      (merge-documents element))
                     ((case (orientation element)
                        (:previous
                         (eq current-orientation orientation))
                        (:next
                         (or (null (next-orientation rest))
                             (eq orientation (next-orientation rest))))
                        (:inherit (or (null (find-sub-document-orientation element))
                                      (eq (find-sub-document-orientation element)
                                          orientation))))
                      (merge-documents
                       (copy-object element
                                    :orientation orientation)))
                     (t (list element)))))

;;; (group-list #'evenp '(1 1 2 4 5)) =>
;;;  ((1 1) (2 4) (5))

(defun group-list (test list)
  (loop while list
        collect
        (loop with tested = (funcall test (car list))
              for (element) = list
              while (and list
                         (eq tested
                             (funcall test element)))
              collect element
              do (pop list))))

(defun inherit-orientation (documents orientation)
  (flet ((find-next-orientation (documents)
           (loop for d in documents
                 for orientation = (orientation d)
                 thereis (case orientation
                           ((:landscape :portrate)
                            orientation)
                           (:inherit
                            (find-sub-document-orientation d))))))
    (loop with current-orientation = orientation
          for (document . rest) on documents
          collect (ecase (orientation document)
                    (:previous
                     (copy-object document
                                  :orientation current-orientation))
                    (:next
                     (copy-object document
                                  :orientation (or (find-next-orientation rest)
                                                   orientation)))
                    (:inherit
                     (copy-object document
                                  :orientation
                                  (or (find-sub-document-orientation document)
                                      orientation)))
                    ((:landscape :portrate)
                     (setf current-orientation
                           (orientation document))
                     document)))))

(defun merge-adjacent-documents (documents orientation)
  (let ((groups (group-list (lambda (x)
                              (eq (orientation x) :landscape))
                            (inherit-orientation documents
                                                 orientation))))
    (loop for group in groups
          for elements = (reduce #'append group :key #'elements)
          collect (make-instance 'document-part
                                 :orientation (orientation (car group))
                                 :elements elements
                                 :header (document-header (car group))))))

(defun group-elements (elements orientation)
  (let ((groups (group-list (lambda (x) (typep x 'document))
                            elements)))
    (loop for group in groups
          collect (if (typep (car group) 'document)
                      (merge-adjacent-documents group orientation)
                      group))))

(defmethod render-element ((document document) (type (eql :pdf)))
  (let ((groups (group-elements (merge-documents document)
                                (orientation document))))
    (loop for group in groups
          do
          (if (typep (car group) 'document)
              (render-element (car group) type)
              (draw-pdf-pages
               (tt:compile-text ()
                 (render-objects group type))
               document)))))

(defun remove-nil-properties (plist)
  (loop for (property value) on plist by #'cddr
        when value
        collect property
        and
        collect value))

(defun select-pdf-font (&key bold italic)
  (cond ((and bold italic)
         *pdf-font-bold-italic*)
        (bold
         *pdf-font-bold*)
        (italic
         *pdf-font-italic*)))

(defmacro with-pdf-style ((style) &body body)
  (tt::with-gensyms (new-style restore-style)
    `(let* ((,new-style (apply #'make-instance 'tt::text-style ,style))
	    (,restore-style (tt::make-restore-style ,new-style)))
       (tt::add-box ,new-style)
       (tt::use-style ,new-style)
       ,@(mapcar 'tt::insert-stuff body)
       (tt::add-box ,restore-style)
       (tt::use-style ,restore-style))))

(defmethod process-text-style ((text text) (type (eql :pdf)))
  (remove-nil-properties
   (list :font-size (size text)
         :font (select-pdf-font :bold (eql :bold (weight text))
                                :italic (eql :italic (style text))))))

(defmethod render-element ((string string) (type (eql :pdf)))
  (tt:put-string string))

(defmethod render-element ((text text) (type (eql :pdf)))
  (let ((style (process-text-style text type)))
    (if style
        (with-pdf-style (style)
          (render-element (text text) type))
        (render-element (text text) type))))

(defmethod render-element ((paragraph paragraph) (type (eql :pdf)))
  (tt:paragraph (:h-align (h-align paragraph))
    (render-objects (elements paragraph) type)))

(defmethod make-color (spec (type (eql :pdf)))
  (and spec
       (parse-integer spec :radix 16)))

(defun string-width (string)
  (let ((tt::*default-font-size* 10.0))
    (typeset::compute-boxes-natural-size
     (typeset::boxes
      (tt:compile-text ()
        (tt:put-string string)))
     #'typeset::dx)))

(defun max-width (columns)
  (loop for cell in columns
        for text = (text cell)
        maximize (etypecase text
                   ((or number string)
                    (string-width (princ-to-string text)))
                   (document-element
                    (string-width (princ-to-string (text text))))
                   
                   (list
                    (max-width text)))))

(defun row-length (row)
  (reduce #'+ (cells row) :key #'col-span))

(defun %compute-col-widths (table)
  (loop for column in (table-columns table)
        collect (max-width column)))

(defun %document-width ()
  (- (ecase (orientation (current-element 'document))
       (:landscape (elt pdf:*a4-portrait-page-bounds* 3))
       (:portrate (elt pdf:*a4-portrait-page-bounds* 2)))
     (elt *pdf-margins* 0)
     (elt *pdf-margins* 2)))

(defun compute-col-widths (table)
  (let* ((real-widths (%compute-col-widths table))
         (paper-width (- (%document-width)
                         (* *pdf-table-padding* 2)
                         ;; borders
                         (+ (* *pdf-table-border* (length real-widths))
                            *pdf-table-border*)
                         ;; paddings
                         (+ (* 2 *pdf-table-cell-padding*
                               (length real-widths)))))
         (real-width (reduce #'+ real-widths))
         (scale (min (/ paper-width real-width)
                     1)))
    (loop for width in real-widths
          collect (max (* width scale) 12))))

(defun filter-header-rows (rows)
  (flet ((header-row-p (row)
           (loop for cell in (cells row)
                 always (typep cell 'header))))
    (loop for (row . rest) on rows
          while (header-row-p row)
          collect row into header-rows
          finally (return (values header-rows (cons row rest))))))

(defun render-header-row (row)
  (tt:header-row (:background-color (make-color *header-color* :pdf))
    (render-objects (cells row) :pdf)))

(defmethod render-element ((table table) (type (eql :pdf)))
  (tt:table (:col-widths (compute-col-widths table)
             :splittable-p t
             :border *pdf-table-border*
             :padding *pdf-table-padding*
             :cell-padding *pdf-table-cell-padding*)
    (multiple-value-bind (header-rows rest)
        (filter-header-rows (rows table))
      (when (title table)
        (tt:header-row ()
          (tt:cell (:v-align :center
                    :col-span 0
                    :border nil)
            (tt:paragraph (:h-align :center
                           :font-size 14)
              (tt:put-string (title table))))))
      (mapcar #'render-header-row header-rows)
      (render-objects rest type))))

(defmethod render-element ((row row) (type (eql :pdf)))
  (tt:row ()
    (render-objects (cells row) type)))

(defmethod render-element ((cell cell) (type (eql :pdf)))
  (tt:cell (:row-span (row-span cell)
            :col-span (col-span cell)
            :v-align (v-align cell)
            :background-color (make-color (bg-color cell) type))
    (tt:paragraph (:h-align (h-align cell))
      (when (text cell)
        (render-objects (text cell) type)))))

(defmethod render-element ((element html) (type (eql :pdf)))
  (unless (html-only element)
    (render-objects (html-to-document (rendered-string element))
                    type)))

(defmethod render-element ((heading heading) (type (eql :pdf)))
  (check-type (size heading) (integer 1 6))
  (tt:paragraph (:font-size (elt *pdf-heading-sizes*
                                 (size heading)))
    (tt:put-string (text heading))))

(defmethod render-element ((page new-page) (type (eql :pdf)))
  (tt:new-page))

(defmethod render-element ((line new-line) (type (eql :pdf)))
  (tt:new-line))

(defmethod render-element ((element pdf) (type (eql :pdf)))
  (funcall (pdf-code element)))

(defun jpeg-size (path)
  (multiple-value-bind (image y x) (jpeg:decode-image path)
    (declare (ignore image))
    (values x y)))

(defun image-path (path)
  (merge-pathnames (enough-namestring path "/") *pdf-image-path*))

(defmethod render-element ((image image) (type (eql :pdf)))
  (let ((path (image-path (path image))))
    (multiple-value-bind (x y) (jpeg-size path)
      (tt:image :file path :dx x :dy y))))
