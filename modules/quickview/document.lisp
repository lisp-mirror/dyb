(in-package :ems-quickview)

(defvar *header-color* "ffffd6")
(defvar *current-elements* nil)

(defclass document-element ()
  ())

(defclass document (document-element)
  ((elements :initarg :elements
             :initform nil
             :accessor elements)
   (orientation :initarg :orientation
                :initform :portrate
                :accessor orientation
                :type (member :portrate :landscape
                              :preivous :next
                              :inherit))
   (header :initarg :header
           :initform nil
           :accessor document-header)))

(defclass document-part (document)
  ())

(defclass paragraph (document-element)
  ((elements :initarg :elements
             :initform nil
             :accessor elements)
   (h-align :initarg :h-align
            :initform :center
            :accessor h-align)))

(defclass text (document-element)
  ((text :initarg :text
         :initform nil
         :accessor text)
   (style :initarg :style
          :initform nil
          :accessor style
          :type '(member nil :italic))
   (weight :initarg :weight
           :initform nil
           :accessor weight
           :type '(member nil :bold))
   (size :initarg :size
         :initform nil
         :accessor size)))

(defclass link (text)
  ((url :initarg :url
        :initform nil
        :accessor url)
   (target :initarg :target
           :initform nil
           :accessor target)))

(defclass heading (text)
  ((size :initarg :size
         :initform 1
         :accessor size
         :documentation "From 1-6, like h1-h6 in HTML")))

(defvar *table-title* nil)

(defclass table (document-element)
  ((title :initarg :title
          :initform *table-title*
          :accessor title)
   (rows :initarg :rows
         :initform nil
         :accessor rows)
   (class :initarg :class
          :initform "quickview_table"
          :accessor html-class)))

(defclass row (document-element)
  ((cells :initarg :cells
          :initform nil
          :accessor cells)))

(defclass cell (document-element)
  ((text :initarg :text
         :initform nil
         :accessor text)
   (row-span :initarg :row-span
             :initform 1
             :accessor row-span)
   (col-span :initarg :col-span
             :initform 1
             :accessor col-span)
   (bg-color :initarg :bg-color
             :initform nil
             :accessor bg-color)
   (v-align :initarg :v-align
            :initform :center
            :accessor v-align)
   (h-align :initarg :h-align
            :initform :center
            :accessor h-align)
   (vertical-text :initarg :vertical-text
                  :initform nil
                  :accessor vertical-text)))

(defun print-text (object stream text)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~@[~a~]" text)))

(defmethod print-object ((cell cell) stream)
  (print-text cell stream (text cell)))

(defmethod print-object ((text text) stream)
  (print-text text stream (text text)))

(defclass header (cell)
  ()
  (:default-initargs :bg-color *header-color*))

(defclass html (document-element)
  ((html-only :initarg :html-only
              :initform t
              :accessor html-only)
   (string :initarg :string
           :initform nil
           :accessor rendered-string)))

(defclass pdf (document-element)
  ((code :initarg :code
         :initform nil
         :accessor pdf-code)))

(defclass new-page (document-element)
  ())

(defclass new-line (document-element)
  ())

(defmethod text ((new-line new-line))
  "
")

(defclass image (document-element)
  ((path :initarg :path
         :initform nil
         :accessor path)))

;;;

(defun cell (text &rest args &key row-span col-span bg-color vertical-text
                                  h-align v-align)
  (declare (ignore row-span col-span bg-color vertical-text h-align v-align))
  (apply #'make-instance 'cell :text text args))

(defun header (text &rest args &key url row-span col-span bg-color vertical-text
                                    h-align v-align)
  (declare (ignore url row-span col-span bg-color vertical-text h-align v-align))
  (apply #'make-instance 'header :text text args))

(defun table (&rest rows)
  (make-instance 'table :rows rows))

(defun row (&rest cells)
  (make-instance 'row :cells cells))

(defun document (&rest elements)
  (make-instance 'document :elements elements))

(defun paragraph (text &rest args &key elements h-align)
  (declare (ignore h-align))
  (apply #'make-instance 'paragraph
         :elements (if text
                       (cons (make-instance 'text :text text) elements)
                       elements)
         args))

(defun heading (text &rest args &key size)
  (declare (ignore size))
  (apply #'make-instance 'heading :text text args))

(defun make-text (string &rest args &key size style weight)
  (declare (ignore size style weight))
  (apply #'make-instance 'text :text string args))

(defun make-link (text &optional url)
  (make-instance 'link :text text
                       :url url))

(defun image (path)
  (make-instance 'image :path path))

(defmacro with-add-html ((&key (html-only t)) &body body)
  `(add (make-instance 'html
                       :html-only ,html-only
                       :string
                       (with-html-to-string ()
                         ,@body))))

(defmacro with-add-pdf (&body body)
  `(add (make-instance 'pdf
                       :code
                       (lambda () ,@body))))

(defun rows-from-alist (alist)
  (loop for (a b) in alist
        do (add (row (cell a)
                  (cell b)))))

(defun new-page ()
  (make-instance 'new-page))

(defun new-line ()
  (make-instance 'new-line))

(defun html (string &rest args &key html-only)
  (declare (ignore html-only))
  (apply #'make-instance 'html :string string
         args))

(defun multiline-text (&rest args)
  (loop for (arg . rest) on args
        collect arg
        when rest
        collect (new-line)))

;;;

(defun spanned-row (row)
  (loop for cell in (cells row)
        append (make-list (col-span cell)
                          :initial-element cell)))

(defun spanned-rows (table)
  (mapcar #'spanned-row
          (rows table)))

(defun table-columns (table)
  (let* ((rows (spanned-rows table))
         (columns-number (reduce #'max rows :key #'length))
         (rows-length (length rows)))
    (loop repeat columns-number
          collect
          (loop for i = 0 then (+ i (if cell
                                        (row-span cell)
                                        1))
                while (< i rows-length)
                for cell = (pop (nth i rows))
                when cell
                collect cell))))

(defgeneric render-element (object type))

(defgeneric make-color (spec type))

(defgeneric h-alignment (spec type))
(defgeneric v-alignment (spec type))

(defgeneric process-text-style (text type))

(defun render-objects (objects type)
  (loop for object in (alexandria:ensure-list objects)
        do (typecase object
             (cons
              (render-objects object type))
             (t
              (render-element object type)))))

(defmethod render-element ((string string) type)
  (render-element (make-instance 'text :text string) type))

(defmethod render-element ((number number) type)
  (render-element (make-instance 'text :text (princ-to-string number))
                  type))

(defmethod render-element ((none null) type))

(defmethod render-element :around (element type)
  (let* ((*current-elements* (or *current-elements*
                                 (make-hash-table :test #'eq))))
    (add-element (type-of element) element)
    (call-next-method)))

;;;

(defun current-element (type)
  (gethash type *current-elements*))

(defun add-element (type element)
  (setf (gethash type *current-elements*) element))

(defun remove-element (type)
  (pop (gethash type *current-elements*)))

(defun push-element (type element)
  (push element (gethash type *current-elements*)))

(defun pop-element (type)
  (pop (gethash type *current-elements*)))

(defun top-element (type)
  (car (gethash type *current-elements*)))

(defmacro with-element ((type element) &body body)
  (let ((type-symbol (gensym "TYPE")))
    `(let ((,type-symbol ,type))
       (unwind-protect
            (progn
              (push-element ,type-symbol ,element)
              ,@body)
         (pop-element ,type-symbol)))))

(defun add-document (&rest args)
  (let* ((current-document (top-element 'document))
         (document (if current-document
                       (apply #'make-instance
                              'document-part
                              (append args
                                      (list :orientation :inherit)))
                       (apply #'make-instance
                              'document args))))
    (when current-document
      (alexandria:appendf (elements current-document) (list document)))
    document))

(defmacro with-document ((&whole args &key orientation) &body body)
  (declare (ignore orientation))
  (let ((document-symbol (gensym "DOCUMENT")))
    `(let* ((*current-elements*
              (or *current-elements* (make-hash-table :test #'eq)))
            (,document-symbol (add-document ,@args)))
       (with-element ('document ,document-symbol)
         ,@body
         ,document-symbol))))

(defmacro with-table ((&whole args &key class) &body body)
  (declare (ignore class))
  `(progn
     (add (make-instance 'table ,@args))
     ,@body))

(defgeneric add (object))

(defmethod add ((element document-element))
  (add-element (type-of element) element)
  (alexandria:appendf (elements (top-element 'document))
   (list element))
  element)

(defmethod add ((table table))
  (add-element 'table table)
  (alexandria:appendf (elements (top-element 'document))
                      (list table))
  table)

(defmethod add ((row row))
  (add-element 'row row)
  (alexandria:appendf (rows (current-element 'table))
                      (list row))
  row)

(defmethod add ((cell cell))
  (alexandria:appendf (cells (current-element 'row))
   (list cell))
  cell)
