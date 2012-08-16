(in-package :ems-quickview)

(defclass document-sink (chtml::sink)
  ())

(defun make-document-sink ()
  (make-instance 'document-sink
                 :encoding "UTF-8"))

(defvar *current-tags* ())
(defvar *converted-objects* ())

(defmethod hax:start-element ((sink document-sink) name attributes)
  (push (cons name attributes) *current-tags*))

(defmethod hax:end-element ((sink document-sink) name)
  (pop *current-tags*)
  (values))

(defmethod hax:end-document ((sink document-sink)))

(defun find-current-tag (name)
  (find name *current-tags*
        :key #'car :test #'string=))

(defmethod hax:characters ((sink document-sink) data)
  (let ((italic (find-current-tag 'em))
        (bold (find-current-tag 'strong)))
    (push (make-text data
                     :style (and italic
                                 :italic)
                     :weight (and bold
                                  :bold))
          *converted-objects*)))

(defun html-to-document (html)
  (when html
   (let ((*converted-objects* ()))
     (chtml:serialize-pt
      (parse-html html)
      (make-document-sink))
     (nreverse *converted-objects*))))
