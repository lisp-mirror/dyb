(in-package :dyb)

(defclass short-url (doc)
  ((url :initarg :url
        :initform nil
        :accessor url)
   (short :initarg :short
              :initform nil
              :accessor short)
   (click-count :initarg :click-count
                :initform 0
                :accessor click-count))
  (:metaclass storable-class))

(defun short-url-collection ()
  (get-collection (system-db) "short-url"))

(defun short-urls ()
  (docs (short-url-collection)))

(defmethod doc-collection ((doc short-url))
  (short-url-collection))

(defun expand-short-url (url)
  (loop for short-url across (short-urls)
        when (equalp (short short-url)
                     url)
        return short-url))

(defun find-short-url (url)
  (loop for short-url across (short-urls)
        when (equalp (url short-url)
                     url)
        return (short short-url)))

(defun make-short-url (url)
  (or (find-short-url url)
      (loop for short = (format nil "~(~32r~)" (random 999999))
            unless (find-short-url short)
            return
            (let ((new (make-instance 'short-url
                                      :url url
                                      :short short
                                      :click-count 0)))
              (persist new)
              short))))

(defun format-short-url (short)
  (if short
      (format nil "~adyb/s/~a" *site-url* short)))


(defun shortify-string (text)
  (ppcre:regex-replace-all
   "(https://|http://|www\\.)\\S+" text
   (lambda (string start end match-start match-end &rest rest)
     (declare (ignore start end rest))
     (let* ((url (subseq string match-start match-end))
            (last (alexandria:last-elt url))
            (special-last (case last
                            (#\)
                             ;; For
                             ;; http://en.wikipedia.org/wiki/Egypt_(bird)
                             (not (find #\( url)))
                            (t
                             (find last ",.(:;"))))
            (url (if special-last
                     (subseq url 0 (1- (length url)))
                     url))
            (suffix (if special-last
                        last
                        "")))
       (frmt "~a~a"
             (format-short-url (make-short-url url))
             suffix)))))

(unless (short-url-collection)
  (add-collection (system-db) "short-url"
                  :collection-class 'dyb-collection
                  :load-from-file-p t))
