(in-package :dyb)

(defclass short-url (doc)
  ((url :initarg :url
        :initform nil
        :accessor url)
   (short :initarg :short
              :initform nil
              :accessor short))
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
        return (url short-url)))

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
                                      :short short)))
              (persist new)
              short))))

(defun format-short-url (short)
  (format nil "~adyb/s/~a" *site-url* short))

(add-collection (system-db) "short-url"
                :collection-class 'dyb-collection
                :load-from-file-p t)
