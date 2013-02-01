(in-package :dyb)



(setf *random-state* (make-random-state t))

(setf hunchentoot:*catch-errors-p* nil)

(defvar *dyb-acceptor* (make-instance 'dyb-acceptor :port 8090))

(unless (started *dyb-acceptor*)
  (start *dyb-acceptor*))

(defparameter *dyb-ajax-processor*
  (make-instance 'ht-simple-ajax:ajax-processor :server-uri "/dyb/ajax"))

(defun call-lisp-function (processor)
  "This is called from hunchentoot on each ajax request. It parses the
   parameters from the http request, calls the lisp function and returns
   the response."
  (let* ((fn-name (string-trim "/" (subseq (script-name* *request*)
                                           (length (ht-simple-ajax::server-uri processor)))))
         (fn (gethash fn-name (ht-simple-ajax::lisp-fns processor)))
         (args (mapcar #'cdr (get-parameters* *request*))))
    (unless fn
      (error "Error in call-lisp-function: no such function: ~A" fn-name))

    (setf (reply-external-format*) (reply-external-format processor))
    (setf (content-type*) (content-type processor))
    (no-cache)
    (apply fn args)))

(defun create-ajax-dispatcher (processor)
  "Creates a hunchentoot dispatcher for an ajax processor"
  (create-prefix-dispatcher (ht-simple-ajax::server-uri processor)
                            (lambda () (call-lisp-function processor))))

(defvar *ajax-prefix-dispatcher* (create-ajax-dispatcher *dyb-ajax-processor*))

(pushnew *ajax-prefix-dispatcher* *dispatch-table*)


(defparameter *installation* "Local Machine");;"Live Serve"

(defparameter *site-url* "http://dxw.co.za/")

(if (string-equal *installation* "Live Serve")
  (setf *site-url* "http://dxw.co.za/")
  (setf *site-url* "http://local.dataxware.co.za/"))

(unless (string-equal *installation* "Live Serve")
  (declaim (optimize (speed 0) (space 0) (debug 3)))
  )