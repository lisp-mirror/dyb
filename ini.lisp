(in-package :ems)

(setf hunchentoot:*catch-errors-p* nil)

(defvar *acceptor* (make-instance 'ems-acceptor :port 8000))

(unless (started *acceptor*)
  (start *acceptor*))

(defparameter *ajax-processor*
  (make-instance 'ht-simple-ajax:ajax-processor :server-uri "/ems/ajax"))

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

(setf *dispatch-table* (list 'dispatch-easy-handlers 
                             (create-ajax-dispatcher *ajax-processor*)))
