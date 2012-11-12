(in-package :dyb)

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
(defparameter *site-url* "http://localhost/")

(defparameter *facebook-oauth-uri* "https://www.facebook.com/dialog/oauth")
(defparameter *facebook-access-token-uri* "https://graph.facebook.com/oauth/access_token")
(defparameter *facebook-client-id* "254949787943221")
(defparameter *facebook-client-secret* "b7ca6b0a7243cb0df0b39d009257e4b2")
(defparameter *facebook-callback-uri* "http://app.digyourbrand.co.za/dyb/fbcallback")


(defparameter *twitter-client-id* "4O5D2Awvn4lv1r7RJmWoAA")
(defparameter *twitter-client-secret* "SGATve6iTNXyZ5ZZGTazNmNuf85acHjDHybv68")
(defparameter *twitter-oauth-uri* "https://api.twitter.com/oauth/request_token")
(defparameter *twitter-access-token-uri* "https://api.twitter.com/oauth/access_token")
(defparameter *twitter-oauth-authorize-uri* "https://api.twitter.com/oauth/authorize")
(defparameter *twitter-callback-uri* "http://app.digyourbrand.co.za/dyb/twitcallback")


(cond ((string-equal *installation* "Live Serve")
       (setf *facebook-callback-uri* "http://app.digyourbrand.co.za/dyb/fbcallback")
       (setf *twitter-callback-uri* "http://app.digyourbrand.co.za/dyb/twitcallback")
       (setf *facebook-client-id* "254949787943221")
       (setf *facebook-client-secret* "b7ca6b0a7243cb0df0b39d009257e4b2")
       (setf *twitter-client-id* "4O5D2Awvn4lv1r7RJmWoAA")
       (setf *twitter-client-secret* "SGATve6iTNXyZ5ZZGTazNmNuf85acHjDHybv68"))
      ((string-equal *installation* "Local Machine")
       (setf *facebook-callback-uri* "http://local.dataxware.co.za/dyb/fbcallback")
       (setf *twitter-callback-uri* "http://local.dataxware.co.za/dyb/twitcallback")
       (setf *facebook-client-id* "470974262943332")
       (setf *facebook-client-secret* "60e49502079c8b82391e28e273513ce2")
       (setf *twitter-client-id* "H5wWh6azz3n0Go4hOu5kgg")
       (setf *twitter-client-secret* "m7u7UoEyTPIg5p0Gl1EV73hkl139tu3GkZjMetzS7G8")
       ))
