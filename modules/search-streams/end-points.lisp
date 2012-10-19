(in-package #:ems)

(defun social-mention-search (search)
  (multiple-value-bind (body)
      (drakma:http-request 
       (format nil "http://api2.socialmention.com/search?q=~A&f=json&t=all&lang=en" 
               search))
    (json:decode-json-from-string (babel:octets-to-string body))))