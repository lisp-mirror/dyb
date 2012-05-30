(in-package :ems)

;;TODO: Parse errors 
;;((:ERRORS ((:CODE . 17) (:MESSAGE . "No user matches for specified terms"))))
(defun get-twit-user-id (nick)
  (let ((id-thing (json::decode-json-from-string
                   (babel:octets-to-string
                    (drakma::http-request   
                     (format nil 
                             "https://api.twitter.com/1/users/lookup.json?screen_name=~A&include_entities=false" nick))))))
   ; (break "~a ~% ~a" id-thing  )
    ;(break "~a" (first (first (car id-thing))) )
    (if (listp (first (first id-thing)))
                      (gethash :id (json:MAKE-OBJECT (first id-thing) 'hash-table))
               (if (string-equal (first (first id-thing)) "errors")
                   -1
                   (gethash :id (json:MAKE-OBJECT (first id-thing) 'hash-table))))))
