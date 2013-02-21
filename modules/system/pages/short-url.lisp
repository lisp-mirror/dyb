(in-package :dyb)

(define-easy-handler (shorten-url :uri (lambda (x)
                                         (alexandria:starts-with-subseq
                                          "/dyb/s/"
                                          (script-name x)))
                                  :for-everyone t) ()
  (let* ((short (subseq (script-name*) (length "/dyb/s/")))
         (url (expand-short-url short))
         (obj (find-short-url-object short) ))

    (when url
        (redirect url)
        (if (get-val obj 'click-count)
          (incf (get-val obj 'click-count) )
          (setf (get-val obj 'click-count) 1 ))
        (persist obj))

    (unless url
      (setf (return-code*) +http-not-found+))))
