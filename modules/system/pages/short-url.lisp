(in-package :dyb)

(define-easy-handler (shorten-url :uri (lambda (x)
                                         (alexandria:starts-with-subseq
                                          "/dyb/s/"
                                          (script-name x)))
                                  :for-everyone t) ()
  (let* ((short (subseq (script-name*) (length "/dyb/s/")))
         (url (expand-short-url short)))
    (cond (url
           (incf (click-count url))
           (persist url)
           (redirect (url url)))
          (t
           (setf (return-code*) +http-not-found+)
           (abort-request-handler)))))
