(in-package :dyb)

(defun handle-short-url (short)
  (let ((url (expand-short-url short)))
    (cond (url
           (incf (click-count url))
           (persist url)
           (redirect-external (url url)))
          (t
           (setf (return-code*) +http-not-found+)
           (abort-request-handler)))))

(define-easy-handler (shorten-url :uri (lambda (x)
                                         (alexandria:starts-with-subseq
                                          "/dyb/s/"
                                          (script-name x)))
                                  :for-everyone t) ()
  (handle-short-url (subseq (script-name*) (length "/dyb/s/"))))

(define-easy-handler (shorten-url-2 :uri (lambda (x)
                                           (alexandria:starts-with-subseq
                                            "/s/"
                                            (script-name x)))
                                    :for-everyone t) ()
  (handle-short-url (subseq (script-name*) (length "/s/"))))
