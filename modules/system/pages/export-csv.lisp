(in-package :ems)

(define-easy-handler (export-csv-page :uri "/ems/export-csv")
    (grid script-name)
  (setf (content-type*) "text/plain")
  (export-csv (get-widget grid :script-name script-name)))
