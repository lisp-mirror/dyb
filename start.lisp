(in-package :dyb)

(unless (started *dx-acceptor*)
  (start *dx-acceptor*))

(start-scheduler)
