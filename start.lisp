(in-package :dyb)

(fix-sequences)

(unless (started *dx-acceptor*)
  (start *dx-acceptor*))

(start-scheduler)
