(in-package :dyb)

(unless (started *acceptor*)
  (start *acceptor*))

(start-scheduler)
