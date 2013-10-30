(in-package :dyb)

(fix-sequences)

(clear-db-cache (system-db))

(unless (started *dx-acceptor*)
  (start *dx-acceptor*))

(start-scheduler)
