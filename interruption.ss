#!r6rs


(import (rnrs base (6))
        (only (racket base)
              thread
              parameterize-break
              break-thread
              kill-thread
              sleep
              displayln))


(define (foo)
  (displayln "start")
  (sleep 1.5)
  (displayln "end"))
