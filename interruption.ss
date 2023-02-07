#!r6rs


(import (rnrs base (6))
        (only (racket base)
              thread
              parameterize-break
              break-thread
              kill-thread
              sleep
              displayln
              with-handlers
              exn:break
              printf))


(define (foo)
  (with-handlers ([exn:break (lambda (x) (printf "Interrupted by ~a" x))])
    (displayln "start")
     (sleep 5)
     (displayln "end")))
