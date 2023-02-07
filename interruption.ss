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
              exn:break?
              printf))


(define (interrupt-sleep)
  (with-handlers ([exn:break? (lambda (x) (printf "Interrupted by ~a ~n" x))])
    (displayln "start")
     (sleep 5)
     (displayln "end")))

(define (interrupt-sleep-test)
  (let ([t (thread interrupt-sleep)])
    (sleep 3)
    (break-thread t)))
