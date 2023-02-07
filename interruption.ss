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

(define (timeout n thunk)
  (let ([t (thread thunk)])
    (sleep n)
    (break-thread t)))


(define (interruptible-sleep)
  (with-handlers ([exn:break? (lambda (x) (printf "Interrupted by ~a ~n" x))])
    (displayln "start")
     (sleep 5)
     (displayln "end")))

(define (interruptible-sleep-test) (timeout 3 interruptible-sleep))

