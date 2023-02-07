#!r6rs
(import (rnrs base (6))
        (only (racket base)
              thread
              parameterize-break
              break-thread
              break-enabled
              thread-wait
              kill-thread
              sleep
              displayln
              with-handlers
              exn:break?
              printf))

(define (timeout n thunk)
  (let ([t (thread thunk)])
    (sleep n)
    (break-thread t)
    (thread-wait t)))

(define (sleep/enable-break n)
  (break-enabled #t)
  (sleep n)
  (break-enabled #f))

(define default-timeout 3)
(define default-sleep 5)

(define (sleep-thread sleeping-f)
  (with-handlers ([exn:break? (lambda (x) (printf "Interrupted by ~a ~n" x))])
    (displayln "start")
     (sleeping-f default-sleep)
     (displayln "end")))

(define racket-sleep (lambda (n) (sleep n)))

(define (interruptible-sleep-test)
  (timeout default-timeout (lambda ()
               (sleep-thread sleep))))

(define (uninterruptible-sleep-inner-mask-test)
  (timeout default-timeout (lambda ()
               (parameterize-break #f
                 (sleep-thread sleep)))))

(define (interruptible-sleep-inner-mask-test)
  (timeout default-timeout (lambda ()
               (parameterize-break #f
                 (sleep-thread sleep/enable-break)))))



