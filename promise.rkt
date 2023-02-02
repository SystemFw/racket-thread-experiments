#!r6rs
(import (rnrs base (6))
        (rnrs records syntactic)
        (only (racket base)
              make-semaphore
              semaphore-peek-evt
              sync/enable-break
              semaphore-post
              parameterize-break
              thread
              printf
              sleep)
        (only (racket unsafe ops) unsafe-struct*-cas!))

(define-record-type
  (promise new-promise promise?)
  (fields semaphore event (mutable value)))

(define promise-empty-value 'promise-empty-value-marker)

(define (promise-empty-value? promise-value)
  (eq? promise-value promise-empty-value))

(define (make-promise)
  (let* ([sem (make-semaphore)]
         [evt (semaphore-peek-evt sem)])
    (new-promise sem evt promise-empty-value)))

(define (promise-try-read promise) (promise-value promise))

;; Unison.tryEval has to include a break-parameterize #f I think
(define (promise-read promise)
  (let loop ()
    (let* ([value (promise-value promise)]
           [full? (not (promise-empty-value? value))])
      (cond
        [full? value]
        [else (sync/enable-break (promise-event promise)) (loop)]))))

(define (promise-write promise new-value)
  (let loop ()
    (let* ([value (promise-value promise)]
           [full? (not (promise-empty-value? value))]
           [cas! (lambda () (unsafe-struct*-cas! promise 2 value new-value))]
           [awake-readers (lambda () (semaphore-post (promise-semaphore promise)))])
      (cond
        [full? #f]
        [else
         (let ([ok (parameterize-break #f (if (cas!) (awake-readers) #f))])
           (if ok #t (loop)))]))))

(define (spawn-promise-reader name p)
  (thread
   (lambda ()
     (printf "Thread ~a started ~n" name)
     (printf "Thread ~a finished with result ~a ~n" name (promise-read p)))))

(define (test-promise)
  (let ([p (make-promise)])
    (printf "Main thread started ~n")
    (printf "Current promise is ~a ~n" (promise-try-read p))
    (spawn-promise-reader "foo" p)
    (spawn-promise-reader "bar" p)
    (sleep 3)
    (promise-write p 42)
    (sleep 1)
    (promise-write p 73)
    (spawn-promise-reader "baz" p)
    (printf "Current promise is ~a ~n" (promise-try-read p))
    (printf "Main thread finished ~n")))

