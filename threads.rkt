#lang racket
(require racket/unsafe/ops)

(struct promise (semaphore event [value #:mutable]))

(define promise-empty-value 'promise-empty-value-marker)

(define (promise-empty-value? promise-value)
  (eq? promise-value promise-empty-value))

(define (make-promise)
  (let* ([sem (make-semaphore)]
         [evt (semaphore-peek-evt sem)])
    (promise sem evt promise-empty-value)))

(define (promise-try-read promise) (promise-value promise))

(define (promise-read promise)
  (let* ([value (promise-value promise)]
         [empty? (promise-empty-value? value)])
    (cond
      [empty?
       (sync (promise-event promise))
       (promise-read promise)]
      [else value])))

(define (promise-write promise new-value)
  (let* ([value (promise-value promise)]
         [empty? (promise-empty-value? value)]
         [cas! (λ () (unsafe-struct*-cas! promise 2 value new-value))]
         [awake-readers (λ () (semaphore-post (promise-semaphore promise)))])
    (cond
      [empty?
       (let ([no-conflict
             (parameterize-break #f
               (let ([no-conflict (cas!)])
                 (when no-conflict (awake-readers))
                 no-conflict))])
         (if no-conflict #t (promise-write promise new-value)))]
      [else #f])))


(define (spawn-promise-reader name p)
  (thread
   (λ ()
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

