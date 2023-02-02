#lang racket
(require racket/unsafe/ops)

(struct promise (semaphore event [value #:mutable]))

(define (make-promise)
  (let* ([sem (make-semaphore)]
         [evt (semaphore-peek-evt sem)])
    (promise sem evt 'promise-empty-marker)))

(define (promise-read promise)
  (let* ([value (promise-value promise)]
         [empty? (eq? value 'promise-empty-marker)])
    (cond
      [empty? (begin
                (sync (promise-event promise))
                (promise-read promise))]
      [else value])))

(define (promise-write promise new-value)
  (let* ([value (promise-value promise)]
         [empty? (eq? value 'promise-empty-marker)])
    (cond
      [empty?
       (let ([no-conflict
             (parameterize-break #f
               (let ([no-conflict (unsafe-struct*-cas! promise 2 value new-value)])
                 (when no-conflict (semaphore-post (promise-semaphore promise)))
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
    (println "main thread started")
    (spawn-promise-reader "foo" p)
    (spawn-promise-reader "bar" p)
    (sleep 3)
    (promise-write p 42)
    (promise-write p 73)
    (spawn-promise-reader "baz" p)
    (println "main thread finished")))
