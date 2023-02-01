#lang racket
(require racket/unsafe/ops)
;; https://racket.discourse.group/t/what-would-it-take-to-write-an-independent-racket-interpreter/951

;; Designing promise:
;; Nuclear option: try and link to the racket internal lib
;; Option 2: queue of semaphores. Cancellation cleanup is kinda annoying
;; Option 3: single semaphore + event (which doesn't dec), easier to cancel

;; Option 3 is equivalent to the following question: is `Promise[A]` <-> `(Promise[Unit], Ref[Option[A]])`

;;; Notes
#; (parameterize-break #f
  (with-handlers ([exn:break? (lambda (x) (void))])
    (semaphore-wait/enable-break s)))
;;
;; ^ this is equivalent to uncancelable { poll => }
;; breaks are enabled by default
;; for events, there is a `sync/enable-break` form

;; TODO how do I represent option here?
;; - just #f --> Option[Boolean] is ambiguous
;; - undefined
;; - a gensym symbol
;; - a struct with two fields
;; - another hardcoded symbol
;; (struct option (tag value))
;; (define option-none (option 'none #f))
;; (define (option-some v) (option 'some v))


(define (basic-thread)
  (println "start")
  (thread
   (λ ()
     (sleep 2)
     (println "thead done")))
  (println "spawner done"))

(define (test-breaks-default)
  (printf "main thread ~a ~n" (break-enabled))
  (thread
   (λ ()
     (printf "spawned thread ~a ~n" (break-enabled)))))

;; deterministically prints done-3-2-1
(define (test-spawn-order)
   (thread (λ () (println 1)))
   (thread (λ () (println 2)))
   (thread (λ () (println 3)))
   (println "done"))

;; deterministically prints 2-1-done-3
(define (test-spawn-order-2)
   (thread (λ () (println 1)))
   (thread (λ () (println 2)))
   (sleep 1)
   (thread (λ () (println 3)))
   (println "done"))


;; As predicted, the second spawned thread hangs
;; (they spawn in reverse order, so "foo" hangs)
(define (test-semaphore-no-broadcast)
  (define sem (make-semaphore))
  (define (thread-logic name)
    (λ ()
      (printf "Thread ~a started ~n" name)
      (semaphore-wait sem)
      (printf "Thread ~a finished ~n" name)))
  (println "Main thread started")
  (thread (thread-logic "foo"))
  (thread (thread-logic "bar"))
  (sleep 3)
  (semaphore-post sem)
  (println "Main thread finished"))

;; Simply using the semaphore as an event behaves as semaphore-wait
(define (test-semaphore-simple-event-no-broadcast)
  (define sem (make-semaphore))
  (define (thread-logic name)
    (λ ()
      (printf "Thread ~a started ~n" name)
      (unless (sync/timeout 5 sem)
        (printf "Thread ~a timed out ~n" name))
      (printf "Thread ~a finished ~n" name)))
  (println "Main thread started")
  (thread (thread-logic "foo"))
  (thread (thread-logic "bar"))
  (sleep 3)
  (semaphore-post sem)
  (println "Main thread finished"))

;; peek works! Broadcasts completion
(define (test-semaphore-peek-event-broadcast)
  (define sem (make-semaphore))
  (define event (semaphore-peek-evt sem))
  (define (thread-logic name)
    (λ ()
      (printf "Thread ~a started ~n" name)
      (unless (sync/timeout 5 event)
        (printf "Thread ~a timed out ~n" name))
      (printf "Thread ~a finished ~n" name)))
  (println "Main thread started")
  (thread (thread-logic "foo"))
  (thread (thread-logic "bar"))
  (sleep 3)
  (semaphore-post sem)
  (println "Main thread finished"))



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
         (if no-conflict #t (promise-write promise)))]
      [else #f])))


         ;; [value-pos 2] ; positition of the value field in the struct
         ;; [cas! (λ (new-value) (unsafe-struct*-cas! promise value-pos value new-value))]
         ;; [write-and-wake! (λ () )]
