#lang racket

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


(define basic-thread
  (thunk
   (println "start")
   (thread (thunk
            (sleep 2)
            (println "thead done")))
   (println "spawner done")))

(define test-breaks-default
  (thunk
   (print "main thread")
   (println (break-enabled))
   (thread
    (thunk
     (print "spawned thread")
     (println (break-enabled))))))


;; deterministically prints done-3-2-1
(define test-spawn-order
  (thunk
   (thread (thunk (println 1)))
   (thread (thunk (println 2)))
   (thread (thunk (println 3)))
   (println "done")))

;; deterministically prints 2-1-done-3
(define test-spawn-order-2
  (thunk
   (thread (thunk (println 1)))
   (thread (thunk (println 2)))
   (sleep 1)
   (thread (thunk (println 3)))
   (println "done")))


;; As predicted, the second spawned thread hangs
;; (they spawn in reverse order, so "foo" hangs)
(define test-semaphore-no-broadcast
  (thunk
   (define sem (make-semaphore))
   (define event (semaphore-peek-evt sem))
   (define (thread-logic name)
     (thunk
      (printf "Thread ~a started ~n" name)
      (semaphore-wait sem)
      (printf "Thread ~a finished ~n" name)))
   (println "Main thread started")
   (thread (thread-logic "foo"))
   (thread (thread-logic "bar"))
   (sleep 3)
   (semaphore-post sem)
   (println "Main thread finished")))

;; Simply using the semaphore as an event behaves as semaphore-wait
(define test-semaphore-simple-event-no-broadcast
  (thunk
   (define sem (make-semaphore))
   (define event (semaphore-peek-evt sem))
   (define (thread-logic name)
     (thunk
      (printf "Thread ~a started ~n" name)
      (unless (sync/timeout 5 sem)
        (printf "Thread ~a timed out ~n" name))
      (printf "Thread ~a finished ~n" name)))
   (println "Main thread started")
   (thread (thread-logic "foo"))
   (thread (thread-logic "bar"))
   (sleep 3)
   (semaphore-post sem)
   (println "Main thread finished")))
