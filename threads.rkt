#lang racket



(define foo
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
