#lang racket



(define foo
  (thunk
   (print "start")
   (thread (thunk
            (sleep 2)
            (print "thead done")))
   (print "spawner done")))


;; https://racket.discourse.group/t/what-would-it-take-to-write-an-independent-racket-interpreter/951

;; Designing promise:
;; Nuclear option: try and link to the racket internal lib
;; Option 2: queue of semaphores. Cancellation cleanup is kinda annoying
;; Option 3: single semaphore + event (which doesn't dec), easier to cancel


;;; Notes
#; (parameterize-break #f
  (with-handlers ([exn:break? (lambda (x) (void))])
    (semaphore-wait/enable-break s)))
;;
;; ^ this is equivalent to uncancelable { poll => }
