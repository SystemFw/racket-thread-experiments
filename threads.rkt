#lang racket



(begin
  (print "start")
  (thread (lambda ()
            (sleep 2)
            (print "thead done")))
  (print "spawner done"))


;; https://racket.discourse.group/t/what-would-it-take-to-write-an-independent-racket-interpreter/951
