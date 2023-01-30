#lang racket


(begin
  (print "start")
  (thread (lambda ()
            (sleep 2)
            (print "thead done")))
  (print "spawner done"))
