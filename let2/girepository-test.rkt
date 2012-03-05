#lang racket

(require "girepository.rkt")

(define (check)
  (let ([env (empty-gir-env)])
    (load-gir! env "Gtk" #f #:lax #f #:transitive #t)
    env))

(define (check/time) (time (check)) (void))

(define (check/disp) (show-env (check)))

