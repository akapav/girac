#lang racket

(require "girepository.rkt")

(define (check)
  (let ([env (empty-gir-env)])
    (load-gir! env "Gtk" #f #:lax #t #:transitive #f)
    env))

(define (check/time) (time (check)) (void))

(define (check/disp) (show-env (check)))

