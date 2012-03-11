#lang racket 

(require "../phase-1-utils.rkt"
         ffi/unsafe)

(provide define-glib
         define-glib/n)

(define-ffi-definers define-glib
                     (ffi-lib "libglib-2.0")
                     #:lispify lispify-string~
                     #:provide provide)

