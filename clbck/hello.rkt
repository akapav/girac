#lang racket

(require ffi/unsafe)

(define libgobject (ffi-lib "/usr/lib/libgobject-2.0"))
(define libgtk (ffi-lib "/usr/lib/libgtk-3"))
(define libhello (ffi-lib "./hello"))

(define _closure-ptr (_cpointer 'GClosure))
(define _gvalue-ptr (_cpointer 'GValue))
(define _marshall-type (_fun _closure-ptr
			     _gvalue-ptr
			     _int
			     _gvalue-ptr
			     _pointer
			     _pointer -> _void))

(define run (get-ffi-obj "run" libhello (_fun -> _void)))
(define foo (get-ffi-obj "foo_closure" libhello (_fun -> _void)))
(define tito (get-ffi-obj "tito_closure" libhello (_fun -> _void)))
(define rkt (get-ffi-obj "half_racket" libhello (_fun _marshall-type -> _void)))

(define pif (get-ffi-obj "pif" libhello (_fun (_fun _int -> _int) -> _void)))
(define *closure* (get-ffi-obj "closure" libhello (_cpointer/null 'GClosure)))

(pif (lambda (n) (+ 3 n)))

(define (marsh a b c d e f) (let loop () (loop)))
;(rkt marsh)
(tito)
(run)
