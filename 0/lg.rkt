#lang racket

(require racket/class
	 ffi/unsafe
	 ffi/unsafe/define)

(require "gobject.rkt")

;;;

(define liblg (ffi-lib "/home/aka/devel/girac/0/code/liblg"))

;;;
(define _component-pointer (_cpointer 'LgComponent))

(define _component-to-string
  (get-ffi-obj "lg_component_to_string" liblg
	       (_fun _component-pointer _pointer -> _int)))

(define (component_to_string_wrap% obj)
  (let ((buff (malloc 'atomic (+ 1 (_component-to-string obj #f)))))
      (_component-to-string obj buff)
      (cast buff _pointer _string)))

(define component%
  (class gobject%
    (init raw-ptr)
    (super-new (raw-ptr raw-ptr))
    (inherit-field get-raw-ptr)
    (define (casted) (cast (get-raw-ptr) _pointer _component-pointer))
    (define/public (to-string) (component_to_string_wrap% (casted)))))

;;;
(define _button-pointer (_cpointer 'LgButton))

(define _button-new
  (get-ffi-obj "lg_button_new" liblg (_fun -> _button-pointer)))

(define button%
  (class component%
    (super-new (raw-ptr (_button-new)))
    (inherit-field get-raw-ptr)
    (define (casted) (cast (get-raw-ptr) _pointer _button-pointer))))
