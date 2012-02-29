#lang racket

(require "phase-1-utils.rkt"
         ffi/unsafe
	 ffi/unsafe/define)

(provide ensure-gtypes-initialized)

(define-ffi-definers define-libgobject
                     (ffi-lib "libgobject-2.0" "0")
                     #:lispify lispify-string~)

;;; initialize gtype
;;; must be invoked before any other action with gtype system

(define-libgobject/n g_type_init (_fun -> _void))

(define ensure-gtypes-initialized
  (let ((initialized #f))
    (lambda ()
      (unless initialized
        (displayln "initializing gobject....")
	(g-type-init)
	(set! initialized #t)))))

(ensure-gtypes-initialized)

;;; 

; (define _gobject-ptr (_cpointer 'GObject))
; (ffi-wrap "g_object_unref" (_gobject-ptr -> _void))

; (define (gobject-unref obj)
;   (g-object-unref (cast obj _pointer _gobject-ptr)))

; (define (unref-hook f (unref-fn gobject-unref))
;   (lambda args
;     (let ((result (apply f args)))
;       (register-finalizer result unref-fn)
;       result)))
