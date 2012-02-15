#lang racket

(require ffi/unsafe
	 ffi/unsafe/define
	 "./ffi-wrap.rkt")

(provide ensure-gtypes-initialized unref-hook)

;;; initialize gtype
;;; must be invoked before any other action with gtype system

(define libgobject (ffi-lib "/usr/lib/libgobject-2.0"))
(define *lib* libgobject)

(ffi-wrap "g_type_init" ( -> _void))

(define ensure-gtypes-initialized
  (let ((initialized #f))
    (lambda ()
      (unless initialized
	(g-type-init)
	(set! initialized #t)))))

(ensure-gtypes-initialized)

;;; 

(define _gobject-ptr (_cpointer 'GObject))
(ffi-wrap "g_object_unref" (_gobject-ptr -> _void))

(define (gobject-unref obj)
  (g-object-unref (cast obj _pointer _gobject-ptr)))

(define (unref-hook f (unref-fn gobject-unref))
  (lambda args
    (let ((result (apply f args)))
      (register-finalizer result unref-fn)
      result)))
