#lang racket

(require racket/class
	 ffi/unsafe
	 ffi/unsafe/define)


(provide gobject%)

;;;
(define libgobject (ffi-lib "/usr/lib/libgobject-2.0"))

(define g-type-init (get-ffi-obj "g_type_init" libgobject (_fun -> _void)))

(define ensure-gtypes-initialized
  (let ((initialized #f))
    (lambda ()
      (unless initialized
	(g-type-init)
	(set! initialized #t)))))

(ensure-gtypes-initialized)

(define _gobject-pointer (_cpointer 'GObject))

(define _g-object-unref
  (get-ffi-obj "g_object_unref" libgobject (_fun _gobject-pointer -> _void)))

(define gobject%
  (class object%
    (init-field raw-ptr) ;;todo: allow default ctor?
    (field (get-raw-ptr (lambda () raw-ptr)))
    (register-finalizer raw-ptr
			(lambda (obj)
			  (_g-object-unref (cast obj _pointer _gobject-pointer))))
    (super-new)))
