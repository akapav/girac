#lang racket

(require racket/class
	 ffi/unsafe
	 ffi/unsafe/define)

(define libgobject (ffi-lib "/usr/lib/libgobject-2.0"))
;;;
(define g-type-init (get-ffi-obj "g_type_init" libgobject (_fun -> _void)))

(define ensure-gtypes-initialized
  (let ((initialized #f))
    (lambda ()
      (unless initialized
	(g-type-init)
	(set! initialized #t)))))

;;;
(ensure-gtypes-initialized)

(define _gobject-pointer (_cpointer 'GObject))

(define _g-object-unref
  (get-ffi-obj "g_object_unref" libgobject (_fun _gobject-pointer -> _void)))

(define (g-object-unref obj)
  (_g-object-unref (cast obj _pointer _gobject-pointer)))

(define gobject%
  (class object%
    (init-field raw-ptr) ;;todo: allow default ctor?
    (field (get-raw-ptr (lambda () raw-ptr)))
    (register-finalizer raw-ptr g-object-unref)
    (super-new)))

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
