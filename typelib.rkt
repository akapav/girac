#lang racket

(require ffi/unsafe
	 ffi/unsafe/define)

(define libgobject (ffi-lib "/usr/lib/libgobject-2.0"))

(define g-type-init (get-ffi-obj "g_type_init" libgobject (_fun -> _void)))

(define ensure-gtypes-initialized
  (let ((initialized #f))
    (lambda ()
      (unless initialized
	(g-type-init)
	(set! initialized #t)))))

(ensure-gtypes-initialized)

(define libtl (ffi-lib "/usr/lib/libgirepository-1.0.so"))

(define gir-repostiry-prepend-path
  (get-ffi-obj "g_irepository_prepend_search_path" libtl
	       (_fun _string -> _void)))

(define _typelib-ptr (_cpointer/null 'GITypelib))
(define _repos-ptr (_cpointer/null 'GIRepository))
(define _gerror-ptr (_cpointer/null 'Gerror))
(define _gerror-ptr-ptr (_cpointer/null _gerror-ptr))

(define _gir-require
  (get-ffi-obj "g_irepository_require" libtl
	       (_fun _repos-ptr
		     _string _string _int _gerror-ptr-ptr ->
		     _typelib-ptr)))

(define (gir-require name)
  (unless (let ((err (cast
		      (malloc 'atomic (ctype-sizeof _gerror-ptr))
		      _pointer
		      _gerror-ptr-ptr)))
	    (_gir-require #f name #f 0 err))
    (displayln "load error")))

(define gir-get-so
  (get-ffi-obj "g_irepository_get_shared_library" libtl
	       (_fun _repos-ptr
		     _string  ->
		     _string)))

;;;

(gir-repostiry-prepend-path "/home/aka/devel/girac/1")
(gir-require "TestClass")
;(displayln (gir-get-so #f "tc"))

(gir-repostiry-prepend-path "/home/aka/devel/girac/0/code")
(gir-require "Lg")
;(displayln (gir-get-so #f "Lg"))
