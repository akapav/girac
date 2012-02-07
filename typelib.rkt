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
(define _base-info-ptr (_cpointer 'GIBaseInfo))
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

(define gir-require2
  (get-ffi-obj "g_irepository_require" libtl
	       (_fun _repos-ptr
		     _string _string _int  ->
		     _typelib-ptr)))

(define gir-get-so
  (get-ffi-obj "g_irepository_get_shared_library" libtl
	       (_fun _repos-ptr
		     _string  ->
		     _string)))

(define gir-get-n-infos
  (get-ffi-obj "g_irepository_get_n_infos" libtl
	       (_fun _repos-ptr
		     _string ->
		     _int)))

(define gir-get-info
  (get-ffi-obj "g_irepository_get_info" libtl
	    (_fun _repos-ptr
		  _string
		  _int ->
		  _base-info-ptr)))

(define (gir-info/list ns)
  (let ((cnt (gir-get-n-infos #f ns)))
    (for/list ((i (in-range cnt)))
	      (gir-get-info #f ns i))))

(define _gir-base-type
  (get-ffi-obj "g_base_info_get_type" libtl
	       (_fun _base-info-ptr -> _int)))

(define (gir-get-type2 base-info)
  (case (_gir-base-type base-info)
    ((0) 'invalid)
    ((1) 'function)
    ((2) 'callback)
    ((3) 'struct)
    ((4) 'boxed)
    ((5) 'enum)
    ((6) 'flags)
    ((7) 'object)
    ((8) 'interface)
    ((9) 'constant)
    ((10) 'invalid)
    ((11) 'union)
    ((12) 'value)
    ((13) 'signal)
    ((14) 'vfunc)
    ((15) 'property)
    ((16) 'field)
    ((17) 'arg)
    ((18) 'type)
    ((19) 'unresolved)))

(begin-for-syntax
  (define (map-syntax env f stx)
    (datum->syntax env (map f (syntax->datum stx))))

 (define (predicate-name name)
   (string->symbol
    (string-append "gir-type-" (symbol->string name) "?"))))

(define-syntax gir-base-types
  (lambda (stx)
    (syntax-case stx ()
	((_ (name val) ...)
	 (with-syntax ((gir-get-type
			(datum->syntax stx 'gir-get-type))
		       ((predicate ...)
			(map-syntax stx predicate-name  #'(name ...))))
	   #'(begin
	       (define (gir-get-type base-info)
		 (case (_gir-base-type base-info)
		   ((val) 'name) ...))
	       (define predicate
		 (lambda (base-info)
		   (eq? (gir-get-type base-info) 'name))) ...))))))

(gir-base-types
 (invalid 0)
 (function 1)
 (callback 2)
 (struct 3)
 (boxed 4)
 (enum 5)
 (flags 6)
 (object 7)
 (interface 8)
 (constant 9)
 (invalid-0 10)
 (union 11)
 (value 12)
 (signal 13)
 (vfunc 14)
 (property 15)
 (field 16)
 (arg 17)
 (type 18)
 (unresolved 19))
;;;
(gir-repostiry-prepend-path "/home/aka/devel/girac/1")
(gir-require2 #f "TestClass" #f 0)
;(displayln (gir-get-so #f "tc"))

(define libtc (ffi-lib "/home/aka/devel/girac/1/libtc"))

;(displayln (map gir-get-type (gir-info/list "TestClass")))
;;;
(gir-repostiry-prepend-path "/home/aka/devel/girac/0/code")
(gir-require2 #f "Lg" #f 0)
;(displayln (gir-get-so #f "Lg"))

(define liblg (ffi-lib "/home/aka/devel/girac/0/code/liblg"))

(displayln (filter gir-type-object? (gir-info/list "Lg")))
