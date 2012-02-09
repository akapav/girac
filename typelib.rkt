#lang racket

(require ffi/unsafe
	 ffi/unsafe/define)

(define libgobject (ffi-lib "/usr/lib/libgobject-2.0"))

(define g-type-init
  (get-ffi-obj "g_type_init" libgobject (_fun -> _void)))

(define ensure-gtypes-initialized
  (let ((initialized #f))
    (lambda ()
      (unless initialized
	(g-type-init)
	(set! initialized #t)))))

(ensure-gtypes-initialized)

(define libtl (ffi-lib "/usr/lib/libgirepository-1.0.so"))

;;;

(define _gint _int)
(define _gboolean _gint)

(define (gbool->bool gbool)
  (not (zero? gbool)))

(define _typelib-ptr (_cpointer/null 'GITypelib))
(define _repos-ptr (_cpointer/null 'GIRepository))

(define _base-info-ptr (_cpointer 'GIBaseInfo))
(define _gobjinfo-ptr (_cpointer/null 'GObjectInfo))
(define _functioninfo-ptr (_cpointer 'GIFunctionInfo))
(define _callableinfo-ptr (_cpointer 'GIClaalbleInfo))
(define _typeinfo-ptr (_cpointer 'GITypeInfo))
(define _arginfo-ptr (_cpointer 'GIArgInfo))
(define _structinfo-ptr (_cpointer 'GIStructInfo))
(define _registered-ptr (_cpointer 'GIREgisteredTypeInfo))

(define _gerror-ptr (_cpointer/null 'Gerror))
(define _gerror-ptr-ptr (_cpointer/null _gerror-ptr))

(define gir-repostiry-prepend-path
  (get-ffi-obj "g_irepository_prepend_search_path" libtl
	       (_fun _string -> _void)))

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

(define gir-get-n-infos
  (get-ffi-obj "g_irepository_get_n_infos" libtl
	       (_fun _repos-ptr
		     _string ->
		     _gint)))

(define gir-get-info
  (get-ffi-obj "g_irepository_get_info" libtl
	    (_fun _repos-ptr
		  _string
		  _gint ->
		  _base-info-ptr)))

(define (gir-info/list ns)
  (let ((cnt (gir-get-n-infos #f ns)))
    (for/list ((i (in-range cnt)))
	      (gir-get-info #f ns i))))

(define _gir-base-type
  (get-ffi-obj "g_base_info_get_type" libtl
	       (_fun _base-info-ptr -> _gint)))


(define-for-syntax (map-syntax env f stx)
  (datum->syntax env (map f (syntax->datum stx))))

(define-for-syntax (base-type-predicate-name name)
  (string->symbol
   (string-append "gir-type-" (symbol->string name) "?")))

(define-syntax gir-base-types
  (lambda (stx)
    (syntax-case stx ()
	((_ (name val) ...)
	 (with-syntax ((gir-get-type
			(datum->syntax stx 'gir-get-type))
		       ((predicate ...)
			(map-syntax stx base-type-predicate-name #'(name ...))))
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
(define (object-info base-info)
  (cast base-info _base-info-ptr _gobjinfo-ptr))

(define object-get-parent
  (get-ffi-obj "g_object_info_get_parent" libtl
	       (_fun _gobjinfo-ptr -> _gobjinfo-ptr)))

(define object-type-name
  (get-ffi-obj "g_object_info_get_type_name" libtl
	       (_fun _gobjinfo-ptr -> _string)))


(define _object-get-abstract
  (get-ffi-obj "g_object_info_get_abstract" libtl
	       (_fun _gobjinfo-ptr -> _gboolean)))

(define (object-abstract? obj-info)
  (gbool->bool (_object-get-abstract obj-info)))


(define object-get-n-mtds
  (get-ffi-obj "g_object_info_get_n_methods" libtl
	       (_fun _gobjinfo-ptr -> _gint)))

(define object-get-mtd
  (get-ffi-obj "g_object_info_get_method" libtl
	       (_fun _gobjinfo-ptr _gint -> _functioninfo-ptr)))

(define (object-method/list obj-info)
  (let ((cnt (object-get-n-mtds obj-info)))
    (for/list ((i (in-range cnt)))
	      (object-get-mtd obj-info i))))

;;;
(define function-get-flags
  (get-ffi-obj "g_function_info_get_flags" libtl
	       (_fun _functioninfo-ptr -> _int)))

(define-for-syntax (function-flag-predicate-name name)
  (string->symbol
   (string-append "function-is-" (symbol->string name) "?")))

(define-syntax function-flags
  (lambda (stx)
    (syntax-case stx ()
      ((_ (name shft) ...)
       (with-syntax (((predicate ...)
		      (map-syntax stx
				  function-flag-predicate-name #'(name ...))))
	 #'(begin
	     (define predicate
	       (lambda (func-info)
		 (let ((flags (function-get-flags func-info)))
		   (not (zero?
			 (bitwise-and flags
				      (arithmetic-shift 1 shft))))))) ...))))))

(function-flags
 (method 0)
 (constructor 1)
 (getter 2)
 (setter 3)
 (wraps-vfunc 4)
 (throws 5))

(define (function-info base-info)
  (cast base-info _base-info-ptr _functioninfo-ptr))

(define function-symbol
  (get-ffi-obj "g_function_info_get_symbol" libtl
	       (_fun _functioninfo-ptr -> _string)))

(define (func->callable-info func-info)
  (cast func-info _functioninfo-ptr _callableinfo-ptr))

(define callable-return-type
  (get-ffi-obj "g_callable_info_get_return_type" libtl
	       (_fun _callableinfo-ptr -> _typeinfo-ptr)))

(define callable-get-n-args
  (get-ffi-obj "g_callable_info_get_n_args" libtl
	       (_fun _callableinfo-ptr -> _gint)))

(define callable-get-arg
  (get-ffi-obj "g_callable_info_get_arg" libtl
	       (_fun _callableinfo-ptr _gint -> _arginfo-ptr)))

(define (callable-args/list call-info)
  (let ((cnt (callable-get-n-args call-info)))
    (for/list ((i (in-range cnt)))
	      (callable-get-arg call-info i))))

(define type-get-tag
  (get-ffi-obj "g_type_info_get_tag" libtl
	       (_fun _typeinfo-ptr -> _int)))

(define type-get-name
  (get-ffi-obj "g_type_tag_to_string" libtl
	       (_fun _typeinfo-ptr -> _string)))

(define type-get-iface
  (get-ffi-obj "g_type_info_get_interface" libtl
	       (_fun _typeinfo-ptr -> _base-info-ptr)))


(define _type-get-is-pointer
  (get-ffi-obj "g_type_info_is_pointer" libtl
	       (_fun _typeinfo-ptr -> _gboolean)))

(define (type-is-pointer? type-info)
  (gbool->bool (_type-get-is-pointer type-info)))

(define type-array-length
  (get-ffi-obj "g_type_info_get_array_length" libtl
	       (_fun _typeinfo-ptr -> _gint)))

(define type-array-type
  (get-ffi-obj "g_type_info_get_array_type" libtl
	       (_fun _typeinfo-ptr -> _int)))

(define argument-get-type
  (get-ffi-obj "g_arg_info_get_type" libtl
	       (_fun _arginfo-ptr -> _typeinfo-ptr)))

;; wrong!
;; (define gir-find-by-type
;;   (get-ffi-obj "g_irepository_find_by_gtype" libtl
;; 	       (_fun _repos-ptr _typeinfo-ptr -> _base-info-ptr)))

(define (type->list type)
  (let* ((tag (type-get-tag type))
	 (ptr (type-is-pointer? type))
	 (type-sym
	  (case tag
	    ((0) '_void)
	    ((1) '_gboolean)
	    ((2) '_gint8)
	    ((3) '_guint8)
	    ((4) '_gint16)
	    ((5) '_guint16)
	    ((6) '_gint32)
	    ((7) '_guint32)
	    ((8) '_gint64)
	    ((9) '_guint64)
	    ((10) '_gfloat)
	    ((11) '_gdouble)
	    ((12) '_gtype)
	    ((13) (if ptr '_string
		      "not supported: GI_TYPE_TAG_UTF8"))
	    ((14) "not supported: GI_TYPE_TAG_FILENAME")
	    ((15) `(array ,(type-array-type type)))
	    ((16) (let ((bt (gir-get-type (type-get-iface type))))
		    (case bt
		      ((object)
		       (object-type-name (object-info (type-get-iface type))))
		      ((struct)
		       (let ((name (registered-get-name
				    (struct->registered-info
				     (struct-info
				      (type-get-iface type))))))
			 (if name (string->symbol name)
			     '_pointer)))
		      (else bt))))
	     ((17) "not supported: GI_TYPE_TAG_GLIST")
	     ((18) "not supported: GI_TYPE_TAG_GSLIST")
	     ((19) "not supported: GI_TYPE_TAG_GHASH")
	     ((20) "not supported: GI_TYPE_TAG_ERROR")
	     ((21) "not supported: GI_TYPE_TAG_UNICHAR"))))
	 (if (and (symbol? type-sym)
		  (not (eq? type-sym '_string))
		  (not (eq? type-sym '_pointer))
		  ptr)
	     (cons '* type-sym)
	     type-sym)))

(define (struct-info base-info)
  (cast base-info _base-info-ptr _structinfo-ptr))

(define (struct->registered-info struct-info)
  (cast struct-info _structinfo-ptr _registered-ptr))

(define registered-get-name
  (get-ffi-obj "g_registered_type_info_get_type_name" libtl
	       (_fun _registered-ptr -> _string)))
;;;

(define (callable-return-dump fi)
  (let ((type (callable-return-type (func->callable-info fi))))
    (type->list type)))
                                                         
(define (callable-args-dump fi)
  (let ((args (callable-args/list(func->callable-info fi))))
    (map (compose type->list argument-get-type) args)))

(define (function-dump fi)
  (list (function-symbol fi)
	(function-get-flags fi)
	(callable-args-dump fi) '=>
	(callable-return-dump fi)))

(gir-require "GLib")
(gir-require "GObject")
(gir-require "GIRepository")

(map (lambda (base-info)
       (cond ((gir-type-object? base-info)
	      (map function-dump (object-method/list (object-info base-info))))
	     ((gir-type-function? base-info)
	      (function-dump (function-info base-info)))
	     ((gir-type-struct? base-info) 'struct)
	     (else (begin (display "skipping: ")
			  (displayln (gir-get-type base-info))
			  #f))))
	     (gir-info/list "GIRepository"))
