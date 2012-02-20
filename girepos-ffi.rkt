#lang racket

(require ffi/unsafe
	 ffi/unsafe/define
	 "./gobject.rkt"
	 "./ffi-wrap.rkt"
	 (for-syntax "./ffi-wrap.rkt")
	 (for-syntax "./names.rkt"))

(define-syntax define/provide
  (syntax-rules ()
    ((define/provide (name args ...) val)
     (define/provide name (lambda (args ...) val)))
    ((define/provide name val)
     (begin
       (provide name)
       (define name val)))))

;;; wrapper for common enumeration form in grepository
;;; (let ((cnt (get-n-items obj)))
;;;   (for/list ((i (in-range cnt)))
;;;      (get-item obj i)))
;;;
(define-syntax define-enumerator->list
  (lambda (stx)
    (syntax-case stx ()
      ((_ name n-infos get-info (args-n ...) (args-get ...))
       (with-syntax ((name #'name)
		     (n-infos #'n-infos)
		     (get-info #'get-info))
	 #'(define (name obj)
	     (let ((cnt (n-infos args-n ... obj)))
	       (for/list ((i (in-range cnt)))
			 (get-info args-get ... obj i)))))))))

(define libtl (ffi-lib "/usr/lib/libgirepository-1.0.so"))

;; todo: use *lib* as a parameter -- doesn't work out of the box since
;; it is forbidden to use 'provide from 'parameterize block
(define *lib* libtl)

;;;; ffi typedefs
(define (gbool->bool gbool)
  (not (zero? gbool)))

(define _gint _int)
(define _gboolean _gint)
(define _gsize _ulong)
(define _typelib-ptr (_cpointer/null 'GITypelib))
(define _repos-ptr (_cpointer/null 'GIRepository))
(define _baseinfo-ptr (_cpointer/null 'GIBaseInfo))
(define _gobjinfo-ptr (_cpointer/null 'GObjectInfo))
(define _functioninfo-ptr (_cpointer 'GIFunctionInfo))
(define _callableinfo-ptr (_cpointer 'GICallableInfo))
(define _typeinfo-ptr (_cpointer 'GITypeInfo))
(define _arginfo-ptr (_cpointer 'GIArgInfo))
(define _structinfo-ptr (_cpointer 'GIStructInfo))
(define _registered-ptr (_cpointer 'GIREgisteredTypeInfo))
(define _enuminfo-ptr (_cpointer 'GIEnumInfo))
(define _valueinfo-ptr (_cpointer 'GIValueInfo))
(define _interfaceinfo-ptr (_cpointer 'GIInterfaceInfo))
(define _gerror-ptr (_cpointer/null 'Gerror))
(define _gerror-ptr-ptr (_cpointer/null _gerror-ptr))

;;;; utils for type handling

;;todo: find proper place -- could be used widely
(define-for-syntax (map-syntax env f stx)
  (datum->syntax env (map f (syntax->datum stx))))

;
(define-for-syntax (base-type-predicate-name name)
  (string->symbol
   (string-append "type-" (symbol->string name) "?")))

(define-syntax gir-base-types
  (lambda (stx)
    (syntax-case stx ()
      ((_ (name val) ...)
       (with-syntax ((gir-get-type
		      (datum->syntax stx 'get-type))
		     ((predicate ...)
		      (map-syntax stx base-type-predicate-name #'(name ...))))
	 #'(begin
	     (define/provide gir-get-type
	       (lambda (base-info)
		 (case (g-base-info-get-type base-info)
		   ((val) 'name) ...)))
	     (define/provide predicate
	       (lambda (base-info)
		 (eq? (gir-get-type base-info) 'name))) ...))))))

;;; generates following forms:
;;; get-type :: _baseinfo-ptr -> symbol (one from below)
;;; type-<type>? :: _baseinfo-ptr -> bool
;;; where <type> is a symbol from the list below
;;;
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

(define-for-syntax (downcast-name name)
  (string->symbol
   (string-append (symbol->string name) "-info")))

(define-syntax gir-base-casts
  (lambda (stx)
    (syntax-case stx ()
      ((_ (name type) ...)
       (with-syntax ((baseinfo-downcast-fn
		      (datum->syntax stx 'baseinfo-downcast-fn))
		     ((cast-name ...)
		      (map-syntax stx downcast-name #'(name ...)))
		     ((type ...)  #'(type ...)))
	 #'(begin
	     (define/provide cast-name
	       (lambda (base-info)
		 (cast base-info _baseinfo-ptr type))) ...
	     (define/provide baseinfo-downcast-fn
	       (lambda (base-info)
		 (case (get-type base-info)
		   ((name) cast-name) ...
		   (else identity))))))))))

;;; generates <type>-info functions for downcasting c pointers from
;;; _baseinfo-ptr to concrete type and baseinfo-downcast-fn which
;;; returns matching cast function for a given value
;;; in type is not in the list (below), identity is applied
;;;
(gir-base-casts
 (function _functioninfo-ptr)
 (object _gobjinfo-ptr)
 (struct _structinfo-ptr)
 (enum _enuminfo-ptr)
 (flags _enuminfo-ptr)
 (interface _interfaceinfo-ptr))

(define-for-syntax (upcast-name dsym usym)
  (string->symbol
   (string-append
    (symbol->string dsym) "->" (symbol->string usym))))

(define-syntax define-upcast
  (lambda (stx)
    (syntax-case stx ()
      ((_ (dname dtype) (uname utype))
       (with-syntax ((upcast
		      (datum->syntax
		       stx (upcast-name (syntax->datum #'dname)
					(syntax->datum #'uname)))))
	 #'(define/provide (upcast ptr)
	     (cast ptr dtype utype)))))))

;;; apply downcast
(define/provide (baseinfo-downcast base-info)
  ((baseinfo-downcast-fn base-info) base-info))

;;;; repository functions

;;; unref function for all baseinfo derived types
(ffi-wrap "g_base_info_unref"
	  (_baseinfo-ptr -> _void))

(define (unref-baseinfo fn (cast identity))
  (unref-hook fn (compose g-base-info-unref cast)))

;;; at the moment, all functions work with the global (default)
;;; repository so #f (null) is always sent as a repository argument

;;; load a repository (typelib)
(ffi-wrap "g_irepository_require"
	  (_repos-ptr _string _string _int _gerror-ptr-ptr
		      -> _typelib-ptr))

;; todo: clumsy hack with malloc for error pointer -- see how to
;; allocate it on the stack
(define/provide (repository-require name (version #f))
  (unless (let ((err (cast
		      (malloc 'atomic (ctype-sizeof _gerror-ptr))
		      _pointer
		      _gerror-ptr-ptr)))
	    (g-irepository-require #f name version 0 err))
    (displayln "load error")))

(ffi-wrap "g_irepository_get_dependencies"
	  (_repos-ptr _string -> (_cpointer _string)))

(define/provide repository-dependencies
  (lambda (name)
    (let ((deps (g-irepository-get-dependencies #f name)))
      (let loop ((ndx 0) (acc '()))
	(let ((str (ptr-ref deps _string ndx)))
	  (free (cast (ptr-ref deps _string ndx)
		      (_cpointer _string) _pointer))
	  (if str
	      (loop (add1 ndx) (cons str acc))
	      (let ((re (pregexp "(.*)-([[:digit:]]+.[[:digit:]]+)$")))
		(free deps)
		(map (lambda (str)
		       (cdr (regexp-match re str))) acc))))))))

;;; find top level entries
(ffi-wrap "g_irepository_get_n_infos"
	  (_repos-ptr _string
		      -> _gint))

(ffi-wrap "g_irepository_get_info"
	  (_repos-ptr _string _gint
		      -> _baseinfo-ptr))

(define g-irepository-get-info-unref
  (unref-baseinfo g-irepository-get-info))

;;; return a list of top level entries (as _baseinfo-ptr)
(define-enumerator->list
  repository-info-list g-irepository-get-n-infos g-irepository-get-info-unref
  (#f) (#f))

(provide repository-info-list)

;;; return a type for an entry
(ffi-wrap "g_base_info_get_type"
	  (_baseinfo-ptr -> _gint))

(ffi-wrap "g_base_info_get_name"
	  (_baseinfo-ptr -> _string))

(define-upcast (typeinfo _typeinfo-ptr) (baseinfo _baseinfo-ptr))
(define-upcast (arginfo _arginfo-ptr) (baseinfo _baseinfo-ptr))
(define-upcast (gobjinfo _gobjinfo-ptr) (baseinfo _baseinfo-ptr))
(define-upcast (funcinfo _functioninfo-ptr) (baseinfo _baseinfo-ptr))
(define-upcast (structinfo _structinfo-ptr) (baseinfo _baseinfo-ptr))
(define-upcast (enuminfo _enuminfo-ptr) (baseinfo _baseinfo-ptr))
(define-upcast (valueinfo _valueinfo-ptr) (baseinfo _baseinfo-ptr))
(define-upcast (interfaceinfo _interfaceinfo-ptr) (baseinfo _baseinfo-ptr))

;;;; function type

(ffi-wrap "g_function_info_get_symbol"
	  (_functioninfo-ptr -> _string))

(define/provide function-symbol g-function-info-get-symbol)

;;; function flags utils

;;; originally (in girepository) flags are modeled as a bitmask where
;;; all values are exclusive, but it seems that throwable is the only
;;; orthogonal property

(ffi-wrap "g_function_info_get_flags"
	  ( _functioninfo-ptr -> _int))

;(define/provide foo g-function-info-get-flags)

(define/provide (function-throwable? func-info)
  (not (zero? (bitwise-and (g-function-info-get-flags func-info) 32))))

(define-for-syntax (function-flag-predicate-name name)
  (string->symbol
   (string-append "function-is-" (symbol->string name) "?")))

(define-syntax function-flags
  (lambda (stx)
    (syntax-case stx ()
      ((_ (name shft) ...)
       (with-syntax ((function-type (datum->syntax stx 'function-type))
		     ((predicate ...)
		      (map-syntax stx
				  function-flag-predicate-name #'(name ...))))
	 #'(begin
	     (define/provide function-type
	       (lambda (func-info)
		 (let ((flags (g-function-info-get-flags func-info)))
		   (cond
		    ((not (zero? (bitwise-and flags (arithmetic-shift 1 shft))))
		     'name)
		    ...
		    (else 'plain-function)))))
	     (define/provide predicate
	       (lambda (func-info)
		 (let ((type (function-type func-info)))
		   (eq? type 'name)))) ...))))))

(define/provide (function-is-plain? func-info)
  (let ((type (function-type func-info)))
    (eq? type 'plain-function)))

;;; generates function-type function which returnes one of following
;;; symbols including plain-function (as default)
;;; and function-is-<type>? predicates
(function-flags
 (method 0)
 (constructor 1)
 (getter 2)
 (setter 3)
 (wraps-vfunc 4))
; (throws 5) -- removed, implemented as a separate property

;;;; callable type

(define-upcast (function _functioninfo-ptr) (callable _callableinfo-ptr))

(ffi-wrap "g_callable_info_get_return_type"
	  ( _callableinfo-ptr -> _typeinfo-ptr))

(define/provide callable-return-type
  (unref-baseinfo g-callable-info-get-return-type
		  typeinfo->baseinfo))

(ffi-wrap"g_callable_info_get_n_args"
	 (_callableinfo-ptr -> _gint))

(ffi-wrap "g_callable_info_get_arg"
	  (_callableinfo-ptr _gint -> _arginfo-ptr))

(define g-callable-info-get-arg-unref
  (unref-baseinfo g-callable-info-get-arg
		  arginfo->baseinfo))

(define-enumerator->list
  callable-arguments g-callable-info-get-n-args g-callable-info-get-arg-unref
  () ())

(provide callable-arguments)


;;;; typeinfo type

(ffi-wrap "g_type_info_get_tag"
	  ( _typeinfo-ptr -> _int))

(define-syntax type-tags
  (lambda (stx)
    (syntax-case stx ()
      ((_ (name val) ...)
       (with-syntax ((type-tag (datum->syntax stx 'type-tag)))
	 #'(define/provide (type-tag type)
	     (let ((tag (g-type-info-get-tag type)))
	       (case tag
		 ((val) 'name) ...
		 (else 'unknown-type)))))))))

(type-tags
 (void 0)
 (boolean 1)
 (int8 2)
 (uint8 3)
 (int16 4)
 (uint16 5)
 (int32 6)
 (uint32 7)
 (int64 8)
 (uint64 9)
 (float 10)
 (double 11)
 (gtype 12)
 (utf8 13)
 (filename 14)
 (array 15)
 (interface 16)
 (glist 17)
 (gslist 18)
 (ghash 19)
 (error 20)
 (unichar 21))


(ffi-wrap "g_type_tag_to_string" 
	  ( _int -> _string))

(define/provide (type->string type-info)
  (let ((tag (g-type-info-get-tag type-info)))
    (g-type-tag-to-string tag)))

;; huh, modeled after following definition from the doc:
;; #define G_TYPE_TAG_IS_BASIC(tag) (tag < GI_TYPE_TAG_ARRAY || tag == GI_TYPE_TAG_UNICHAR)
(define/provide (type-is-basic? type-info)
  (let ((tag (g-type-info-get-tag type-info)))
    (or (< tag 15) (= tag 21))))

;;todo: add type check
(ffi-wrap "g_type_info_get_interface" 
	  ( _typeinfo-ptr -> _baseinfo-ptr))

(define/provide type-interface
  (unref-baseinfo g-type-info-get-interface))

(ffi-wrap "g_type_info_is_pointer" 
	  ( _typeinfo-ptr -> _gboolean))

(define/provide (type-is-pointer? type-info)
  (gbool->bool (g-type-info-is-pointer type-info)))

;;todo: wrapper -- not sure how to handle arrays
(ffi-wrap "g_type_info_get_array_length" 
	  ( _typeinfo-ptr -> _gint))

(ffi-wrap "g_type_info_get_array_type" 
	  ( _typeinfo-ptr -> _int))

;; g_type_info_get_param_type
;

;;;; arginfo type

(ffi-wrap "g_arg_info_get_direction"
	  (_arginfo-ptr -> _int))

;;; argument direction -- no need for macro since it is the only place
;;; where direction is used
(define/provide argument-direction
  (lambda (arg-info)
    (let ((dir (g-arg-info-get-direction arg-info)))
      (case dir
	((0) 'in-arg)
	((1) 'out-arg)
	((2) 'in/out-arg)))))


;;todo: this macro doesn't work -- not clear why
;(define-syntax define-argument-predicate
;  (lambda (stx)
;    (syntax-case stx ()
;      ((_ sym name)
;       (with-syntax ((lisp-name
;                       (datum->syntax stx
;				     (string->symbol
;				      (lispify-string
;				       (syntax->datum #'name))))))
;	 #'(begin
;	     (ffi-wrap name (_arginfo-ptr -> _gboolean))
;	     (define/provide sym
;	       (lambda (arg-info)
;		 (gbool->bool (lisp-name arg-info))))))))))
;

(ffi-wrap "g_arg_info_is_caller_allocates"
	  (_arginfo-ptr -> _gboolean))

(define/provide (argument-client-allocates? arg-info)
  (gbool->bool (g-arg-info-is-caller-allocates arg-info)))


(ffi-wrap "g_arg_info_is_return_value"
	  (_arginfo-ptr -> _gboolean))

;; todo: not clear what is this predicate for -- doesn't correlate
;; with argument direction
(define/provide (argument-is-return-value? arg-info)
  (gbool->bool (g-arg-info-is-return-value arg-info)))

(ffi-wrap "g_arg_info_is_optional"
	  (_arginfo-ptr -> _gboolean))

(define/provide (argument-is-optional? arg-info)
  (gbool->bool (g-arg-info-is-optional arg-info)))


(ffi-wrap "g_arg_info_may_be_null"
	  (_arginfo-ptr -> _gboolean))

(define/provide (argument-is-nullable? arg-info)
  (gbool->bool (g-arg-info-may-be-null arg-info)))


(ffi-wrap "g_arg_info_get_ownership_transfer"
	  (_arginfo-ptr -> _int))

;;; simillar as argument-direction
(define/provide argument-ownership-transfer
  (lambda (arg-info)
    (let ((own (g-arg-info-get-ownership-transfer arg-info)))
      (case own
	((0) 'transfer-nothing)
	((1) 'transfer-container)
	((2) 'transfer-full)))))


(ffi-wrap "g_arg_info_get_type"
	  (_arginfo-ptr -> _typeinfo-ptr))

(define/provide argument-type
  (unref-baseinfo g-arg-info-get-type typeinfo->baseinfo))


;;;; object type
;;; instead of "object", word "class" will be used in the wrapper
;;; library

(ffi-wrap "g_object_info_get_type_name"
	  (_gobjinfo-ptr -> _string))

(define/provide class-name g-object-info-get-type-name)


(ffi-wrap "g_object_info_get_type_init"
	  (_gobjinfo-ptr -> _string))

(define/provide class-register-function g-object-info-get-type-init)

(ffi-wrap "g_object_info_get_abstract"
	  (_gobjinfo-ptr -> _gboolean))

(define/provide (class-is-abstract? obj-info)
  (gbool->bool (g-object-info-get-abstract obj-info)))

(ffi-wrap "g_object_info_get_parent"
	  (_gobjinfo-ptr -> _gobjinfo-ptr))

(define/provide class-parent
  (unref-baseinfo g-object-info-get-parent
		  gobjinfo->baseinfo))


(ffi-wrap "g_object_info_get_n_methods"
	  (_gobjinfo-ptr -> _gint))

(ffi-wrap "g_object_info_get_method"
	  (_gobjinfo-ptr _gint -> _functioninfo-ptr))

(define g-object-info-get-method-unref
  (unref-baseinfo g-object-info-get-method
		  funcinfo->baseinfo))

(define-enumerator->list class-methods
  g-object-info-get-n-methods g-object-info-get-method-unref () ())

(provide class-methods)

(ffi-wrap "g_object_info_get_n_interfaces"
	  (_gobjinfo-ptr -> _gint))

(ffi-wrap "g_object_info_get_interface"
	  (_gobjinfo-ptr _gint -> _interfaceinfo-ptr))

(define g-object-info-get-interface-unref
  (unref-baseinfo g-object-info-get-interface interfaceinfo->baseinfo))

(define-enumerator->list class-interfaces
  g-object-info-get-n-interfaces g-object-info-get-interface-unref () ())

(provide class-interfaces)

;;;; registered type
;
;(ffi-wrap "g_registered_type_info_get_type_name"
;	  (_registered-ptr -> _string))
;
;(define-upcast (structinfo _structinfo-ptr) (registeredinfo _registered-ptr))

;;;; struct type

(define/provide (struct-name struct-info)
  (g-base-info-get-name (structinfo->baseinfo struct-info)))

(ffi-wrap "g_struct_info_get_size"
	  (_structinfo-ptr -> _gsize))

(define/provide struct-size g-struct-info-get-size)

(ffi-wrap "g_struct_info_is_foreign"
	  (_structinfo-ptr -> _gboolean))

(define/provide (struct-is-foregin? struct-info)
  (gbool->bool (g-struct-info-is-foreign struct-info)))

;;;; enum type

(ffi-wrap "g_enum_info_get_n_values"
	  (_enuminfo-ptr -> _gint))

(ffi-wrap "g_enum_info_get_value"
	  (_enuminfo-ptr _gint -> _valueinfo-ptr))

(define g-enum-info-get-value-unref
  (unref-baseinfo g-enum-info-get-value valueinfo->baseinfo))

(define-enumerator->list
  enum-list g-enum-info-get-n-values g-enum-info-get-value-unref () ())

(ffi-wrap "g_value_info_get_value"
	  (_valueinfo-ptr -> _int64))

(define/provide (enumeration-list enum-info)
  (map (lambda (val-info)
	 (list (g-base-info-get-name (valueinfo->baseinfo val-info))
	       (g-value-info-get-value val-info)))
       (enum-list enum-info)))

(define/provide (enumeration-name enum-info)
  (g-base-info-get-name (enuminfo->baseinfo enum-info)))

;;;; iface type

(define/provide interface-name
  (compose g-base-info-get-name interfaceinfo->baseinfo))

(ffi-wrap "g_interface_info_get_n_prerequisites"
	  (_interfaceinfo-ptr -> _gint))

(ffi-wrap "g_interface_info_get_prerequisite"
	  (_interfaceinfo-ptr _gint -> _baseinfo-ptr))

(define g-interface-info-get-prerequisite-unref
  (compose interface-info (unref-baseinfo g-interface-info-get-prerequisite)))

(define-enumerator->list interface-deps
  g-interface-info-get-n-prerequisites g-interface-info-get-prerequisite-unref
  () ())

(provide interface-deps)

(ffi-wrap "g_interface_info_get_n_methods"
	  (_interfaceinfo-ptr -> _gint))

(ffi-wrap "g_interface_info_get_method"
	  (_interfaceinfo-ptr _gint -> _functioninfo-ptr))

(define g-interface-info-get-method-unref
  (unref-baseinfo g-interface-info-get-method funcinfo->baseinfo))

(define-enumerator->list interface-methodes
  g-interface-info-get-n-methods g-interface-info-get-method-unref () ())

(provide interface-methodes)
