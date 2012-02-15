#lang racket

(require ffi/unsafe
	 ffi/unsafe/define
	 "./gobject.rkt"
	 "./ffi-wrap.rkt")

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
(define _typelib-ptr (_cpointer/null 'GITypelib))
(define _repos-ptr (_cpointer/null 'GIRepository))
(define _baseinfo-ptr (_cpointer/null 'GIBaseInfo))
(define _gobjinfo-ptr (_cpointer/null 'GObjectInfo))
(define _functioninfo-ptr (_cpointer 'GIFunctionInfo))
(define _callableinfo-ptr (_cpointer 'GIClaalbleInfo))
(define _typeinfo-ptr (_cpointer 'GITypeInfo))
(define _arginfo-ptr (_cpointer 'GIArgInfo))
(define _structinfo-ptr (_cpointer 'GIStructInfo))
(define _registered-ptr (_cpointer 'GIREgisteredTypeInfo))
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
 (object _gobjinfo-ptr))

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

;;; at the moment, all functions work with the global repository
;;; so #f (null) is always sent as a repository argument

;;; load a repository (typelib)
(ffi-wrap "g_irepository_require"
	  (_repos-ptr _string _string _int _gerror-ptr-ptr
		      -> _typelib-ptr))

;; todo: clumsy hack with malloc for error pointer -- see how to
;; allocate it on the stack
(define/provide repository-require
  (lambda (name)
    (unless (let ((err (cast
			(malloc 'atomic (ctype-sizeof _gerror-ptr))
			_pointer
			_gerror-ptr-ptr)))
	      (g-irepository-require #f name #f 0 err))
      (displayln "load error"))))

;;; find top level entries
(ffi-wrap "g_irepository_get_n_infos"
	  (_repos-ptr _string
		      -> _gint))

(ffi-wrap "g_irepository_get_info"
	  (_repos-ptr _string _gint
		      -> _baseinfo-ptr))

(define-enumerator->list
  repos-infos g-irepository-get-n-infos g-irepository-get-info (#f) (#f))

;;; return a list of top level entries (as _baseinfo-ptr)
(define/provide repository-info-list repos-infos)

;;; return a type for an entry
(ffi-wrap "g_base_info_get_type"
	  (_baseinfo-ptr -> _gint))

(ffi-wrap "g_base_info_unref"
	  (_baseinfo-ptr -> _void))

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

(define/provide foo g-function-info-get-flags)

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

(define/provide callable-return-type g-callable-info-get-return-type)

(ffi-wrap"g_callable_info_get_n_args"
	 (_callableinfo-ptr -> _gint))

(ffi-wrap "g_callable_info_get_arg"
	  (_callableinfo-ptr _gint -> _arginfo-ptr))

(define-enumerator->list
  callable-args g-callable-info-get-n-args g-callable-info-get-arg () ())

(define/provide callable-arguments callable-args)

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

;;todo: chack what's going on -- always returns "unknown"
(ffi-wrap "g_type_tag_to_string" 
	  ( _typeinfo-ptr -> _string))
(define/provide type->string g-type-tag-to-string)

;; huh, modeled after following definition from the doc:
;; #define G_TYPE_TAG_IS_BASIC(tag) (tag < GI_TYPE_TAG_ARRAY || tag == GI_TYPE_TAG_UNICHAR)
(define/provide (type-is-basic? type-info)
  (let ((tag (g-type-info-get-tag type-info)))
    (or (< tag 15) (= tag 21))))

;;todo: add type check
(ffi-wrap "g_type_info_get_interface" 
	  ( _typeinfo-ptr -> _baseinfo-ptr))
(define/provide type-interface
  (unref-hook g-type-info-get-interface
	      g-base-info-unref))

(ffi-wrap "g_type_info_is_pointer" 
	  ( _typeinfo-ptr -> _gboolean))
(define/provide (type-is-pointer? type-info)
  (gbool->bool (g-type-info-is-pointer type-info)))

;;todo: wrapper -- not sure how to handle arrays
(ffi-wrap "g_type_info_get_array_length" 
	  ( _typeinfo-ptr -> _gint))

(ffi-wrap "g_type_info_get_array_type" 
	  ( _typeinfo-ptr -> _int))
;
