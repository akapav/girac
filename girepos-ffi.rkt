#lang racket

(require ffi/unsafe
	 ffi/unsafe/define
	 (for-syntax "./names.rkt"))

;; todo: support for (define (name args) ...) forms
(define-syntax define/provide
  (syntax-rules ()
    ((define/provide (name args ...) val)
     (define/provide name (lambda (args ...) val)))
    ((define/provide name val)
     (begin
       (provide name)
       (define name val)))))

(define-syntax ffi-wrap
  (lambda (stx)
    (syntax-case stx ()
      ((_ name ftype)
       (with-syntax ((lib
		      (datum->syntax stx '*lib*))
		     (lisp-name
		      (datum->syntax stx
				     (string->symbol
				      (lispify-string
				       (syntax->datum #'name)))))
		     (full-ftype
		      (datum->syntax stx
				     (cons '_fun (syntax->datum #'ftype)))))
	 #'(define lisp-name
	     (get-ffi-obj name lib full-ftype)))))))

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
(define _gint _int)
(define _typelib-ptr (_cpointer/null 'GITypelib))
(define _repos-ptr (_cpointer/null 'GIRepository))
(define _baseinfo-ptr (_cpointer 'GIBaseInfo))
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

;;todo: find proper place -- will be used widely
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


;;;; function type

;;;downcast from _baseinfo-ptr to _functioninfo-ptr
(define/provide (function-info base-info)
  (cast base-info _baseinfo-ptr _functioninfo-ptr))

(ffi-wrap "g_function_info_get_symbol"
	  (_functioninfo-ptr -> _string))

(define/provide function-symbol g-function-info-get-symbol)

;;; function flags utils

;;; originally (in girepository) flags are modeled as a bitmask where
;;; all values are exclusive, and throwable is only orthogonal
;;; property

(ffi-wrap "g_function_info_get_flags"
	  ( _functioninfo-ptr -> _int))

(define/provide (function-throwable? func-info)
  (not (zero? (bitwise-and (g-function-info-get-flags func-info) 32))))

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
	     (define/provide predicate
	       (lambda (func-info)
		 (let ((flags (g-function-info-get-flags func-info)))
		   (not (zero?
			 (bitwise-and flags
				      (arithmetic-shift 1 shft))))))) ...))))))

(function-flags
 (method 0)
 (constructor 1)
 (getter 2)
 (setter 3)
 (wraps-vfunc 4))
; (throws 5) -- removed, implemented as a separate property
