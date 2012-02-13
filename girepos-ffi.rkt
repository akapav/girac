#lang racket

(require ffi/unsafe
	 ffi/unsafe/define
	 (for-syntax "./names.rkt"))

;;; todo: support for (define (name args) ...) forms
(define-syntax-rule (define/provide name val)
  (begin
    (provide name)
    (define name val)))

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

(define libtl (ffi-lib "/usr/lib/libgirepository-1.0.so"))

;; todo: use *lib* as a parameter -- doesn't work out of the box since
;; it is forbidden to use 'provide from 'parameterize block
(define *lib* libtl)

;;; ffi typedefs
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

;;; repository functions
(ffi-wrap "g_irepository_require"
	  (_repos-ptr
	   _string _string _int _gerror-ptr-ptr ->
	   _typelib-ptr))

;; todo: clumsy hack with malloc for error pointer -- see how to
;; allocate it on the stack
(define/provide gir-require
  (lambda (name)
    (unless (let ((err (cast
			(malloc 'atomic (ctype-sizeof _gerror-ptr))
			_pointer
			_gerror-ptr-ptr)))
	      (g-irepository-require #f name #f 0 err))
      (displayln "load error"))))
