#lang racket

(require ffi/unsafe
	 (for-syntax "./names.rkt"))

(provide ffi-wrap)

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
