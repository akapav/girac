#lang racket

(require ffi/unsafe
	 ffi/unsafe/define
	 "./girepos-ffi.rkt")

;;; initialize gtype
;;; must be invoked before any other action with gtype system

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

;;; misc utils

;; destructive push to list -- works for structs and variables
(define-for-syntax (struct/setter-name stx)
  (string->symbol
   (string-append "set-" (symbol->string (syntax->datum stx)) "!")))

(define-syntax push/list!
  (lambda (stx)
    (syntax-case stx ()
      ((_ val (acc obj))
       (with-syntax
	   ((setter (datum->syntax stx (struct/setter-name #'acc))))
	 #'(setter obj (cons val (acc obj)))))
      ((_ val list) #'(set! list (cons val list))))))

;;; 
(struct repository
	(classes funs)
	#:mutable)
