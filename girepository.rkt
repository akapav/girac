#lang racket

(require "./gobject.rkt"
	 (prefix-in gir: "./girepos-ffi.rkt"))

;;; misc utils
;;todo: find proper place -- could be used widely

;;; destructive push to list -- works for structs and variables
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


;;;; test
(define (do-gir gir)
  (gir:repository-require gir)
  (for ((fn (filter gir:type-function? (gir:repository-info-list gir))))
       (let* ((fn (gir:baseinfo-downcast fn))
	      (clbl (gir:function->callable fn)))
	 (displayln
	  (list
	   (gir:function-symbol fn)
	   (gir:foo fn)
	   (gir:function-type fn)
	   (gir:function-throwable? fn)
	   (let ((rt (gir:callable-return-type clbl)))
	     (list (gir:type-interface rt)
		   (gir:type-tag rt)
		   (gir:type-is-pointer? rt)))
	   (gir:callable-arguments clbl))))))

(do-gir "Gtk")
