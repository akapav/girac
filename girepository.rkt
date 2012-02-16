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

(struct type
	(tag/interface string pointer?))

(struct argument
	(type client-allocates? is-optional? is-nullable? ownership))

(struct function
	(symbol type throwable? return-type arguments))

(struct gclass
	(name parent register-function abstract? methods)
	#:mutable)

(struct repository
	(classes functions)
	#:mutable
	#:transparent)

(define (load-class cls type-env)
  (unless cls (error 'unknown-class))
  (let* ((name (gir:class-name cls))
	 (in-hash (hash-ref type-env name #f)))
    (or in-hash
	(let ((gcls (gclass name
			    (load-class (gir:class-parent cls) type-env)
			    (gir:class-register-function cls)
			    (gir:class-is-abstract? cls)
			    (map (lambda (mtd) (load-function mtd type-env))
				 (gir:class-methods cls)))))
	  (hash-set! type-env name gcls)
	  gcls))))

(define (load-function fn type-env)
  (function (gir:function-symbol fn)
	    (gir:function-type fn)
	    (gir:function-throwable? fn)#f #f))

(define (load-gir gir type-env)
  (gir:repository-require gir)
  (let ((repos (repository '() '())))
    (for ((info (gir:repository-info-list gir)))
	 (let ((info~ (gir:baseinfo-downcast info)))
	   (cond
	    ((gir:type-object? info)
	     (push/list! (load-class info~ type-env)
			 (repository-classes repos)))
	    ((gir:type-function? info)
	     (push/list! (load-function info~ type-env)
			 (repository-functions repos))))))
    repos))

;; todo: tmp
(define (display-function fn)
  (printf "~a~%" (function-symbol fn))
  (printf "type: ~a throwable: ~a~%~%"
	  (function-type fn)
	  (if (function-throwable? fn) "yes" "no")))

(define (display-class cls)
  (printf
   "class: ~a~%parent: ~a~%"
   (gclass-name cls)
   (gclass-name (gclass-parent cls)))
  (printf
   "type register function: ~a~%abstract: ~a~%"
   (gclass-register-function cls)
   (if (gclass-abstract? cls) "yes" "no"))
  (printf "methods (~a):~%" (length (gclass-methods cls)))
  (for ((mtd (gclass-methods cls)))
       (display-function mtd))
  (printf "------------------~%"))

(define (display-repos repos)
  (for ((cls (repository-classes repos)))
       (display-class cls))
  (for ((fn (repository-functions repos)))
       (display-function fn)))


(let ((type-env (make-hash)))
  (hash-set! type-env "GObject"
	     (gclass "GObject" #f #f #f '()))
  (display-repos (load-gir "Gtk" type-env)))

;;;; todo: to be removed!!!!
(define (dump-fn fn)
  (let* ((fn (gir:baseinfo-downcast fn))
	 (clbl (gir:function->callable fn)))
    (displayln
     (list
      (gir:function-symbol fn)
      (gir:foo fn)
      (gir:function-type fn)
      (gir:function-throwable? fn)
      (let ((rt (gir:callable-return-type clbl)))
	(list (let ((iface (gir:type-interface rt)))
		(if iface (gir:get-type iface) '_))
	      (gir:type-tag rt)
	      (gir:type->string rt)
	      (gir:type-is-pointer? rt)))
      (map (lambda (arg)
	     (list
	      (gir:argument-direction arg)
	      (if (gir:argument-client-allocates? arg)
		  'client-allocates
		  'server-allocates)
	      (if (gir:argument-is-optional? arg)
		  'optional
		  'mandatory)
	      (if (gir:argument-is-nullable? arg)
		  'nullable
		  'not-nullable)
	      (if (gir:argument-is-return-value? arg)
		  'return-value
		  'isnt-return-value)
	      (gir:argument-ownership-transfer arg)))
	   (gir:callable-arguments clbl))))))

(define (dump-obj obj)
  (let ((obj (gir:baseinfo-downcast obj)))
    (displayln
     (list
      (gir:class-name obj)
      (gir:class-register-function obj)
      (gir:class-is-abstract? obj)
      (gir:class-name (gir:class-parent obj))
      (map gir:function-symbol (gir:class-methods obj))))))

(define (do-gir gir)
  (gir:repository-require gir)
  (for((obj (filter gir:type-object? (gir:repository-info-list gir))))
      (dump-obj obj)))

;(do-gir "Gtk")
(collect-garbage)
