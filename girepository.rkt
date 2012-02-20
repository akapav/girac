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
(define (basic-env)
  (let ((env (make-hash)))
    (hash-set! env "-GObject-"
	       (gir-class "GObject" #f #f #t '()))
    env))

(define (empty-repository) (gir-repository '() '() '() '()))

;;; 

(struct gir-type
	(tag/interface string pointer?)
	#:mutable)

(struct gir-argument
	(type client-allocates? is-optional? is-nullable? ownership))

(struct gir-function
	(symbol type throwable? return-type arguments))

(struct gir-enum
	(name values))

(struct gir-struct
	(name size is-foregin?))

(struct gir-class
	(name parent register-function abstract? methods)
	#:mutable)

(struct gir-repository
	(classes functions structs enums)
	#:mutable
	#:transparent)

(define (load-type-interface tag type-iface type-env)
  (let ((type-iface~ (gir:baseinfo-downcast type-iface)))
    (cond
     ((gir:type-object? type-iface)
      (list 'class (gir:class-name type-iface~)))
     ((gir:type-struct? type-iface)
      (list 'struct (gir:struct-name type-iface~)))
     ((or (gir:type-enum? type-iface) (gir:type-flags? type-iface))
      (list 'enum (gir:enumeration-name type-iface~)))
     (else (list 'not-supported-yet (gir:get-type type-iface))))))

(define (load-type type type-env)
  (let ((tag (gir:type-tag type))
	(iface (gir:type-interface type)))
    (gir-type (if iface
		  (load-type-interface tag iface type-env)
		  (list tag tag))
	      (gir:type->string type)
	      (gir:type-is-pointer? type))))

(define (load-argument arg type-env)
  (gir-argument (load-type (gir:argument-type arg) type-env)
		(gir:argument-client-allocates? arg)
		(gir:argument-is-optional? arg)
		(gir:argument-is-nullable? arg)
		(gir:argument-ownership-transfer arg)))

(define (load-enum enum type-env)
  (let* ((name (string-append "enum:"
			      (gir:enumeration-name enum)))
	 (in-hash (hash-ref type-env name #f)))
    (or in-hash
	(let ((genum (gir-enum name
			       (gir:enumeration-list enum))))
	  (hash-set! type-env name genum)
	  genum))))

(define (load-struct str type-env)
  (let* ((name (gir:struct-name str))
	 (in-hash (hash-ref type-env name #f)))
    (or in-hash
	(let ((gstr (gir-struct name
				(gir:struct-size str)
				(gir:struct-is-foregin? str))))
	  (hash-set! type-env name gstr)
	  gstr))))

(define (load-class cls type-env)
  (unless cls (error 'unknown-class))
  (let* ((name (gir:class-name cls))
	 (in-hash (hash-ref type-env name #f)))
    (or in-hash
	(let* ((gcls (gir-class name
				(if (string=? name "GObject") "-GObject-"
				    (gir:class-name (gir:class-parent cls)))
				(gir:class-register-function cls)
				(gir:class-is-abstract? cls)
				(map (lambda (mtd) (load-function mtd type-env))
				     (gir:class-methods cls)))))
	  (hash-set! type-env name gcls)
	  gcls))))

(define (load-function fn type-env)
  (let ((clbl (gir:function->callable fn)))
    (gir-function (gir:function-symbol fn)
		  (gir:function-type fn)
		  (gir:function-throwable? fn)
		  (load-type (gir:callable-return-type clbl) type-env)
		  (map (lambda (arg)
			 (load-argument arg type-env))
		       (gir:callable-arguments clbl)))))

(define (load-gir gir type-env repos (version #f))
  (gir:repository-require gir version)
  (for ((info (gir:repository-info-list gir)))
       (let ((info~ (gir:baseinfo-downcast info)))
	 (cond
	  ((gir:type-object? info)
	   (push/list! (load-class info~ type-env)
		       (gir-repository-classes repos)))
	  ((gir:type-function? info)
	   (push/list! (load-function info~ type-env)
		       (gir-repository-functions repos)))
	  ((gir:type-struct? info)
	   (push/list! (load-struct info~ type-env)
		       (gir-repository-structs repos)))
	  ((or (gir:type-enum? info) (gir:type-flags? info))
	   (push/list! (load-enum info~ type-env)
		       (gir-repository-enums repos)))
		   
)))
  (values repos type-env))

(define (load-girs girs)
  (for/fold ((repos (empty-repository)) (type-env (basic-env)))
      ((gir girs))
    (load-gir (first gir) type-env repos (second gir))))

;; todo: tmp
(define (display-type type)
  (printf "~a" (gir-type-tag/interface type))
  (when (gir-type-pointer? type)
    (printf "*")))

(define (display-arg arg)
  (display-type (gir-argument-type arg)))

(define (display-function fn)
  (printf "~a :: ( " (gir-function-symbol fn))
  (for ((arg (gir-function-arguments fn)))
       (display-arg arg) (printf " "))
  (printf ") -> ")
  (display-type (gir-function-return-type fn))
  (printf "~%")
  (printf "type: ~a throwable: ~a~%~%"
	  (gir-function-type fn)
	  (if (gir-function-throwable? fn) "yes" "no")))

(define (display-enum enum)
  (printf "enum: ~a values: ~a~%~%"
	  (gir-enum-name enum)
	  (gir-enum-values enum)))

(define (display-struct str)
  (printf "struct: ~a size: ~a foregin: ~a~%~%"
	  (gir-struct-name str)
	  (gir-struct-size str)
	  (if (gir-struct-is-foregin? str) "yes" "no")))

(define (display-class cls)
  (printf
   "class: ~a~%parent: ~a~%"
   (gir-class-name cls)
   (gir-class-parent cls))
  (printf
   "type register function: ~a~%abstract: ~a~%"
   (gir-class-register-function cls)
   (if (gir-class-abstract? cls) "yes" "no"))
  (printf "methods (~a):~%" (length (gir-class-methods cls)))
  (for ((mtd (gir-class-methods cls)))
       (display-function mtd))
  (printf "------------------~%"))

(define (display-repos repos)
  (for ((cls (gir-repository-classes repos)))
       (display-class cls))
  (for ((fn (gir-repository-functions repos)))
       (display-function fn))
  (for ((str (gir-repository-structs repos)))
       (display-struct str))
  (for ((enum (gir-repository-enums repos)))
       (display-enum enum)))

(define (verify-types repos type-env)
  (for ((cls (gir-repository-classes repos)))
       (begin
	 (let ((parent-name (gir-class-parent cls)))
	   (unless (hash-ref type-env parent-name #f)
	     (printf "~a is missing~%" parent-name)))
	 (let ((mtds (gir-class-methods cls)))
	   (for ((mtd mtds))
		(let ((rt (gir-type-tag/interface
			   (gir-function-return-type mtd))))
		  (when (eq? (first rt) 'interface)
		    (unless (hash-ref type-env (second rt) #f)
		      (printf "~a is missing~%" (second rt))))))))))

(let-values (((repos type-env)
	      (load-girs '(
			   ("Gtk" #f)
			   ("Atk" "1.0")
			   ("GLib" "2.0")
			   ("GModule" "2.0")
			   ("GObject" "2.0")
			   ("Gdk" "3.0") 
			   ("GdkPixbuf" "2.0")
			   ("Gio" "2.0")
			   ("Pango" "1.0")
			   ("cairo" "1.0")
			   ("xlib" "2.0")
			   ))))
 (display-repos repos)
 (verify-types repos type-env)
  )

(collect-garbage)
