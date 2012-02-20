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
	       (gir-class "GObject" #f #f #t '() '()))
    env))

(define (empty-repository) (gir-repository '() '() '() '() '()))

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

(struct gir-interface
	(name methods deps))

(struct gir-class
	(name parent register-function abstract? methods interfaces)
	#:mutable)

(struct gir-repository
	(classes functions structs enums interfaces)
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

(define (load-interface iface type-env)
  (let* ((name (string-append "iface:" (gir:interface-name iface)))
	 (in-hash (hash-ref type-env name #f)))
    (or in-hash
	(let ((giface
	       (gir-interface name
			      (map (lambda (mtd) (load-function mtd type-env))
				   (gir:interface-methodes iface))
			      (map gir:interface-name
				   (gir:interface-deps iface)))))
	  (hash-set! type-env name giface)
	  giface))))

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
				     (gir:class-methods cls))
				(map (lambda (iface)
				       (gir:interface-name iface))
				     (gir:class-interfaces cls))
				)))
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
	  ((gir:type-interface? info)
	   (push/list! (load-interface info~ type-env)
		       (gir-repository-interfaces repos)))
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

(define (display-interface iface)
  (printf "interface: ~a~%" (gir-interface-name iface))
  (printf "inherits: ~a~%" (gir-interface-deps iface))
  (printf "methods (~a):~%" (length (gir-interface-methods iface)))
  (for ((mtd (gir-interface-methods iface)))
       (display-function mtd))
  (printf "------------------~%"))

(define (display-class cls)
  (printf
   "class: ~a~%parent: ~a~%"
   (gir-class-name cls)
   (gir-class-parent cls))
  (printf "implements: ~a~%" (gir-class-interfaces cls))
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
  (for ((cls (gir-repository-interfaces repos)))
       (display-interface cls))
  (for ((fn (gir-repository-functions repos)))
       (display-function fn))
  (for ((str (gir-repository-structs repos)))
       (display-struct str))
  (for ((enum (gir-repository-enums repos)))
       (display-enum enum)))

;;;
(define (verify-name name type-env)
  (unless (hash-ref type-env name #f)
    (printf "~a is missing~%" name)))

(define (verify-class cls type-env)
  (verify-name (gir-class-name cls) type-env)
  (verify-name (gir-class-parent cls) type-env)
  (for ((mtd (gir-class-methods cls)))
       (verify-function mtd type-env))
  (for ((iface (gir-class-interfaces cls)))
       (verify-name iface type-env)))

(define (verify-interface iface type-env)
  (verify-name (gir-interface-name iface) type-env)
  (for ((mtd (gir-interface-methods iface)))
       (verify-function mtd type-env))
  (for ((iface (gir-interface-deps iface)))
       (verify-name iface type-env)))

(define (verify-function fn type-env)
  (let ((ver (lambda (ty)
	       (let ((tt (gir-type-tag/interface ty)))
		 (case (first tt)
		   ((struct interface class enum)
		    (verify-name (second tt) type-env)))))))
    (ver (gir-function-return-type fn))
    (for ((arg (gir-function-arguments fn)))
	 (ver (gir-argument-type arg)))))

(define (verify-repos repos type-env)
  (for ((cls (gir-repository-classes repos)))
       (verify-class cls type-env))
  (for ((iface (gir-repository-interfaces repos)))
       (verify-interface iface type-env))
  (for ((fn (gir-repository-functions repos)))
       (verify-function fn type-env)))

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
; (display-repos repos)
 (verify-repos repos type-env)
  )

(collect-garbage)
