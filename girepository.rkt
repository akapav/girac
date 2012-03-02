#lang racket

(provide (struct-out gir-type)
	 (struct-out gir-argument)
	 (struct-out gir-function)
	 (struct-out gir-callback)
	 (struct-out gir-enum)
	 (struct-out gir-struct)
	 (struct-out gir-interface)
	 (struct-out gir-class)
	 (struct-out gir-repository)
	 load-gir
	 gir:repository-prepend-path)

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

;;todo: duplicated code!!!
(define-for-syntax (map-syntax env f stx)
  (datum->syntax env (map f (syntax->datum stx))))

;;;
(define (basic-env)
  (let ((env (make-hash)))
    (hash-set! env (cons 'class "-NO-CLASS-")
	       (gir-class (cons 'class "GObject") #f #f #t '() '()))
    env))

(define (empty-repository) (gir-repository '() '() '() '() '() '()))

;;; 

(struct gir-type
	(tag/interface string pointer?)
	#:mutable)

(struct gir-argument
	(type client-allocates? is-optional? is-nullable? ownership))

(struct gir-function
	(symbol type throwable? return-type arguments))

(struct gir-callback
	(name return-type arguments))

(struct gir-enum
	(name values))

(struct gir-struct
	(name methods size is-foregin?))

(struct gir-interface
	(name methods deps))

(struct gir-class
	(name parent register-function abstract? methods interfaces)
	#:mutable)

(struct gir-repository
	(classes functions callbacks structs enums interfaces)
	#:mutable
	#:transparent)

(define-for-syntax (mk-key-name name)
  (string->symbol
   (string-append (symbol->string name) "-key")))

(define-syntax hash-key
  (lambda (stx)
    (syntax-case stx ()
      ((_ key ...)
       (with-syntax (((key-name ...)
		      (map-syntax stx mk-key-name #'(key ...))))
	 #'(begin
	     (define (key-name val)
	       (cons 'key val)) ... ))))))

(hash-key
 class
 interface
 struct
 enum
 callback)

(define-syntax-rule (with-hashed-type (key exp) hash val-exp)
  (let* ((key exp)
	 (in-hash (hash-ref hash key #f)))
    (or in-hash
	(let ((val val-exp))
	  (hash-set! hash key val)
	  val))))

(define (load-type-interface tag type-iface type-env)
  (let ((type-iface~ (gir:baseinfo-downcast type-iface)))
    (cond
     ((gir:type-object? type-iface)
      (class-key (gir:class-name type-iface~)))
     ((gir:type-interface? type-iface)
      (interface-key (gir:interface-name type-iface~)))
     ((gir:type-struct? type-iface)
      (struct-key (gir:struct-name type-iface~)))
     ((gir:type-callback? type-iface)
      (callback-key (gir:callable-name type-iface~)))
     ((or (gir:type-enum? type-iface) (gir:type-flags? type-iface))
      (enum-key (gir:enumeration-name type-iface~)))
     (else (list 'not-supported-yet (gir:get-type type-iface))))))

(define (load-type type type-env)
  (let ((tag (gir:type-tag type))
	(iface (gir:type-interface type)))
    (gir-type (if iface
		  (load-type-interface tag iface type-env)
		  tag )
	      (gir:type->string type)
	      (gir:type-is-pointer? type))))

(define (load-argument arg type-env)
  (gir-argument (load-type (gir:argument-type arg) type-env)
		(gir:argument-client-allocates? arg)
		(gir:argument-is-optional? arg)
		(gir:argument-is-nullable? arg)
		(gir:argument-ownership-transfer arg)))

(define (load-enum enum type-env)
  (with-hashed-type
   (name (enum-key (gir:enumeration-name enum))) type-env
   (gir-enum name (gir:enumeration-list enum))))

(define (load-struct str type-env)
  (with-hashed-type
   (name (struct-key (gir:struct-name str))) type-env
   (gir-struct name
	       (map (lambda (mtd) (load-function mtd type-env))
		    (gir:struct-methods str))
	       (gir:struct-size str)
	       (gir:struct-is-foregin? str))))

(define (load-interface iface type-env)
  (with-hashed-type
   (name (interface-key (gir:interface-name iface))) type-env
   (gir-interface name
		  (map (lambda (mtd) (load-function mtd type-env))
		       (gir:interface-methodes iface))
		  (map (lambda (obj)
			 (let ((obj~ (gir:baseinfo-downcast obj)))
			   (cond ((gir:type-interface? obj)
				  (interface-key (gir:interface-name obj~)))
				 ((gir:type-object? obj)
				  (class-key (gir:class-name obj~)))
				 (else (error "unsupported interface dep")))))
		       (gir:interface-deps iface)))))

(define (toplevel-class? name)
  (ormap (lambda (tlc) (string=? name tlc))
	 '("GObject" "GParam")))

(define (load-class cls type-env)
  (with-hashed-type
   (name (class-key (gir:class-name cls))) type-env
   (gir-class name
	      (class-key
	       (if (toplevel-class? (cdr name))  "-NO-CLASS-"
		   (gir:class-name (gir:class-parent cls))))
	      (gir:class-register-function cls)
	      (gir:class-is-abstract? cls)
	      (map (lambda (mtd) (load-function mtd type-env))
		   (gir:class-methods cls))
	      (map (lambda (iface)
		     (interface-key (gir:interface-name iface)))
		   (gir:class-interfaces cls)))))

(define (load-function fn type-env)
  (let ((clbl (gir:function->callable fn)))
    (gir-function (gir:function-symbol fn)
		  (gir:function-type fn)
		  (gir:function-throwable? fn)
		  (load-type (gir:callable-return-type clbl) type-env)
		  (map (lambda (arg)
			 (load-argument arg type-env))
		       (gir:callable-arguments clbl)))))

(define (load-callback clbl type-env)
  (with-hashed-type
   (name (callback-key (gir:callable-name clbl))) type-env
   (gir-callback name
		 (load-type (gir:callable-return-type clbl) type-env)
		 (map (lambda (arg)
			(load-argument arg type-env))
		      (gir:callable-arguments clbl)))))

(define *loaded-girs* (set "GLib" "GObject"))

(define (load-1gir gir type-env repos (version #f))
  (unless (set-member? *loaded-girs* gir)
    (set! *loaded-girs* (set-add *loaded-girs* gir))
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
	     ;;todo: add to function a canonic name (not only a symbol)
	     ;; (displayln (gir:base-name info))
	     (push/list! (load-function info~ type-env)
			 (gir-repository-functions repos)))
	    ((gir:type-callback? info)
	     (push/list! (load-callback info~ type-env)
			 (gir-repository-callbacks repos)))
	    ((gir:type-struct? info)
	     (push/list! (load-struct info~ type-env)
			 (gir-repository-structs repos)))
	    ((or (gir:type-enum? info) (gir:type-flags? info))
	     (push/list! (load-enum info~ type-env)
			 (gir-repository-enums repos)))))))
  (values repos type-env))

(define (load-gir gir (version #f) (repos (empty-repository)) (type-env (basic-env)))
  (gir:repository-require gir version)
  (let ((deps (gir:repository-dependencies gir)))
    (let-values (((repos2 type-env2)
		  (for/fold ((repos repos) (type-env type-env))
		      ((gir deps))
		    (load-gir (first gir) (second gir) repos type-env))))
      (load-1gir gir type-env2 repos2 version))))
      
