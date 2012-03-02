#lang racket

(require "girepository.rkt")

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

(define (display-callback cbck)
  (printf "callback: ~a :: ( " (gir-callback-name cbck))
  (for ((arg (gir-callback-arguments cbck)))
       (display-arg arg) (printf " "))
  (printf ") -> ")
  (display-type (gir-callback-return-type cbck))
  (printf "~%"))

(define (display-enum enum)
  (printf "enum: ~a values: ~a~%~%"
	  (gir-enum-name enum)
	  (gir-enum-values enum)))

(define (display-struct str)
  (printf "struct: ~a size: ~a foregin: ~a~%"
	  (gir-struct-name str)
	  (gir-struct-size str)
	  (if (gir-struct-is-foregin? str) "yes" "no"))
  (printf "methods (~a):~%" (length (gir-struct-methods str)))
  (for ((mtd (gir-struct-methods str)))
       (display-function mtd))
  (printf "------------------~%"))

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
  (for ((fn (gir-repository-callbacks repos)))
       (display-callback fn))
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

(define (verify-struct str type-env)
  (verify-name (gir-struct-name str) type-env)
  (for ((mtd (gir-struct-methods str)))
       (verify-function mtd type-env)))

(define (verify-function fn type-env)
  (let ((ver (lambda (ty)
	       (let ((tt (gir-type-tag/interface ty)))
		 (when (pair? tt)
		   (case (car tt)
		     ((struct interface class enum callback)
		      (verify-name tt type-env))))))))
    (ver (gir-function-return-type fn))
    (for ((arg (gir-function-arguments fn)))
	 (ver (gir-argument-type arg)))))

(define (verify-callback cbck type-env)
  (let ((ver (lambda (ty)
	       (let ((tt (gir-type-tag/interface ty)))
		 (when (pair? tt)
		   (case (car tt)
		     ((struct interface class enum callback)
		      (verify-name tt type-env))))))))
    (ver (gir-callback-return-type cbck))
    (for ((arg (gir-callback-arguments cbck)))
	 (ver (gir-argument-type arg)))))

(define (verify-repos repos type-env)
  (for ((cls (gir-repository-classes repos)))
       (verify-class cls type-env))
  (for ((iface (gir-repository-interfaces repos)))
       (verify-interface iface type-env))
  (for ((str (gir-repository-structs repos)))
       (verify-struct str type-env))
  (for ((enum (gir-repository-enums repos)))
       (verify-name (gir-enum-name enum) type-env))
  (for ((fn (gir-repository-functions repos)))
       (verify-function fn type-env))
  (for ((fn (gir-repository-callbacks repos)))
       (verify-callback fn type-env)))

;;;
(define (main name #:version (version #f) #:path (path #f))
  (when path (gir:repository-prepend-path path))
  (let-values (((repos type-env) (load-gir name version)))
    ;(displayln type-env)
    (display-repos repos)
    (verify-repos repos type-env)))

(main "Gtk")
;(main "TestClass" #:version "0.0" #:path "/home/aka/devel/girac")