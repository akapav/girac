#lang racket

(require "phase-1-utils.rkt"
         (for-syntax racket/struct-info
                     syntax/parse))

(provide define-initializer
         struct/projective)


;;;; Chainable inits... probably already exists in fome form, can't find yet.

(define initializers (make-hasheq))

(define (parent-of stype)
  (let-values ([(e1 e2 e3 e4 e5 e6 p e8) (struct-type-info stype)]) p))

(define-syntax (define-initializer stx)
  (syntax-case stx ()
    [(_ name struct~ expr ...)
     (with-syntax ([info~ (car (extract-struct-info
                                 (syntax-local-value #'struct~)))])
       #'(begin
           (hash-set! initializers info~ (list expr ...))
           (define name
             (let ([inits (let scan ([who info~])
                            (if (not who) '()
                              (append (scan (parent-of who))
                                      (hash-ref initializers who '()))))])
               (lambda (ob)
                 (apply (struct-type-make-constructor info~)
                        (for/list ([init (in-list inits)]) (init ob))))))))]))

;;;; A structure that gets created by copying something over.

(define-syntax struct/projective
  (syntax-parser
    [(_ name (~optional (~seq parent:id)) #:ctor ctor
        ((field:id init) ...) opt ...)
     (let ([p (if (attribute parent) #'(parent) #'())])
       #`(begin
           (struct name #,@p (field ...) #:inspector inspector opt ...)
           (define-initializer ctor name init ...)))]))

(define inspector (make-inspector))

