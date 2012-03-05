#lang racket

(require "phase-1-utils.rkt"
         (for-syntax syntax/parse))

(provide define-initializer
         struct/projective)


;;;; Chainable inits... probably already exists in fome form, can't find yet.

(define initializers (make-hasheq))

(define (parent-of stype)
  (let-values ([(e1 e2 e3 e4 e5 e6 p e8) (struct-type-info stype)]) p))

(define-syntax-rule (define-initializer name struct~ expr ...)
  (begin
    (hash-set! initializers struct~ (list expr ...))
    (define name
      (let ([inits (let scan [(who struct~)]
                     (if (not who) '()
                       (append (scan (parent-of who))
                               (hash-ref initializers who '()))))])
        (lambda (ob)
          (apply (struct-type-make-constructor struct~)
                 (for/list ([init (in-list inits)]) (init ob))))))))


;;;; A structure that gets created by copying something over.

(define-syntax struct/projective
  (syntax-parser
    [(_ name (~optional (~seq parent:id)) #:ctor ctor
        ((field:id init) ...) opt ...)
     (let ([p (if (attribute parent) #'(parent) #'())])
       #`(begin
           (struct name #,@p (field ...) #:inspector inspector opt ...)
           ;; XXX FIXME TODO use phase-1 structure reflection to divine the
           ;; structure-type-descriptor, not name-guessing!!!
           (define-initializer
             ctor #,@(decorate-id #'name '("struct:" "")) init ...)))]))

(define inspector (make-inspector))

