#lang racket

(require ffi/unsafe
         "phase-1-utils.rkt"
         (for-syntax syntax/parse))
(require (for-syntax syntax/parse/debug))

(provide
  define-opaque-ptr
  define-opaque-ptr-hierarchy
  _ptr-ptr/list/0
  _bitmask/bits
  seq-labels)


;;;; Our own version of tagged pointers. Goodies pending.

(define (name->type-tag name)
  (gensym (string-append (symbol->string name) "__")))

(define-syntax-rule (check-pointer ptr t)
  (begin
    (unless (and (cpointer? ptr) (cpointer-has-tag? ptr t))
      (raise-type-error 'scheme->C (symbol->string t) ptr))
    ptr))

(define ((raise-non-reflectable-type who type))
  (raise-type-error who "opaque-ptr type" type))

(define becomers (make-hasheq))

(define (become! ptr type)
  ((hash-ref becomers type
             (raise-non-reflectable-type 'become! type))
   ptr))


(define-syntax (define-opaque-ptr stx)

  ;; This got a little bit out of hand, dive into dox and shorten it.
  (define-splicing-syntax-class children
;     #:attributes (spec-q (spec-a 1) (child 1))
    (pattern (~seq #:children
                   #:specialize spec-q:id
;                    ((spec-a:atom* child:id) ...)))
                   ((sa:atom* child:id) ...))
             #:with ((spec-a ...) ...) #'((sa.e ...) ...))
    (pattern (~seq #:children (child:id ...))
             #:attr spec-q #f
;              #:with (spec-a:atom* ...) #'())
             #:with ((spec-a ...) ...) #'())
    (pattern (~seq) #:attr spec-q #f
             #:with (child ...) #'()
;              #:with (spec-a:atom* ...) #'())
             #:with ((spec-a ...) ...) #'())
    )

  (syntax-parse stx
    [(_ name~
        (~seq (~or (~optional (~seq #:extends parent:id)
                              #:defaults ([parent #'_pointer]))
                   (~optional (~seq #:on-ingress on-in:fun-fragment))
                   (~optional (~seq #:on-outgress on-out:fun-fragment)))
              ...)
        ch:children)

     (with-syntax ([(name?~) (decorate-id #'name~ "?")])
       #`(begin

           (define ptr-tag (name->type-tag 'name~))

           (define (brahma p)
             (when (and p (not (cpointer-has-tag? p ptr-tag)))
               (cpointer-push-tag! p ptr-tag)
               #,(when (attribute on-in)
                   #'(let ([on-in.var p]) on-in.exp ...))
               #,(when (attribute ch.spec-q)
                   #'(case (ch.spec-q p)
                       [(ch.spec-a ...) (become! p ch.child)] ...)))
             p)

           (define (shiva p)
             (and p (check-pointer p ptr-tag))
             #,(when (attribute on-out)
                 #'(let ([on-out.var p]) on-out.exp ...))
             p)

           (define name~ (make-ctype parent shiva brahma))

           (hash-set! becomers name~ brahma)

           (define (name?~ ob)
             (and (cpointer? ob) (cpointer-has-tag? ob ptr-tag) #t)))
       )])
  )

;; Well shave my legs, and tag my inheritance!

(define-syntax (define-opaque-ptr-hierarchy stx)

  (define-splicing-syntax-class to=>
    #:literals (=>)
    (pattern (~seq (~and e (~not =>)) ... =>)))

  (syntax-parse stx
    [(_ name option:to=> #:specialize proc (cmark cname ce ...) ...)
     #'(begin
         (define-opaque-ptr name option.e ... 
           #:children #:specialize proc ((cmark cname) ...))
         (define-opaque-ptr-hierarchy cname #:extends name ce ...)
         ...)]
    [(_ name option:to=> (cname ce ...) ...)
     #'(begin
         (define-opaque-ptr name option.e ... #:children (cname ...))
         (define-opaque-ptr-hierarchy cname #:extends name ce ...)
         ...)]
    [(_ name option ...)
     #'(define-opaque-ptr name option ...)]
    ))


;;;; Some aux `prim' c types.

(define (_ptr-ptr/list/0 type)
  (make-ctype _pointer
    (lambda _ (error '_ptr-ptr/list/0->C "lol this is inparam for now"))
    (lambda (p)
      (and p (let loop ([ndx 0] [acc '()])
               (let ([elt (ptr-ref p type ndx)])
                 (if elt
                   (begin
                     (free (ptr-ref p _pointer ndx))
                     (loop (add1 ndx) (cons elt acc)))
                   (begin (free p) (reverse acc)))))))))


;; Like Racket's `_bitmask', but with bit positions.
(define (_bitmask/bits bmsk)
  (_bitmask (for/list ([x bmsk])
              (if (integer? x) (arithmetic-shift 1 x) x))))


;; (seq-labels a b c) => '(a = 0 b = 1 c = 2)
(define-syntax seq-labels 
  (syntax-rules ()
    [(_ #:from b sym ...)
     (append* (for/list ([id '(sym ...)] [n (in-naturals b)])
                (list id '= n)))]
    [(_ sym ...) (seq-labels #:from 0 sym ...)]))

