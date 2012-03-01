#lang racket

(require ffi/unsafe
         "phase-1-utils.rkt"
         (for-syntax syntax/parse))

(provide
  define-opaque-ptr
  define-opaque-ptr-hierarchy
  _ptr-ptr/list/0)


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

  (optional-keyword-class extends #:extends type:id #'_pointer)

  (define-splicing-syntax-class children
    (pattern (~seq #:children #:specialize spec-q:id
                   ((spec-mark:expr child:id) ...))) ;; <- mark: literal?
    (pattern (~seq #:children (child:id ...)) #:attr spec-q #f
             #:with (spec-mark:expr ...) #'())
    (pattern (~seq) #:attr spec-q #f
             #:with (child:id ...) #'()
             #:with (spec-mark:expr ...) #'()))

  (syntax-parse stx
    [(_ name~ parent:extends c:children)

     (with-syntax
       ([(name?~) (decorate-id #'name~ "?")]
        [splice-specializer
          (if (attribute c.spec-q)
            #'(define-syntax-rule (specialize p)
                (case (c.spec-q p)
                  [(c.spec-mark) (become! p c.child)]
                  ...))
            #'(define-syntax-rule (specialize p) (void)))])

       #'(begin
           splice-specializer
           (define ptr-tag (name->type-tag 'name~))

           (define (genesis p)
             (when p (cpointer-push-tag! p ptr-tag) (specialize p))
             p)
           (define name~
             (make-ctype parent.type
                         (lambda (p) (and p (check-pointer p ptr-tag)))
                         genesis))
           (hash-set! becomers name~ genesis)

           (define (name?~ ob)
             (and (cpointer? ob) (cpointer-has-tag? ob ptr-tag) #t)))
       )])
  )

;; Well shave my legs, and tag my inheritance!

(define-syntax (define-opaque-ptr-hierarchy stx)

  (define-splicing-syntax-class to=>
    #:literals (=>)
    (pattern (~seq (~and e (~not =>)) ...)))

  (syntax-parse stx
    [(_ name option:to=> #:specialize proc => (cmark cname ce ...) ...)
     #'(begin
         (define-opaque-ptr name option.e ... 
           #:children #:specialize proc ((cmark cname) ...))
         (define-opaque-ptr-hierarchy cname ce ...)
         ...)]
    [(_ name option:to=> => (cname ce ...) ...)
     #'(begin
         (define-opaque-ptr name option.e ... #:children (cname ...))
         (define-opaque-ptr-hierarchy cname ce ...)
         ...)]
    [(_ name option:to=>)
     #'(define-opaque-ptr name option.e ...)]
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

