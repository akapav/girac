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

(define pointer-tags (make-hasheq))

(define ((raise-r-t-error who type))
  (raise-type-error who "reflectable cpointer type" type))

(define (cpointer-type-tags type)
  (hash-ref pointer-tags type (raise-r-t-error 'cpointer-type-tags type)))

(define (cpointer-reflectable-type? type)
  (hash-has-key? pointer-tags type))

(define-syntax-rule (cpointer-tag-as! ptr type)
  (cpointer-push-tag! ptr
    (hash-ref pointer-tags type (raise-r-t-error 'cpointer-tag-as! type))))

(define-syntax-rule (pointer-check-tag ptr t)
  (begin
    (unless (and (cpointer? ptr) (cpointer-has-tag? ptr t))
      (raise-type-error 'scheme->C (symbol->string t) ptr))
    ptr))


(define-syntax (define-opaque-ptr stx)

  (optional-keyword-class extends #:extends type:id #'_pointer)

  (define-splicing-syntax-class children
    (pattern (~seq #:children #:downcast dcast-sel:id
                   ((dcast-mark:expr child:id) ...))) ;; <- mark: literal?
    (pattern (~seq #:children (child:id ...)) #:attr dcast-sel #f
             #:with (dcast-mark:expr ...) #'())
    (pattern (~seq) #:attr dcast-sel #f
             #:with (child:id ...) #'()
             #:with (dcast-mark:expr ...) #'()))

  (syntax-parse stx
    [(_ name~ parent:extends c:children)

     (with-syntax
       ([(name?~) (decorate-id #'name~ "?")]
        [define-downtag (if (attribute c.dcast-sel)
                          #'(define-syntax-rule (downtag p)
                              (case (c.dcast-sel p)
                                [(c.dcast-mark) (cpointer-tag-as! p c.child)]
                                ...))
                          #'(define-syntax-rule (downtag p) (void)))])

       #'(begin
           define-downtag
           (define ptr-tag (name->type-tag 'name~))
           (define name~
             (make-ctype parent.type
                         (lambda (p) (and p (pointer-check-tag p ptr-tag)))
                         (lambda (p)
                           (when p (cpointer-push-tag! p ptr-tag) (downtag p))
                           p)))
           (define (name?~ ob)
             (and (cpointer? ob) (cpointer-has-tag? ob ptr-tag) #t))
           (hash-set! pointer-tags name~ ptr-tag))
       )])
  )

;; Well shave my legs, and tag my inheritance!

(define-syntax (define-opaque-ptr-hierarchy stx)

  (define-splicing-syntax-class to=>
    #:literals (=>)
    (pattern (~seq (~and e (~not =>)) ...)))

  (syntax-parse stx
    [(_ name option:to=> #:downcast proc => (cmark cname ce ...) ...)
     #'(begin
         (define-opaque-ptr name option.e ... 
           #:children #:downcast proc ((cmark cname) ...))
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

