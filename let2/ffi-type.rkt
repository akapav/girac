#lang racket

(require ffi/unsafe)

(provide
  cpointer-type-tags
  define-opaque-ptr
  define-opaque-ptr-hierarchy
  _ptr-ptr/list/0)


(define (name->type-tag name)
  (gensym (string-append (symbol->string name) "__")))

;; Our own version of tagged pointers. Goodies pending.

;; XXX for now, nulls are ok. TODO restrict.
(define-syntax define-opaque-ptr 
  (syntax-rules ()
    [(_ name #:extends parent)
     (define name
       (let ([tag (name->type-tag 'name)])
         (_cpointer/null tag parent)))]
    [(_ name)
     (define-opaque-ptr name #:extends _pointer)]))

;; Well shave my legs, and tag my inheritance!

(define-syntax define-opaque-ptr-hierarchy
  (syntax-rules (->)
    [(_ (name #:extends parent -> (child e ...) ...))
     (begin
       (define-opaque-ptr name #:extends parent)
       (define-opaque-ptr-hierarchy
         (child #:extends name e ...))
       ...
       )]
    [(_ (name -> rest ...))
     (define-opaque-ptr-hierarchy
       (name #:extends _pointer -> rest ...))]
    [(_ (name #:extends parent))
     (define-opaque-ptr name #:extends parent)]
    ))

;; Yes, I'm going to burn in hell, reducing Omega for eternity. I know.

(define cpointer-type-tags
  (let ([ragdoll (function-ptr identity (_fun -> _void))])
    (lambda (type)
      (cpointer-tag (cast ragdoll _pointer type)))))


;; Some aux `prim' c types.

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

