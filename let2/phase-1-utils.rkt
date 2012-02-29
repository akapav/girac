#lang racket

(require ffi/unsafe/define
         (for-meta 1 "names.rkt"))

(provide (for-syntax map-syntax-keyword
                     map-syntax-keyword/str
                     string-fun->symbol-fun
                     lispify-string~
                     lispify-string
                     )
         define-naming-definer-for
         define-ffi-definers
         )

;; Here's the deal: either this goes meta + 1, or macros go meta - 1. But in the
;; latter case, they need to be directly `define'-d, which is less pleasant.

(begin-for-syntax

  ;; Process a syntax identifier as if it were a symbol; replace the result back
  ;; with the same lexical information.
  (define (map-syntax-keyword kw fun)
    (define unwrapped (syntax->datum kw))
    (if (symbol? unwrapped)
      (datum->syntax kw (fun unwrapped))
      (error 'map-syntax-keyword "syntax object identifier: ~s" kw)))

  ;; As above, but through the string domain.
  (define (map-syntax-keyword/str kw fun)
    (map-syntax-keyword kw (string-fun->symbol-fun fun)))

  (define (string-fun->symbol-fun fun)
    (compose1 string->symbol fun symbol->string))

  (define lispify-string~ (string-fun->symbol-fun lispify-string))

  )

;; For a form generated through (define-ffi-definer definer-name ...), generate
;; the corresponding form (definer-name/n ...), which takes only the the
;; external c-name and generates the internal one using the supplied [phase-1]
;; function. The the internal form is CPS-d, to propagate the generated name.

(define-syntax define-naming-definer-for
  (lambda (stx)
    (with-syntax ([(_ definer c-to-lisp-proc) stx])
      (with-syntax ([definer/n
                      (map-syntax-keyword/str
                        #'definer
                        (lambda (name) (string-append name "/n")))])
      #'(define-syntax definer/n
          (lambda (stx)
            (syntax-case stx ()
              [(_ c-name e (... ...) #:continue (k0 k1 (... ...)))
               (with-syntax ([name (map-syntax-keyword #'c-name c-to-lisp-proc)])
                 #'(begin
                     (definer name e (... ...) #:c-id c-name)
                     (k0 name k1 (... ...))))]
              [(me c-name e (... ...))
               #'(me c-name e (... ...) #:continue (void))])))))))

;; Create them in tandem. The topmost ffi-definer-introducer -- FOR NOW.

(define-syntax-rule (define-ffi-definers name lib #:lispify proc foo ...)
  (begin
    (define-ffi-definer name lib foo ...)
    (define-naming-definer-for name proc)))

