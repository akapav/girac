#lang racket

(require ffi/unsafe/define
         (for-meta 1 "names.rkt"
                     racket/match
                     syntax/parse))

(provide (for-meta 1 map-syntax-keyword
                     map-syntax-keyword/str
                     string-fun->symbol-fun
                     decorate-id
                     lispify-string~
                     lispify-string
                     optional-keyword-class
                     atom*
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

  ;; Generate variations of an id with varying pre- and suffixes.
  (define (decorate-id id . dec)
    (define id-s (symbol->string (syntax->datum id)))
    (datum->syntax id
      (for/list ([d dec])
        (string->symbol
          (match d
            [(list pre post) (string-append pre id-s post)]
            [(cons pre post) (string-append pre id-s post)]
            [post            (string-append id-s post)])))))

  (define lispify-string~ (string-fun->symbol-fun lispify-string))

  )

(begin-for-syntax

  ;; Probably a stupid hack - easily manufacture local optional keyword-tagged
  ;; syntax classes for syntax-parse.

  (define-syntax-rule (optional-keyword-class name marker pat alt)
    (define-splicing-syntax-class name
      (pattern (~seq marker pat))
      (pattern (~seq) #:with pat alt)))

  ;; Accept either a singleton or a list and expand to sequence.
  (define-syntax-class atom*
    (pattern (e ...))
    (pattern e~ #:with (e ...) #'(e~)))
)

;; For a form generated through (define-ffi-definer definer-name ...), generate
;; the corresponding form (definer-name/n ...), which takes only the the
;; external c-name and generates the internal one using the supplied [phase-1]
;; function. Inner form is CPS-d, to propagate the generated name.

(define-syntax (define-naming-definer-for stx)
  (with-syntax ([(_ definer c-to-lisp-proc) stx])
    (with-syntax ([(definer/n) (decorate-id #'definer "/n")])
      #'(define-syntax (definer/n stx)
          (syntax-case stx ()
            [(_ c-name e (... ...) #:continue (k0 k1 (... ...)))
             (with-syntax ([name (map-syntax-keyword #'c-name c-to-lisp-proc)])
               #'(begin
                   (definer name e (... ...) #:c-id c-name)
                   (k0 name k1 (... ...))))]
            [(me c-name e (... ...))
             #'(me c-name e (... ...) #:continue (void))])))))

;; Create them in tandem. The topmost ffi-definer-introducer -- FOR NOW.

(define-syntax-rule (define-ffi-definers name lib #:lispify proc foo ...)
  (begin
    (define-ffi-definer name lib foo ...)
    (define-naming-definer-for name proc)))

