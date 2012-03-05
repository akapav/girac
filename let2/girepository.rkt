#lang racket

(require (for-syntax syntax/parse)
         srfi/26
         "struct-projective.rkt"
         "girepository-raw.rkt")

(provide load-gir!
         (struct-out info)
         (struct-out enum)
         (struct-out callable)
         (struct-out function)
         (struct-out signal)
         (struct-out vfunc)
         (struct-out gir-env)
         empty-gir-env
         show-env)

;;;;
;;;; structure
;;;; (We should really tweak these according to the needs of later stages.)

(struct/projective info #:ctor auto-info
  ([namespace g-base-info-get-namespace]
   [name      g-base-info-get-name]))


(struct/projective enum info #:ctor auto-enum
  ([storage g-enum-info-get-storage-type]
   ;;; WHAT THE FUCK IS A `METHOD' ATTACHED SOLELY TO AN ENUMERATION THAT
   ;;; DOESN'T EVEN MENTION THE GIVEN ENUMERATION!!??
;    [methods (lambda (p)
;               (map resolve-info/local (enum-info-get-methods p)))]
;    [methods (lambda (p) (map resolve-info/local
;                              (enum-info-get-methods p)))]
   [values  (lambda (p)
              (for/list ([v (in-list (enum-info-get-values p))])
                (cons (g-base-info-get-name v)
                      (g-value-info-get-value v))))]))

(struct/projective argument info #:ctor auto-argument
  ([type           (lambda (p) (decode-type-info (g-arg-info-get-type p)))]
   [direction      g-arg-info-get-direction]
   [transfer       g-arg-info-get-ownership-transfer]
   [caller-alloc?  g-arg-info-is-caller-allocates]
   [return?        g-arg-info-is-return-value]
   [optional?      g-arg-info-is-optional]
   [null?          g-arg-info-may-be-null]
   [closure-info   (match-lambda
                     [(app g-arg-info-get-scope 'invalid) #f]
                     [p `((scope   ,(g-arg-info-get-scope   p))
                          (closure ,(g-arg-info-get-closure p))
                          (destroy ,(g-arg-info-get-destroy p)))])]
))

(struct/projective callable info #:ctor auto-callable
  ([args   (lambda (p) (map auto-argument (callable-info-get-args p)))]
   [return (lambda (p)
             `((type     ,(decode-type-info
                            (g-callable-info-get-return-type p)))
               (transfer ,(g-callable-info-get-caller-owns   p))
               (null?    ,(g-callable-info-may-return-null   p))))]))

(define-syntax-rule (gate/f [id f] test)
  (lambda (x) (let ([id (f x)]) (if test id #f))))

(struct/projective array-type #:ctor auto-array-type
  ([element-type     (lambda (p) (decode-type-info (type-info-param-type p)))]
   [array-type       g-type-info-get-array-type]
   [zero-terminated? g-type-info-is-zero-terminated]
   [length           (gate/f [l g-type-info-get-array-length    ] (>= l 0))]
   [fixed-length     (gate/f [s g-type-info-get-array-fixed-size] (>= s 0))]))

(struct/projective gcontainer-type #:ctor auto-gcontainer-type
  ([container    g-type-info-get-tag]
   [element-type (lambda (p) (decode-type-info (type-info-param-type p)))]))


(define (decode-type-info ti)
  (define tag (g-type-info-get-tag ti))
  (case tag
    [(glist gslist ghash) (auto-gcontainer-type ti)]
    [(array)     (auto-array-type ti)]
    [(interface) (resolve-info (g-type-info-get-interface ti))]
    [else        tag]))

;; Utterly underdocumented, it seems we are expected to know the arity of
;; higher-kinded types in advance. And they are all of kind *->*, really.
(define (type-info-param-type t) (g-type-info-get-param-type t 0))

;; fali....
(struct/projective function callable #:ctor auto-function
  ([symbol g-function-info-get-symbol]
   [flags  g-function-info-get-flags]))

(struct/projective signal callable #:ctor auto-signal
  ([flags            g-signal-info-get-flags]
   [class-closure    g-signal-info-get-class-closure]
   [true-stops-emit? g-signal-info-true-stops-emit]))

(struct/projective vfunc callable #:ctor auto-vfunc
  ([flags   g-vfunc-info-get-flags]
   [offset  g-vfunc-info-get-offset]
   [signal  g-vfunc-info-get-signal]
   [invoker g-vfunc-info-get-invoker]))

;;;;
;;;; decode

(define current-name-ctx (make-parameter #f))

(define current-gir-env (make-parameter #f))

(define current-pending (make-parameter #f))

(struct gir-env (entries girs) #:transparent)

(struct gir-ref (namespace name scope) #:transparent)

(define (empty-gir-env) (gir-env (make-hash) (set)))

(define (make-gir-ref ob [scoped #t])
  (gir-ref (g-base-info-get-name ob)
           (g-base-info-get-namespace ob)
           (and scoped (current-name-ctx))))

(define (nothing-pending) (box (set)))

(define (register-resolved-thingie! ref ob)
  (hash-set! (gir-env-entries (current-gir-env)) ref ob))

(define (register-pending! ref)
  (define p (current-pending))
  (set-box! p (set-add (unbox p) ref)))

(define-syntax-rule (with-name-ctx x e ...)
  (begin
    (define ctx (cond [(current-name-ctx)
                       => (cute cons (g-base-info-get-name x) <>)]
                      [else (g-base-info-get-name x)]))
    (parameterize ([current-name-ctx ctx]) e ...)))

(define (resolve-info/local ri)
  (resolve-info ri #:local-info #t))

(define (resolve-info ri #:local-info [local? #f])
  (let ([reference (make-gir-ref ri local?)])
    (cond [(and (current-name-ctx) (not local?))
           (register-pending! reference)]
          [else (with-name-ctx ri
                  (register-resolved-thingie!
                    reference (project-raw-info ri)))])
    reference))

(define (project-raw-info i)
  (let-syntax ([app-if (syntax-rules (=>)
                         [(_ (pred => f) ...)
                          (cond [(pred i) (f i)] ...)])])

    (app-if (_function-info? => auto-function)
            (_signal-info?   => auto-signal  )
            (_vfunc-info?    => auto-vfunc   )
            (_callable-info? => auto-callable)
            (_enum-info?     => auto-enum    )
            (values          => auto-info    ))))

(define (load-gir! env gir-name [gir-version #f]
                   #:lax        [lax? #f]
                   #:transitive [trans? #t])

  (g-irepository-require gir-name gir-version)

  (define repos
    `((,gir-name ,gir-version)
      . ,(or (and trans? (g-irepository-get-dependencies gir-name))
             '())))

  (define pending (nothing-pending))

  (parameterize ([current-gir-env env]
                 [current-pending pending])
    (for* ([repo+v repos]
           [raw-info (irepository-get-infos (car repo+v))])
      (resolve-info raw-info)))

  (unless lax?
    (define unresolved
      (for/list ([sig (in-set (unbox pending))]
                 #:when
                 (not (hash-has-key? (gir-env-entries env) sig)))
        sig))
    (when (pair? unresolved)
      (error 'load-gir! "some info entries are still unknown: ~s"
             unresolved)))
  repos)

(define show-env (compose1 pretty-print gir-env-entries))

