#lang racket

(require "phase-1-utils.rkt"
         (for-syntax syntax/parse)
         "girepository-raw.rkt"
         racket/mpair)

(provide load-gir!
         (struct-out gir-env)
         empty-gir-env
         show-env)


;;;;
;;;; metastructure
;;;;

;; Chainable inits... probably already exists in fome form, can't find yet.

(define inspector (make-inspector))

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

;; A structure that gets created by copying something.

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

;;;;
;;;; structure
;;;; (We should really tweak these according to the needs of later stages.)
;;;;

;; ROCK IT LIKE IT'S 1959!!

(define-syntax (prop stx)
  (syntax-parse stx
    [(_ x tag:id expr:expr) #'`((tag . ,(expr x)))]
    [(_ x tag:id expr:expr #:flag) #'(if (expr x) '(tag) '())]
    [(_ x tag:id expr:expr #:when [id:id test:expr])
     #'(let ([id (expr x)]) (if test `((tag . ,id)) '()))]))

(define-syntax plist
  (syntax-rules ()
    [(_ x (e0 ...) e1 ...) (append (prop x e0 ...) (plist x e1 ...))]
    [(_ x) '()]))


(struct/projective info #:ctor auto-info
  ([namespace g-base-info-get-namespace]
   [name      g-base-info-get-name]))


(struct/projective enum info #:ctor auto-enum
  ([storage g-enum-info-get-storage-type]
   [methods enum-info-get-methods]
   [values  (lambda (p)
              (for/list ([v (in-list (enum-info-get-values p))])
                (cons (g-base-info-get-name v)
                      (g-value-info-get-value v))))]))

(struct/projective callable info #:ctor auto-callable
  ([args
     (lambda (p)
       (for/list ([a (in-list (callable-info-get-args p))])
         ((plist a
            (name          g-base-info-get-name)
            (type          (compose1 decode-typeinfo g-arg-info-get-type))
            (direction     g-arg-info-get-direction)
            (transfer      g-arg-info-get-ownership-transfer)
            (caller-alloc  g-arg-info-is-caller-allocates #:flag)
            (return        g-arg-info-is-return-value     #:flag)
            (optional      g-arg-info-is-optional         #:flag)
            (null          g-arg-info-may-be-null         #:flag))
          . append .
           (if (eq? 'invalid (g-arg-info-get-scope a)) '()
             (plist a
               (scope       g-arg-info-get-scope)
               (closure     g-arg-info-get-closure)
               (destroy     g-arg-info-get-destroy))))))]
   [return
     (lambda (p)
       (plist p
         (type     (compose1 decode-typeinfo g-callable-info-get-return-type))
         (transfer g-callable-info-get-caller-owns)
         (null     g-callable-info-may-return-null #:flag)))]))

(define (decode-typeinfo t)
  (define tag (g-type-info-get-tag t))
  (match tag
    ['interface
     `(interface ,(resolve-raw-info/cycle
                    (g-type-info-get-interface t)))]
    ['array
     `(array .
      ,(plist t
         (array-type g-type-info-get-array-type)
         (type (compose1 decode-typeinfo type-info-param-type))
         (zero-terminated g-type-info-is-zero-terminated #:flag)
         (len g-type-info-get-array-length #:when (l (>= l 0)))
         (fixed g-type-info-get-array-fixed-size #:when (s (>= s 0)))))]
    [(or 'glist 'gslist 'ghash)
     `(,tag (type . ,(decode-typeinfo (type-info-param-type t))))]
    [_ (list tag)]))

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
;;;;

(struct gir-env (entries girs) #:transparent)

(define (empty-gir-env)
  (gir-env (make-hash) (set)))

(define (register-raw-info! env ri)
  (define i (resolve-raw-info ri))
  (hash-set! (gir-env-entries env) (gir-info-sig i) i))

(define (gir-info-sig i)
  (cons (info-namespace i) (info-name i)))

(define (resolve-raw-info i)
  (let-syntax ([app-if (syntax-rules (=>)
                         [(_ (pred => f) ...)
                          (cond [(pred i) (f i)] ...)])])

    (app-if (_function-info? => auto-function)
            (_signal-info?   => auto-signal  )
            (_vfunc-info?    => auto-vfunc   )
            (_callable-info? => auto-callable)
            (_enum-info?     => auto-enum    )
            (values          => auto-info    ))))

(define current-pending (make-parameter #f))

(define (resolve-raw-info/cycle ri)
  (define-values (p i) (values (current-pending)
                               (auto-info ri)))
  (set-box! p (set-add (unbox p) (gir-info-sig i)))
  i)

(define (load-gir! env
                   gir-name [gir-version #f]
                   #:lax        [lax? #f]
                   #:transitive [trans? #t])

  (g-irepository-require gir-name gir-version)

  (define repos
    `((,gir-name ,gir-version)
      . ,(or (and trans? (g-irepository-get-dependencies gir-name))
             '())))

  (define pending (box (set)))

  (parameterize ([current-pending pending])
    (for* ([repo+v repos]
           [raw-info (irepository-get-infos (car repo+v))])
          (register-raw-info! env raw-info)))

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

