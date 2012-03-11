#lang racket

(require (for-syntax syntax/parse)
         "phase-1-utils.rkt"
         srfi/26
         "struct-projective.rkt"
         "girepository-raw.rkt")

(provide load-gir!

         (struct-out info)
         (struct-out enum)
         (struct-out argument)
         (struct-out callable)
         (struct-out function)
         (struct-out signal)
         (struct-out vfunc)

         (struct-out typeinfo)
         (struct-out typeinfo-prim)
         (struct-out typeinfo-array)
         (struct-out typeinfo-gcontainer)

         (struct-out gir-ref)
         (struct-out gir-env)

         empty-gir-env
         show-env)

;;;;
;;;; structure
;;;; (We should really tweak these according to the needs of later stages.)

;; This is useful, not for the name, but because it's a macro so
;; f and g needn't be already bound.
(define-syntax-rule (<< f g) (lambda (x) (f (g x))))

(define ((map_ f) xs) (map f xs))


(struct/projective info #:ctor make-info
  ([namespace g-base-info-get-namespace]
   [name      g-base-info-get-name]))

(struct/projective reg-type info #:ctor make-reg-type
  ([name  g-registered-type-info-get-type-name]
   [init  g-registered-type-info-get-type-init]
   [gtype g-registered-type-info-get-g-type   ] ))

(struct/projective enum reg-type #:ctor make-enum
  ([storage (type-tag-assert-simple . << . g-enum-info-get-storage-type)]
   [methods ((map_ resolve-info/local) . << . enum-info-get-methods)    ]
   [values  ((map_ (lambda (v) (cons (g-base-info-get-name v)
                                     (g-value-info-get-value v))))
             . << . enum-info-get-values)                               ] ))

(struct/projective interface reg-type #:ctor make-interface
  ([prereq       (<< (map_ resolve-info      ) interface-info-get-prerequisites)]
   [props        (<< (map_ resolve-info/local) interface-info-get-properties   )]
   [methods      (<< (map_ resolve-info/local) interface-info-get-methods      )]
   [vfuncs       (<< (map_ resolve-info/local) interface-info-get-vfuncs       )]
   [signals      (<< (map_ resolve-info/local) interface-info-get-signals      )]
   [constants    (<< (map_ resolve-info      ) interface-info-get-constants    )]
   [iface-struct g-interface-info-get-iface-struct] ))

(struct/projective class reg-type #:ctor make-class
  ([abstract?    g-object-info-get-abstract                               ]
   [fundamental? g-object-info-get-fundamental                            ]
   [parent       g-object-info-get-parent                                 ]
   [interfaces   (<< (map_ resolve-info      ) object-info-get-interfaces)]
   [fields       (<< (map_ resolve-info/local) object-info-get-fields    )]
   [props        (<< (map_ resolve-info/local) object-info-get-properties)]
   [methods      (<< (map_ resolve-info/local) object-info-get-methods   )]
   [vfuncs       (<< (map_ resolve-info/local) object-info-get-vfuncs    )]
   [signals      (<< (map_ resolve-info/local) object-info-get-signals   )]
   [constants    (<< (map_ resolve-info      ) object-info-get-constants )]
   [class-struct g-object-info-get-class-struct                           ]
   [ref+unref    (lambda (i)
                   (cons (g-object-info-get-ref-function i)
                         (g-object-info-get-unref-function i)))]
   [get+set      (lambda (i)
                   (cons (g-object-info-get-get-value-function i)
                         (g-object-info-get-set-value-function i)))] ))


(struct/projective argument info #:ctor make-argument
  ([type           (make-typeinfo . << . g-arg-info-get-type)  ]
   [direction      g-arg-info-get-direction                    ]
   [transfer       g-arg-info-get-ownership-transfer           ]
   [caller-alloc?  g-arg-info-is-caller-allocates              ]
   [return?        g-arg-info-is-return-value                  ]
   [optional?      g-arg-info-is-optional                      ]
   [null?          g-arg-info-may-be-null                      ]
   [closure-info   (match-lambda
                     [(app g-arg-info-get-scope 'invalid) #f]
                     [p `((scope   ,(g-arg-info-get-scope   p))
                          (closure ,(g-arg-info-get-closure p))
                          (destroy ,(g-arg-info-get-destroy p)))])]
   ))

(struct/projective callable info #:ctor make-callable
  ([args   ((map_ make-argument) . << . callable-info-get-args)   ]
   [return (lambda (p)
             `((type     ,(make-typeinfo
                            (g-callable-info-get-return-type p)))
               (transfer ,(g-callable-info-get-caller-owns   p))
               (null?    ,(g-callable-info-may-return-null   p))))] ))

(struct/projective constant info #:ctor make-constant
  ([type (make-typeinfo . << . g-constant-info-get-type)]
   [value (const 'TODO)]))

(struct/projective field info #:ctor make-field
  ([flags  g-field-info-get-flags ]
   [size   g-field-info-get-size  ]
   [offset g-field-info-get-offset]
   [type   g-field-info-get-type  ] ))

(struct/projective property info #:ctor make-property
  ([flags    g-property-info-get-flags             ]
   [type     g-property-info-get-type              ]
   [transfer g-property-info-get-ownership-transfer] ))

;; fali....
(struct/projective function callable #:ctor make-function
  ([symbol g-function-info-get-symbol]
   [flags  g-function-info-get-flags ]
   [fvunc  (lambda (p)
             (and (memq 'wraps-vfunc (g-function-info-get-flags p))
                  (g-function-info-get-vfunc p)))] ))

(struct/projective signal callable #:ctor make-signal
  ([flags            g-signal-info-get-flags        ]
   [class-closure    g-signal-info-get-class-closure]
   [true-stops-emit? g-signal-info-true-stops-emit  ] ))

(struct/projective vfunc callable #:ctor make-vfunc
  ([flags   g-vfunc-info-get-flags  ]
   [offset  g-vfunc-info-get-offset ]
   [signal  g-vfunc-info-get-signal ]
   [invoker g-vfunc-info-get-invoker] ))


;;;;
;;;; typeinfo zone

(struct/projective typeinfo #:ctor make-typeinfo~
  ([pointer? g-type-info-is-pointer]))

(struct/projective typeinfo-prim typeinfo #:ctor mk-tpi-simple
  ([type g-type-info-get-tag]))

(struct/projective typeinfo-iface typeinfo #:ctor mk-tpi-iface
  ([type (resolve-info . << . g-type-info-get-interface)]))

(struct/projective typeinfo-array typeinfo #:ctor mk-tpi-ary
  ([element-type     (make-typeinfo . << . type-info-param-type)        ]
   [array-type       g-type-info-get-array-type                            ]
   [zero-terminated? g-type-info-is-zero-terminated                        ]
   [length           (gate/f [l g-type-info-get-array-length    ] (>= l 0))]
   [fixed-length     (gate/f [s g-type-info-get-array-fixed-size] (>= s 0))] ))

(struct/projective typeinfo-gcontainer typeinfo #:ctor mk-tpi-gcon
  ([container    g-type-info-get-tag                           ]
   [element-type (make-typeinfo . << . type-info-param-type)] ))

(define-syntax-rule (gate/f [id f] test)
  (lambda (x) (let ([id (f x)]) (if test id #f))))

(define (make-typeinfo ti)
  (define tag (g-type-info-get-tag ti))
  (case tag
    [(glist gslist ghash) (mk-tpi-gcon   ti)]
    [(array)              (mk-tpi-ary    ti)]
    [(interface)          (mk-tpi-iface  ti)]
    [else                 (mk-tpi-simple ti)]))

(define (type-tag-assert-simple tt)
  (when (memq tt '(glist gslist ghash array interface))
    (error 'type-tag-assert-simple
           "not a simple type tag: ~s" tt))
  tt)

;; Utterly underdocumented, it seems we are expected to know the arity of
;; higher-kinded types in advance. And they are all of kind *->*, really.
(define (type-info-param-type t)
  (g-type-info-get-param-type t 0))


;;;;
;;;; decode

(define current-name-ctx (make-parameter #f))

(define current-gir-env (make-parameter #f))

(define current-pending (make-parameter #f))

(struct gir-env (entries girs) #:transparent)

(struct gir-ref (namespace name scope) #:transparent)

(define (empty-gir-env) (gir-env (make-hash) (set)))

(define (make-gir-ref ob [scoped #t])
  (gir-ref (g-base-info-get-namespace ob)
           (g-base-info-get-name ob)
           (and scoped (current-name-ctx))))

(define (nothing-pending) (box (set)))

(define (register-resolved-thingie! ref ob)
  (hash-set! (gir-env-entries (current-gir-env)) ref ob))

(define (register-pending! ref)
  (let ([p (current-pending)])
    (set-box! p (set-add (unbox p) ref))))

(define-syntax-rule (with-name-ctx x e ...)
  (let ([ctx (cond [(current-name-ctx)
                    => (cute cons (g-base-info-get-name x) <>)]
                   [else (list (g-base-info-get-name x))])])
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
  (app-if i (_function-info?  => make-function )
            (_signal-info?    => make-signal   )
            (_vfunc-info?     => make-vfunc    )
            (_callable-info?  => make-callable )
            (_enum-info?      => make-enum     )
            (_interface-info? => make-interface)
            (_object-info?    => make-class    )
            (_constant-info?  => make-constant )
            (_field-info?     => make-field    )
            (_property-info?  => make-property )
            (values           => (const 'WTF) )))
;             (values          => make-info    )))

(define (load-gir! env gir-name [gir-version #f]
                   #:lax        [lax? #f]
                   #:transitive [trans? #t])

  (g-irepository-require gir-name gir-version)

  (let ([pending (nothing-pending)]
        [repos `((,gir-name ,gir-version)
                 . ,(or (and trans? (g-irepository-get-dependencies gir-name))
                        '()))])

    (parameterize ([current-gir-env env]
                   [current-pending pending])
      (for* ([repo+v repos]
             [raw-info (irepository-get-infos (car repo+v))])
        (resolve-info raw-info))

      (unless lax?  (verify-references env (unbox pending)))
      repos)))

(define (verify-references env pending)
  (let* ([env-dict (gir-env-entries env)]
         [unresolved
          (for/list ([sig (in-set pending)]
                     #:when (not (hash-has-key? env-dict sig)))
            sig)])
    (when (pair? unresolved)
      (error 'load-gir!
             "some info entries are still unknown: ~s" unresolved))))

(define show-env (<< pretty-print gir-env-entries))

