#lang racket

(require
  "phase-1-utils.rkt"
  "ffi-type.rkt"
  "gobject.rkt"
  ffi/unsafe
  ffi/unsafe/define
  ffi/unsafe/alloc)


(define-ffi-definers define-libgir
                     (ffi-lib "libgirepository-1.0" "1")
                     #:lispify lispify-string~
                     #:provide provide)

;;;; typez


(define _gint _int)
(define _gboolean _gint)
(define _gsize _ulong)

(define-opaque-ptr _typelib)
(define-opaque-ptr _repo)
(define-opaque-ptr _functioninfo)
(define-opaque-ptr _registered)
(define-opaque-ptr _gerror)

(define _function-flags
  (_bitmask/bits '(method      = 0
                   constructor = 1
                   getter      = 2
                   setter      = 3
                   wraps-vfunc = 4
                   throws      = 5)))

(define-opaque-ptr-hierarchy
  _base-info
  #:specialize g-base-info-get-type =>
    ((1 13 14) _callable-info
     #:specialize g-base-info-get-type =>
        (1  _function-info)
        (13 _signal-info  )
        (14 _vfunc-info   ))
    (2  _callback-info  )
    (3  _struct-info    )
    (4  _boxed-info     )
    (5  _enum-info      )
    (6  _flags-info     )
    (7  _object-info    )
    (8  _interface-info )
    (9  _constant-info  )
    (11 _union-info     )
    (12 _value-info     )
    (15 _property-info  )
    (16 _field-info     )
    (17 _arg-info       )
    (18 _type-info      )
  )

;;;; some random erorrs

;; TODO - figure out how to decode this.
(define (g-error who err)
  (error who "nonsensical error: ~a" err))

;;;; some likely idioms

(define-syntax-rule (define-enumerator->list name (c-get ...) (c-cnt ...))
  (begin
    (define-syntax k
      (syntax-rules ()
        [(_ name)         (define-libgir/n c-get ... #:continue (k name))]
        [(_ get name)     (define-libgir/n c-cnt ... #:continue (k get name))]
        [(_ cnt get name) (begin
                            (define name (enumerator->list get cnt))
                            (provide name get cnt))]))
    (k name)))

(define ((enumerator->list get cnt) ob)
  (for/list ([i (in-range (cnt ob))]) (get ob i)))

;;;; repository functions

;;; unref function for all baseinfo derived types
(define-libgir/n g_base_info_unref
  (_fun _base-info -> _void)
  #:wrap (deallocator))

;;; at the moment, all functions work with the global (default)
;;; repository so #f (null) is always sent as a repository argument

(define-libgir/n g_irepository_prepend_search_path
  (_fun _string -> _void))

;; load a repository (typelib)
(define-libgir/n g_irepository_require
  (_fun (name [ver #f])
   :: (_repo = #f) (name : _string) (ver : _string) (_int = 0) (err : (_ptr o _gerror))
   -> (res : _typelib)
   -> (or res (g-error 'g_irepository_require err))))

(define-libgir/n g_irepository_get_dependencies
  (_fun (_repo = #f) _string -> (res : (_ptr-ptr/list/0 _string))
    -> (for/list ([str res])
         (cdr (regexp-match #px"^(.*)-(\\d+\\.\\d+)$" str)))))

(define-enumerator->list
  repository-info-list
  (g_irepository_get_info (_fun (_repo = #f) _string _gint -> _base-info)
                          #:wrap (allocator g-base-info-unref))
  (g_irepository_get_n_infos (_fun (_repo = #f) _string -> _gint)))

(define-libgir/n g_base_info_get_type (_fun _base-info -> _gint))
(define-libgir/n g_base_info_get_name (_fun _base-info -> _string))

;;;; function type

(define-libgir/n g_function_info_get_symbol (_fun _function-info -> _string))

(define-libgir/n g_function_info_get_flags
                 (_fun _function-info -> _function-flags))

;;;; callable type

(define-libgir/n g_callable_info_get_return_type
                 (_fun _callable-info -> _type-info))

(define-enumerator->list
  callable-arguments
  (g_callable_info_get_arg (_fun _callable-info _gint -> _arg-info))
  (g_callable_info_get_n_args (_fun _callable-info -> _gint)))


