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

(define _size _ulong)

(define-opaque-ptr _typelib)
(define-opaque-ptr _repo)
(define-opaque-ptr _functioninfo)
(define-opaque-ptr _registered)
(define-opaque-ptr _gerror)

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
        [(_ cnt get name) (define/provide name (enumerator->list get cnt))]))
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
  irepository-get-infos
  (g_irepository_get_info (_fun (_repo = #f) _string _int -> _base-info)
                          #:wrap (allocator g-base-info-unref))
  (g_irepository_get_n_infos (_fun (_repo = #f) _string -> _int)))

(define-libgir/n g_base_info_get_type (_fun _base-info -> _int))
(define-libgir/n g_base_info_get_name (_fun _base-info -> _string))

;;;; function type

(define _function-flags
  (_bitmask/bits (sequentially
    method constructor getter setter wraps-vfunc throws)))

(define-libgir/n g_function_info_get_symbol (_fun _function-info -> _string))

(define-libgir/n g_function_info_get_flags
                 (_fun _function-info -> _function-flags))

;;;; callable type

(define-libgir/n g_callable_info_get_return_type
                 (_fun _callable-info -> _type-info))

(define-enumerator->list
  callable-info-get-args
  (g_callable_info_get_arg (_fun _callable-info _int -> _arg-info))
  (g_callable_info_get_n_args (_fun _callable-info -> _int)))

;;;; typeinfo type

(define _typetag
  (_enum (sequentially
    void boolean
    int8 uint8 int16 uint16 int32 uint32 int64 uint64
    float double
    gtype
    utf8 filename
    array
    interface
    glist gslist ghash
    error
    unichar)))

(define-libgir/n g_type_info_get_tag (_fun _type-info -> _typetag))

(define-libgir/n g_type_tag_to_string (_fun _typetag -> _string))

(define-libgir/n g_type_info_get_interface (_fun _type-info -> _base-info))

(define-libgir/n g_type_info_is_pointer (_fun _type-info -> _bool))

(define-libgir type-info-is-basic?
               (_fun _type-info -> (r : _int) -> (or (< r 15) (= r 21)))
               #:c-id g_type_info_get_tag)

(define-libgir/n g_type_info_get_array_length (_fun _type-info -> _int))

; (define-libgir/n g_type_info_get_array_type (_fun _type-info -> _

;;;; arginfo type

(define _direction (_enum (sequentially in out inout)))

(define _transfer (_enum (sequentially nothing container everything)))

(define-libgir/n g_arg_info_get_direction (_fun _arg-info -> _direction))

(define-libgir/n g_arg_info_is_caller_allocates (_fun _arg-info -> _bool))

(define-libgir/n g_arg_info_is_return_value (_fun _arg-info -> _bool))

(define-libgir/n g_arg_info_is_optional (_fun _arg-info -> _bool))

(define-libgir/n g_arg_info_may_be_null (_fun _arg-info -> _bool))

(define-libgir/n g_arg_info_get_ownership_transfer (_fun _arg-info -> _transfer))

(define-libgir/n g_arg_info_get_type (_fun _arg-info -> _type-info))

;;;; object type

(define-libgir/n g_object_info_get_type_name (_fun _object-info -> _string))

(define-libgir/n g_object_info_get_type_init (_fun _object-info -> _string))

(define-libgir/n g_object_info_get_abstract (_fun _object-info -> _bool))

(define-libgir/n g_object_info_get_parent (_fun _object-info -> _object-info))

(define-enumerator->list
  object-info-get-methods
  (g_object_info_get_method (_fun _object-info _int -> _function-info))
  (g_object_info_get_n_methods (_fun _object-info -> _int)))

(define-enumerator->list
  object-info-get-interfaces
  (g_object_info_get_interface (_fun _object-info _int -> _interface-info))
  (g_object_info_get_n_interfaces (_fun _object-info -> _int)))

;;;; struct type

(define-libgir/n g_struct_info_get_size (_fun _struct-info -> _size))

(define-libgir/n g_struct_info_is_foreign (_fun _struct-info -> _bool))

(define-enumerator->list
  struct-info-get-methods
  (g_struct_info_get_method (_fun _struct-info _int -> _function-info))
  (g_struct_info_get_n_methods (_fun _struct-info -> _int)))

;;;; enum type

(define-enumerator->list
  enum-info-get-values
  (g_enum_info_get_value (_fun _enum-info _int -> _value-info))
  (g_enum_info_get_n_values (_fun _enum-info -> _int)))

(define-libgir/n g_value_info_get_value (_value-info -> _int64))

;;;; iface type

(define-enumerator->list
  interface-info-get-prerequisites
  (g_interface_info_get_prerequisite (_fun _interface-info _int -> _base-info))
  (g_interface_info_get_n_prerequisites (_fun _interface-info -> _int)))

(define-enumerator->list
  interface-info-get-methods
  (g_interface_info_get_method (_fun _interface-info _int -> _function-info))
  (g_interface_info_get_n_methods (_fun _interface-info -> _int)))


