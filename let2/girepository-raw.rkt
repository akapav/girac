#lang racket

(require
  "phase-1-utils.rkt"
  "ffi-type.rkt"
  "gobject.rkt"
  "glib-boot/gerror.rkt"
  ffi/unsafe
  ffi/unsafe/define
  ffi/unsafe/alloc
  ffi/cvector)


(define-ffi-definers define-libgir
                     (ffi-lib "libgirepository-1.0" "1")
                     #:lispify lispify-string~
                     #:provide provide)

;;;; typez

(define _size _ulong)

(define-opaque-ptr _typelib)
(define-opaque-ptr _repo)
(define-opaque-ptr _gerror)

;; XXX decode magic numbers
(define _gtype _size)

(define-opaque-ptr-hierarchy
  _base-info
  #:on-ingress (p => (register-finalizer p g-base-info-unref)) =>
    #:specialize g-base-info-get-type
    ((1 13 14) _callable-info =>
        #:specialize g-base-info-get-type
        (1  _function-info)
        (13 _signal-info  )
        (14 _vfunc-info   ))
    (2  _callback-info  )
    ((3 5 7 8 11) _registered-type-info =>
        #:specialize g-base-info-get-type
        (3  _struct-info    )
        (5  _enum-info      )
        (7  _object-info    )
        (8  _interface-info )
        (11 _union-info     ))
    (4  _boxed-info     )
    (6  _flags-info     )
    (9  _constant-info  )
    (12 _value-info     )
    (15 _property-info  )
    (16 _field-info     )
    (17 _arg-info       )
;     (18 _type-info      )
  )

;; wow... this one crashes when treated as base info. gnomyness > 9000.
(define-opaque-ptr _type-info)

(define arg-type-list
  (list _bool
        _int8  _uint8  _int16 _uint16
        _int32 _uint32 _int64 _uint64
        _float _double
        _short _ushort
        _int   _uint   _long  _ulong
;         gssize   v_ssize;
;         gsize    v_size;
        _string
        _pointer))

(define _argument (apply _union arg-type-list))

(define _transfer (_enum (seq-labels nothing container everything)))

; ;; The api claims this was supposed to be "opaque", however, there are no
; ;; constructors. We need to *form* these, opaque, things.
; (define-cstruct _attribute-iter ([data1 _pointer]
;                                  [data2 _pointer]
;                                  [data3 _pointer]
;                                  [data4 _pointer]))

; (define (empty-attribute-iter)
;   (make-attribute-iter #f #f #f #f))

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

(define-syntax-rule (define-enumerator->list* name c-get c-cnt _from _to)
  (define-enumerator->list
    name
    (c-get (_fun _from _int -> _to))
    (c-cnt (_fun _from -> _int))))


;;;; repository functions

;;; unref function for all baseinfo derived types
(define-libgir/n g_base_info_unref (_fun _base-info -> _void))

;;; at the moment, all functions work with the global (default)
;;; repository so #f (null) is always sent as a repository argument

(define-libgir/n g_irepository_prepend_search_path
  (_fun _string -> _void))

;; load a repository (typelib)
(define-libgir/n g_irepository_require
  (_fun (name [ver #f])
   :: (_repo = #f) (name : _string) (ver : _string) (_int = 0) _gerror/raise
   -> _typelib))

(define-libgir/n g_irepository_get_dependencies
  (_fun (_repo = #f) _string -> (res : (_ptr-ptr/list/0 _string))
    -> (if (not res) '()
         (for/list ([str res])
           (cdr (regexp-match #px"^(.*)-(\\d+\\.\\d+)$" str))))))

(define-enumerator->list
  irepository-get-infos
  (g_irepository_get_info (_fun (_repo = #f) _string _int -> _base-info))
  (g_irepository_get_n_infos (_fun (_repo = #f) _string -> _int)))

(define-libgir/n g_irepository_find_by_name
                 (_fun (_repo = #f) _string _string -> _base-info))

(define-libgir/n g_base_info_get_type (_fun _base-info -> _int))
(define-libgir/n g_base_info_get_name (_fun _base-info -> _string))
(define-libgir/n g_base_info_get_namespace  (_fun _base-info -> _string))
; (define-libgir/n g_base_info_iterate_attributes
;   (_fun _base-info _attribute-iter (_ptr o _string) (_ptr o _string) -> _bool))

;;;; callable type

(define-libgir/n g_callable_info_get_return_type
                 (_fun _callable-info -> _type-info))
(define-libgir/n g_callable_info_get_caller_owns
                 (_fun _callable-info -> _transfer))
(define-libgir/n g_callable_info_may_return_null
                 (_fun _callable-info -> _bool))

(define-enumerator->list* callable-info-get-args
  g_callable_info_get_arg g_callable_info_get_n_args 
  _callable-info _arg-info)

;;;; function type

(define _function-flags
  (_bitmask/bits (seq-labels
    method constructor getter setter wraps-vfunc throws)))

(define-libgir/n g_function_info_get_symbol (_fun _function-info -> _string))
(define-libgir/n g_function_info_get_flags (_fun _function-info -> _function-flags))
(define-libgir/n g_function_info_get_vfunc (_fun _function-info -> _vfunc-info))

;; props?

; (define-libgir/n g_function_info_invoke

;                  (_fun (info inlist) ::
;                        (info : _function-info)
;                        (v-in : _cvector = (list->cvector inlist _argument))
;                        (_int = (cvector-length v-in))
;                        (_pointer = #f) (_int = 0)
;                        (_ptr o _argument)
;                        _gerror/raise
;                        -> _bool))

;;;; vfunc type

(define _vfunc-flags
  (_bitmask/bits (seq-labels must-chain-up must-override must-not-override)))

(define-libgir/n g_vfunc_info_get_flags (_fun _vfunc-info -> _vfunc-flags))
(define-libgir/n g_vfunc_info_get_offset
                 (_fun _vfunc-info -> (r : _int) -> (if (= r #xffff) #f r)))
(define-libgir/n g_vfunc_info_get_signal (_fun _vfunc-info -> _signal-info))
(define-libgir/n g_vfunc_info_get_invoker (_fun _vfunc-info -> _function-info))

;;;; signal type

(define _signal-flags
  (_bitmask/bits (seq-labels
    run-first run-last run-cleanup
    no-recurse detailed action no-hooks must-collect)))

(define-libgir/n g_signal_info_get_flags (_fun _signal-info -> _signal-flags))
(define-libgir/n g_signal_info_get_class_closure (_fun _signal-info -> _vfunc-info))
(define-libgir/n g_signal_info_true_stops_emit (_fun _signal-info -> _bool))

;;;; typeinfo type

(define _typetag
  (_enum (seq-labels
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

(define _array-type (_enum (seq-labels c array ptr-array byte-array)))

(define-libgir/n g_type_info_get_tag (_fun _type-info -> _typetag))
(define-libgir/n g_type_tag_to_string (_fun _typetag -> _string))
(define-libgir/n g_type_info_get_interface (_fun _type-info -> _base-info))
(define-libgir/n g_type_info_get_param_type (_fun _type-info _int -> _type-info))
(define-libgir/n g_type_info_is_pointer (_fun _type-info -> _bool))
(define-libgir/n g_type_info_get_array_length (_fun _type-info -> _int))
(define-libgir/n g_type_info_get_array_fixed_size (_fun _type-info -> _int))
(define-libgir/n g_type_info_is_zero_terminated (_fun _type-info -> _bool))
(define-libgir/n g_type_info_get_array_type (_fun _type-info -> _array-type))
(define-libgir type-info-is-basic?
               (_fun _type-info -> (r : _int) -> (or (< r 15) (= r 21)))
               #:c-id g_type_info_get_tag)

;;;; arginfo type

(define _direction (_enum (seq-labels in out inout)))
(define _scope-type (_enum (seq-labels invalid call async notified)))

(define-libgir/n g_arg_info_get_direction (_fun _arg-info -> _direction))
(define-libgir/n g_arg_info_is_caller_allocates (_fun _arg-info -> _bool))
(define-libgir/n g_arg_info_is_return_value (_fun _arg-info -> _bool))
(define-libgir/n g_arg_info_is_optional (_fun _arg-info -> _bool))
(define-libgir/n g_arg_info_may_be_null (_fun _arg-info -> _bool))
(define-libgir/n g_arg_info_get_ownership_transfer (_fun _arg-info -> _transfer))
(define-libgir/n g_arg_info_get_type (_fun _arg-info -> _type-info))
(define-libgir/n g_arg_info_get_scope (_fun _arg-info -> _scope-type))
(define-libgir/n g_arg_info_get_closure (_fun _arg-info -> _int))
(define-libgir/n g_arg_info_get_destroy (_fun _arg-info -> _int))

;;;; struct type

(define-libgir/n g_struct_info_get_size (_fun _struct-info -> _size))
(define-libgir/n g_struct_info_is_foreign (_fun _struct-info -> _bool))

(define-enumerator->list* struct-info-get-methods
  g_struct_info_get_method g_struct_info_get_n_methods 
  _struct-info _function-info)

;;;; enum type

(define-enumerator->list* enum-info-get-values
  g_enum_info_get_value g_enum_info_get_n_values
  _enum-info _value-info)

(define-enumerator->list* enum-info-get-methods
  g_enum_info_get_method g_enum_info_get_n_methods
  _enum-info _function-info)
  
(define-libgir/n g_value_info_get_value (_fun _value-info -> _int64))
(define-libgir/n g_enum_info_get_storage_type (_fun _enum-info -> _typetag))

;;;; iface type

(define-enumerator->list* interface-info-get-prerequisites
  g_interface_info_get_prerequisite g_interface_info_get_n_prerequisites 
  _interface-info _base-info)

(define-enumerator->list* interface-info-get-properties
  g_interface_info_get_property g_interface_info_get_n_properties 
  _interface-info _property-info)

(define-enumerator->list* interface-info-get-methods
  g_interface_info_get_method g_interface_info_get_n_methods 
  _interface-info _function-info)

(define-enumerator->list* interface-info-get-signals
  g_interface_info_get_signal g_interface_info_get_n_signals 
  _interface-info _signal-info)

(define-enumerator->list* interface-info-get-vfuncs
  g_interface_info_get_vfunc g_interface_info_get_n_vfuncs 
  _interface-info _vfunc-info)

(define-enumerator->list* interface-info-get-constants
  g_interface_info_get_constant g_interface_info_get_n_constants 
  _interface-info _constant-info)

(define-libgir/n g_interface_info_get_iface_struct (_fun _interface-info -> _struct-info))

; g_interface_info_get_method
; g_interface_info_find_vfunc


;;;; class type

(define-libgir/n g_object_info_get_type_name          (_fun _object-info -> _string))
(define-libgir/n g_object_info_get_type_init          (_fun _object-info -> _string))
(define-libgir/n g_object_info_get_abstract           (_fun _object-info -> _bool))
(define-libgir/n g_object_info_get_fundamental        (_fun _object-info -> _bool))
(define-libgir/n g_object_info_get_parent             (_fun _object-info -> _object-info))
(define-libgir/n g_object_info_get_class_struct       (_fun _object-info -> _struct-info))
(define-libgir/n g_object_info_get_unref_function     (_fun _object-info -> _string))
(define-libgir/n g_object_info_get_ref_function       (_fun _object-info -> _string))
(define-libgir/n g_object_info_get_set_value_function (_fun _object-info -> _string))
(define-libgir/n g_object_info_get_get_value_function (_fun _object-info -> _string))

(define-enumerator->list* object-info-get-interfaces
  g_object_info_get_interface g_object_info_get_n_interfaces
  _object-info _interface-info)

(define-enumerator->list* object-info-get-fields
  g_object_info_get_field  g_object_info_get_n_fields
  _object-info _field-info)

(define-enumerator->list* object-info-get-properties
  g_object_info_get_property g_object_info_get_n_properties
  _object-info _property-info)

(define-enumerator->list* object-info-get-methods         
  g_object_info_get_method g_object_info_get_n_methods
  _object-info _function-info)

(define-enumerator->list* object-info-get-signals         
  g_object_info_get_signal g_object_info_get_n_signals
  _object-info _signal-info)

(define-enumerator->list* object-info-get-vfuncs          
  g_object_info_get_vfunc g_object_info_get_n_vfuncs
  _object-info _vfunc-info)

(define-enumerator->list* object-info-get-constants       
  g_object_info_get_constant g_object_info_get_n_constants
  _object-info _constant-info)


;;;; constant type

(define-libgir/n g_constant_info_get_type (_fun _constant-info -> _type-info))
; (define-libgir/n g_constant_info_get_value (_fun _constant-info -> _argument))

;;;; field type

(define _fieldinfo-flags (_bitmask/bits (seq-labels readable writable)))

(define-libgir/n g_field_info_get_flags (_fun _field-info -> _fieldinfo-flags))
(define-libgir/n g_field_info_get_size (_fun _field-info -> _int))
(define-libgir/n g_field_info_get_offset (_fun _field-info -> _int))
(define-libgir/n g_field_info_get_type (_fun _field-info -> _type-info))

;;;; property type

(define _param-flags
  (_bitmask/bits (seq-labels
    readable writable construct construct_only lax_validation
    static_name static_nick static_blurb private deprecated)))

(define-libgir/n g_property_info_get_flags (_fun _property-info -> _param-flags))
(define-libgir/n g_property_info_get_type (_fun _property-info -> _type-info))
(define-libgir/n g_property_info_get_ownership_transfer (_fun _property-info -> _transfer))

;;;; registeredtypeinfo type

(define-libgir/n g_registered_type_info_get_type_name
                 (_fun _registered-type-info -> _string))
(define-libgir/n g_registered_type_info_get_type_init
                 (_fun _registered-type-info -> _string))
(define-libgir/n g_registered_type_info_get_g_type
                 (_fun _registered-type-info -> _gtype))

