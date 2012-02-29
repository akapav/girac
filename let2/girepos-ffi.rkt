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
                     #:lispify lispify-string~)

;;;; typez


(define _gint _int)
(define _gboolean _gint)
(define _gsize _ulong)

(define-opaque-ptr _typelib)
(define-opaque-ptr _repo)
(define-opaque-ptr _functioninfo)
(define-opaque-ptr _registered)
(define-opaque-ptr _gerror)

(define-opaque-ptr-hierarchy
  (_baseinfo ->
             (_typeinfo)
             (_arginfo)
             (_gobjinfo)
             (_funcinfo)
             (_callableinfo)
             (_structinfo)
             (_enuminfo)
             (_valueinfo)
             (_interfaceinfo)))

;;;; some random erorrs

;; TODO - figure out how to decode this.
(define (g-error who err)
  (error who "nonsensical error: ~a" err))

;;;; some likely idioms

(define-syntax-rule (define-enumerator->list name (c-get ...) (c-cnt ...))
  (begin
    (define-syntax k
      (syntax-rules ()
        [(_ get name) (define-libgir/n c-cnt ... #:continue (k get name))]
        [(_ cnt get name) (define name (enumerator->list get cnt))]))
    (define-libgir/n c-get ... #:continue (k name))))

(define ((enumerator->list get cnt) ob)
  (for/list ([i (in-range (cnt ob))]) (get ob i)))

;;;; repository functions

;;; unref function for all baseinfo derived types
(define-libgir/n g_base_info_unref
  (_fun _baseinfo -> _void)
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
  (g_irepository_get_info (_fun (_repo = #f) _string _gint -> _baseinfo)
                          #:wrap (allocator g-base-info-unref))
  (g_irepository_get_n_infos (_fun (_repo = #f) _string -> _gint)))

(define-libgir/n g_base_info_get_type (_fun _baseinfo -> _gint))
(define-libgir/n g_base_info_get_name (_fun _baseinfo -> _string))

