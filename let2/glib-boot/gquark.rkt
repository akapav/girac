#lang racket

(require "definer.rkt"
         ffi/unsafe)

(provide gquark)

(define gquark _uint32)

(define-glib make-quark    (_fun _string*/utf-8 -> gquark) #:c-id g_quark_from_string )

(define-glib quark->string (_fun gquark -> _string*/utf-8) #:c-id g_quark_to_string)

(define-glib string->quark
             (_fun _string*/utf-8 -> (res : gquark) -> (if (= res 0) #f res))
             #:c-id g_quark_try_string)
