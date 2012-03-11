#lang racket

(require "definer.rkt"
         "gquark.rkt"
         ffi/unsafe)

(provide _gerror/raise
         (struct-out exn:fail:gerror))

(struct exn:fail:gerror exn:fail [domain code] #:transparent)

(define-cstruct _gerror ([domain  gquark       ]
                         [code    _int         ]
                         [message _string/utf-8]))

(define-glib/n g_error_free (_fun _gerror-pointer -> _void))

(define (raise-gerror p-err)
  (match-let ([(gerror (app quark->string domain) code message) p-err])
    (g-error-free p-err)
    (raise (exn:fail:gerror message (current-continuation-marks) domain code) #t)))

(define-fun-syntax _gerror/raise
  (syntax-id-rules ()
    [_
      (type: _pointer
       expr: (let ([p (malloc _pointer)]) (ptr-set! p _pointer #f) p)
       post: (p => (cond [(ptr-ref p _gerror-pointer/null) => raise-gerror])))]))

