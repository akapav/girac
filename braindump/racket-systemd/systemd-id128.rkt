#lang racket

(require
  ffi/unsafe
  ffi/unsafe/define)

(define-ffi-definer define-sd-id128 (ffi-lib "libsystemd-id128" "0")
                    #:provide provide)

(define-cstruct _sd_id128 ([hi _uint64] [lo _uint64]))

(struct id-128 (raw)
  #:property prop:custom-write
  (lambda (id port mode)
    (display (string-append "#<id-128:" (id-128->string id) ">") port))
  #:property prop:equal+hash
  (let-syntax ([f (syntax-rules ()
                      [(_ ((a b) ...) f e ...)
                       (match-lambda** [((id-128 (sd_id128 a b)) ... f) e ...])])])
    (let ([eq (f ((hi1 lo1) (hi2 lo2)) _ (and (= hi1 hi2) (= lo1 lo2)))]
          [hash (f ((hi lo)) hash (hash (cons hi lo)))])
      (list eq hash hash))))

(provide _id128)

(define _id128 (make-ctype _sd_id128 id-128-raw id-128))


(define-sd-id128 id-128->string
                 (_fun _id128 (_bytes o 33) -> _string/latin-1)
                 #:c-id sd_id128_to_string)

(define-sd-id128 string->id-128
                 (_fun _string*/latin-1 (id : (_ptr o _id128))
                       -> (ret : _int) -> (ck-numeric/ret ret id 'string->sd_id128))
                 #:c-id sd_id128_from_string)

(define-syntax-rule (define-single-outparam name cname)
  (define-sd-id128 name (_fun (id : (_ptr o _id128))
                              -> (res : _int) -> (ck-numeric/ret res id 'name))
                   #:c-id cname))

(define-single-outparam boot-id sd_id128_get_boot)
(define-single-outparam machine-id sd_id128_get_machine)
(define-single-outparam random-id sd_id128_randomize)

(define-syntax-rule (ck-numeric/ret n res who)
  (if (< n 0) (error who "failure: ~a" n) res))

