#lang racket

(require
  "systemd-id128.rkt"
  racket/date
  ffi/unsafe
  ffi/unsafe/define
  ffi/unsafe/alloc
  ffi/unsafe/atomic)

(provide (except-out (all-defined-out)
                     ))
(provide get-data enumerate-data!)

(define-fun-syntax _transfer-out
  (syntax-rules (_string/utf-8)
    [(_ _bytes)
     (type: _pointer pre: (x => (let* ([s (bytes-length x)]
                                       [p (malloc _byte s 'raw)])
                                  (memcpy p x s) p)))]
    ))

(define-ffi-definer define-sd-journal
                    (ffi-lib "libsystemd-journal" "0")
                    )
;                     #:provide provide)

(define-syntax-rule (ck-lennart/ret code res who propagate ...)
  (if (or (>= code 0) (= code propagate) ...)
    res (error who "failure: ~a" (- code))))

(define-syntax define-sd-journal/ck
  (syntax-rules (:)
    [(_ name cname (inparam ...) (code #:include (inc ...) : result) e ...)
     (define-sd-journal name (_fun inparam ... -> (code : _int)
                                   -> (ck-lennart/ret code result 'name inc ...))
                        #:c-id cname e ...)]
    [(_ name cname params (code : result) e ...)
     (define-sd-journal/ck name cname params (code #:include () : result) e ...)]
    [(_ name cname params res e ...)
     (define-sd-journal/ck name cname params (unnamed : res) e ...)]))

(define-syntax define-sd-journal/ck/n
  (lambda (stx)
    (syntax-case stx ()
      [(_ name e ...)
       #`(define-sd-journal/ck name #,(c-name #'name) e ...)])))

(require (for-syntax ffi/unsafe))

(define-for-syntax (c-name name)
  (datum->syntax #'#f
    (string->symbol
      (regexp-replaces (syntax->datum name)
                       '((#px"^" "sd_journal_") (#px"-" "_")
                         (#px"[!*]*$" ""))))))

(define _usec
  (let ([e (lambda (xp n) (* n (expt 10 xp)))])
    (make-ctype _uint64 
      (lambda (d) (+ (e 6  (date->seconds d)) (e -3 (date*-nanosecond d))))
      (lambda (n) (seconds->date (e -6 n))))))

(define _size_t _ulong) ;; shit....

(define _journal (_cpointer 'journal))

(define _cursor (_cpointer 'cursor))

(define _data (_cpointer/null 'data))

(define _open_mask (_bitmask '(local = 1 runtime = 2 system = 4)))

(define-sd-journal close (_fun _journal -> _void)
                   #:c-id sd_journal_close
                   #:wrap (deallocator))

(define-sd-journal/ck/n open
  (([m '()]) :: (j : (_ptr o _journal)) (m : _open_mask)) j
  #:wrap (allocator close))

(define (if-positive n) (and (positive? n) n))

(define-sd-journal/ck/n next!  (_journal) (r : (if-positive r)))

(define-sd-journal/ck/n previous! (_journal) (r : (if-positive r)))

(define-sd-journal/ck/n next-skip! (_journal _uint64) (r : (if-positive r)))

(define-sd-journal/ck/n previous-skip! (_journal _uint64) (r : (if-positive r)))

(define-sd-journal/ck/n get-realtime-usec (_journal (d : (_ptr o _usec))) d)

(define-sd-journal/ck/n get-monotonic-usec
  (_journal (sec : (_ptr o _uint64)) (id : (_ptr o _id128)))
  (cons id sec))

(define-sd-journal/ck get-data/raw sd_journal_get_data
  (_journal (key : _bytes) (blob : (_ptr o _data)) (size : (_ptr o _size_t)))
  (res #:include (-2) :
       (case res
         [(-2) (raise-mismatch-error
                 'get-data "journal cursor contains no data for key: " key)]
         [else (make-sized-byte-string blob size)])))

(define (get-data j key [decode decode-message])
  (let* ([byte-key
           (cond [(symbol? key) (string->bytes/latin-1 (symbol->string key))]
                 [(string? key) (string->bytes/latin-1 key)]
                 [(bytes?  key) key]
                 [else (error 'get-data "Key: can be symbol, string or bytes, was: ~a" key)])]
         [key-len (bytes-length byte-key)]
         [blob (get-data/raw j byte-key)])
    (decode
      (cons key (subbytes blob (add1 key-len) (bytes-length blob))))))

(define-sd-journal restart-data! (_fun _journal -> _void) #:c-id sd_journal_restart_data)

(define-sd-journal/ck/n enumerate-data!*
  (_journal (blob : (_ptr o _data)) (size : (_ptr o _size_t)))
  (res : (and (positive? res)
              (make-sized-byte-string blob size))))

(define (enumerate-data! j [decode decode-message])
  (cond [(enumerate-data!* j)
         => (lambda (blob)
              (let ([kv (split-tag blob)])
                (cons (car kv) (decode kv))))]
         [else #f]))

; (define-sd-journal/ck/n add-match* (_journal _string*/utf-8) (void))
(define-sd-journal/ck/n add-match* (_journal (_transfer-out _bytes)) (void))

(define (add-match j key value)
  (add-match* j (string->bytes/utf-8
                  (string-append (symbol->string key) value))))

(define-sd-journal flush-matches (_fun _journal -> _void) #:c-id sd_journal_flush_matches)

(define-sd-journal/ck/n seek-head!  (_journal) (void))

(define-sd-journal/ck/n seek-tail!  (_journal) (void))

(define-sd-journal/ck/n seek-monotonic-usec!  (_journal _id128 _uint64) (void))

(define-sd-journal/ck/n seek-realtime-usec!  (_journal _usec) (void))

(define-sd-journal/ck/n seek-cursor!  (_journal _cursor) (void))

(define-sd-journal/ck/n get-cursor (_journal (crs : (_ptr o _cursor))) crs)

(define-sd-journal/ck/n get-fd (_journal) (res : res))

(define-sd-journal/ck/n process (_journal) (res : res)) ;; ???


(define (split-tag blob)
  (match (regexp-match-positions #rx#"=" blob)
    [(list (cons pos _))
     (cons (string->symbol (bytes->string/latin-1 blob #\_ 0 pos))
           (subbytes blob (add1 pos) (bytes-length blob)))]
    [#f (error 'extract-data-block "Data block missing `KEY=' : ~a"
               (bytes->string/latin-1 blob))]))

(define (decode-message kv)

  (define-values (key value) (values (car kv) (cdr kv)))

  (case key
    [(MESSAGE) (bytes->string/utf-8 value #\_)]
;     [(MESSAGE) value]

    [(_SOURCE_MONOTONIC_TIMESTAMP
       _SOURCE_REALTIME_TIMESTAMP
       _PID _UID _GID
       PRIORITY SYSLOG_FACILITY)
     (string->number (bytes->string/latin-1 value))]

    [(_TRANSPORT _HOSTNAME
       _COMM _EXE _CMDLINE            
       _SYSTEMD_CGROUP _SYSTEMD_UNIT
       SYSLOG_IDENTIFIER)
     (bytes->string/utf-8 value)]

    [(_BOOT_ID _MACHINE_ID)
     (string->id-128 value)]

    [else value]
    ))

(define (get-all-data! j [decode decode-message])
  (restart-data! j)
  (for/hash ([kv (in-producer (lambda ()
                                (enumerate-data! j decode))
                              not)])
    (values (car kv) (cdr kv))))



;; ...

(define (scan-journal! j [f void])
  (seek-head! j)
  (do ([c 0 (add1 c)] [m (next! j) (next! j)]) ((not m) c) (f)))

(define (scan-enumerate-all! j [f void])
  (scan-journal!  j
    (lambda () (let loop ()
                 (cond [(enumerate-data!* j)
                        => (lambda (b) (f b) (loop))])))))

(define (scan-enumerate-all!/decode j [f void] #:decode [decode decode-message])
  (scan-journal! j
    (lambda () (let loop ()
                 (cond [(enumerate-data! j decode)
                        => (lambda (b) (f b) (loop))])))))

(define (scan-enumerate-all!/hash j [f void] #:decode [decode decode-message])
  (scan-journal! j (lambda () (f (get-all-data! j decode)))))

(define (scan-some! j [keys '(MESSAGE)] #:decode [decode decode-message])
  (scan-journal! j
    (lambda () (for ([k (in-list keys)]) (get-data j k decode)))))

(define-syntax n-timeit
  (syntax-rules ()
    [(_ (times) e ...)
     (time (let lawp ([n times])
             (unless (zero? n)
               e ... (lawp (sub1 n)))))]
    [(_ e ...) (n-timeit (1) e ...)]))

(define derp (make-bytes 16192 65))

(define (ck-reset)
  (define j (open))
  (next-skip! j 4)
  (displayln (get-all-data! j))
  (define mark (get-realtime-usec j))
  (next-skip! j 10000)
  (displayln (seek-realtime-usec! j mark))
  (next! j)
  (displayln (get-all-data! j))
  )

(define (death j)
  (define x 0)
  (scan-enumerate-all!/hash
    j
    (lambda (h) (displayln x) (set! x (add1 x)))
    #:decode cdr))

(define (add-match-die j)

  (define (amn m)
    (add-match* j m)
    (printf "added ~a~n" m)
    (flush-output))

  (amn #"_TRANSPORT=kernel")
  (amn #"_XTRANSPORT=kernel")
  (amn #"_TRANSPORT=kernelx")
  (amn #"_TRANSPORT=kernelxy")
  (amn #"_HERP=derp")

  (displayln (next! j))
  (collect-garbage)
  (displayln "pre-flush")
  (flush-matches)
  (displayln "post-flush")
  (collect-garbage)
  (displayln (next! j))
  )

