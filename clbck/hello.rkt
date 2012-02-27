#lang racket

(require ffi/unsafe)

(define (rc-hash-add! hash key)
  (hash-set! hash key (add1 (hash-ref hash key 0))))

(define (rc-hash-del! hash key)
  (let ((cnt (hash-ref hash key 0)))
    (if (zero? cnt)
	(hash-remove! hash key)
	(hash-set! hash key (sub1 cnt)))))

(define *callbacks* (make-hash))

(define libgobject (ffi-lib "/usr/lib/libgobject-2.0"))
(define libgtk (ffi-lib "/usr/lib/libgtk-3"))
(define libhello (ffi-lib "./hello"))

;(define _callback-fn (_fun -> void))

(define _callback-notify-fn (_fun #:keep (lambda (c) (rc-hash-add! *callbacks* c))
				  _pointer
				  _pointer -> _void))

(define _delete-event-fn (_fun _pointer
			       _pointer
			       _pointer -> ( ret : _int)
			       -> (if ret 1 0)))

(define _gclosure-new
  (get-ffi-obj "g_cclosure_new" libgobject (_fun _pointer
						 _pointer
						 _callback-notify-fn -> _pointer)))

(define run (get-ffi-obj "run" libhello (_fun _pointer -> _void)))

(define (callback fn type)
  (rc-hash-add! *callbacks* fn)
  (_gclosure-new
   (cast fn type _pointer)
   #f ;user data is ignored
   (lambda _
     (printf "delete callback: ~a~%" _)
     (rc-hash-del! *callbacks* fn))))

(run (callback
      (let ((cnt 3))
	(lambda (a b c)
	  (begin0
	      (not (zero? cnt))
	    (printf "~A~%" cnt)
	    (collect-garbage)
	    (set! cnt (sub1 cnt)))))
      _delete-event-fn))
