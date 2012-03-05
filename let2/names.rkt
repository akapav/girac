#lang racket

(provide lispify-string
         gir-class-name
         gir-method-name
         raw-function-name
         raw-type-name
         pointer-type-name
         pointer-type?)

(define (hyphenate str)
  (regexp-replace* #rx"_" str "-"))

(define (case-split str)
  (let* ((patt #px"[[:upper:]]+[[:lower:][:digit:]]+")
         (subs (regexp-match-positions* patt str)))
    (if (null? subs) (list str)
        (let ((end (substring str (cdr (last subs)) (string-length str)))
              (str-subs
               (foldl
                (let ((prev 0))
                  (lambda (sub acc)
                    (let* ((prev~ prev)
                           (from (car sub))
                           (to (cdr sub))
                           (substr (string-downcase(substring str from to))))
                      (set! prev to)
                      (if (= prev~ from)
                          (cons substr acc)
                          (cons substr
                                (cons (substring str prev~ from) acc))))))
                      '() subs)))
          (reverse (if (zero? (string-length end))
                       str-subs
                       (cons end str-subs)))))))

(define (join-hyphenate str-list)
  (let-values (((lst el) (split-at-right str-list 1)))
    (foldr (lambda (str acc)
             (if (or (regexp-match #px"-$" str)
                     (regexp-match #px"^-" acc))
                 (string-append str acc)
                 (string-append str "-" acc))) (car el) lst)))

(define (lispify-string str)
  (join-hyphenate (case-split (hyphenate str))))

(define (gir-class-name c:name)
  (string-append (lispify-string c:name) "%"))

(define (gir-method-name c:class:name c:mtd:name)
  (let* ((cn (string-downcase c:class:name))
         (patt (regexp (string-append cn "_(.*)")))
         (match (regexp-match patt c:mtd:name)))
    (if match (lispify-string (second match))
        (error "unexpected method name"))))

(define (raw-function-name c:name)
  (string-append "_" (lispify-string c:name)))

(define (pointer-type-name c:name)
  (let ((match (regexp-match #rx"(.*)\\*$" c:name)))
    (if match (second match) #f)))

(define (raw-type-name c:name)
  (let ((ptr-name (pointer-type-name c:name)))
    (if ptr-name 
        (string-append "_" ptr-name "-ptr")
        (string-append "_" c:name))))

(define pointer-type? pointer-type-name)
