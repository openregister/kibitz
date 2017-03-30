;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code to turn the alist into JSON.

(use medea)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Library code from medea
(use vector-lib srfi-14)
(define (for-each/delimiter proc list delimiter)
  (let ((size (if (vector? list)
                (vector-length list)
                (length list)))
        (count 0))
    ((if (vector? list) vector-for-each for-each)
     (lambda (d #!optional (ad d))
       (proc ad)
       (set! count (add1 count))
       (unless (= count size)
         (display #\,)))
     list)))
(define control-chars
  (ucs-range->char-set #x0 #x20))
(define json-escape-chars
  "\"\\bfnrt/")
(define json-escapes
  (map (lambda (e c)
         (cons (string e) (string #\\ c)))
       (string->list "\"\\\b\f\n\r\t")
       (string->list json-escape-chars)))
(define (display/escape-control-chars s)
  (let loop ((i 0))
    (let ((j (string-index s control-chars i)))
      (if j
        (begin
          (display (substring/shared s i j))
          (display "\\u")
          (display (string-pad (number->string (char->integer (string-ref s j)) 16) 4 #\0))
          (loop (+ j 1)))
        (display (substring/shared s i))))))
(define (unparse-string s)
  (display #\")
  (display/escape-control-chars (string-translate* s json-escapes))
  (display #\"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(json-unparsers
  (cons
    ; Serialise strings in alist cars. This results in a non-reversible encoding.
    (cons list?
          (lambda (object)
            (display #\{)
            (for-each/delimiter
              (lambda (member)
                (unparse-string
                  (cond
                    ((string? (car member))
                     (car member))
                    ((symbol? (car member))
                     (symbol->string (car member)))))
                (display #\:)
                (write-json (cdr member)))
              object
              #\,)
            (display #\})))
    (json-unparsers)))

(display
  (json->string
    (read)))

