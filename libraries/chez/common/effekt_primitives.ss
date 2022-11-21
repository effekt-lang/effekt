(define (show_impl obj)
  (cond
    [(number? obj) (show-number obj)]
    [(string? obj) obj]
    [(boolean? obj) (if obj "true" "false")]
    ; [(record? obj)
    ;   (let* ([rtd (record-rtd obj)]
    ;          [name (symbol->string (record-type-name rtd))])
    ;     ;; how can we show the fields?
    ;     (string-append name))]
    [(list? obj) (map show_impl obj)]
    [(record? obj) (show-record obj)]
    [else (generic-show obj)]))

(define (generic-show obj)
  (define out (open-output-string))
  (write obj out)
  (get-output-string out))

; conform with the JS way of printing numbers
(define (show-number n)
  (if (integer? n) (number->string (exact n)) (number->string n)))

; here we use eval to find the show function defined with the record...
; (define (show-record rec)
;   (let* ([rtd (record-rtd rec)]
;          [showName (string-append "show" (generic-show (record-type-name rtd)))]
;          [showFun (eval (string->symbol showName))])
;   (showFun rec)))

; we needed to add a unique id to the types in order to prevent duplicate definitions
; now for printing, we need to strip the unique id (starting with $) again.
(define (strip-type-name tpe)
  (define out "")
  (define found #f)
  (for-each
    (lambda (el)
      (if (char=? el #\$) (set! found #t) #f)
      (if found #f (set! out (string-append out (string el)))))
    (string->list tpe))
  out)

(define (show-record rec)
  (let* ([rtd (record-rtd rec)]
         [unique-tpe (generic-show (record-type-name rtd))]
         [tpe (strip-type-name unique-tpe)]
         [fields (record-type-field-names rtd)]
         [n (vector-length fields)])
     (define out (string-append tpe "("))
     (do ([i 0 (+ i 1)])
         ((= i n))
       (set! out (string-append out (show_impl ((record-accessor rtd i) rec))))
       (if (< i (- n 1)) (set! out (string-append out ", "))))
     (set! out (string-append out ")"))
     out))



(define (println_impl obj)
  (display (show_impl obj))
  (newline))

(define (equal_impl obj1 obj2)
  (equal? obj1 obj2))

(define-syntax thunk
  (syntax-rules ()
    [(_ e ...) (lambda () e ...)]))

;; Benchmarking utils

; time in milliseconds
(define (timed block)
  (let ([before (current-time)])
    (block)
    (let ([after (current-time)])
      (seconds (time-difference after before)))))

(define (seconds diff)
  (+ (time-second diff) (/ (time-nanosecond diff) 1000000000.0)))

(define (measure block warmup iterations)
  (define (run n)
    (if (<= n 0)
        '()
        (begin
          (collect)
          (cons (timed block) (run (- n 1))))))
  (begin
    (run warmup)
    (run iterations)))

(define (hole)
  (raise
    (condition
      (make-error)
      (make-message-condition "not implemented"))))