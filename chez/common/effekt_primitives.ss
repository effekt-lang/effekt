(define (show_impl obj)
  (cond
    [(number? obj) (number->string obj)]
    [(string? obj) obj]
    [(boolean? obj) (if obj "true" "false")]
    ; [(record? obj)
    ;   (let* ([rtd (record-rtd obj)]
    ;          [name (symbol->string (record-type-name rtd))])
    ;     ;; how can we show the fields?
    ;     (string-append name))]
    [(list? obj) (map show_impl obj)]
    [else (generic-show obj)]))

(define (generic-show obj)
  (define out (open-output-string))
  (write obj out)
  (get-output-string out))

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
