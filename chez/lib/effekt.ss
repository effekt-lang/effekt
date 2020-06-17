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
    [else obj]))

(define (println_impl obj)
  (display (show_impl obj))
  (newline))

(define-syntax while
  (syntax-rules ()
    [(_ c exp1 exp2 ...)
     (letrec ([loop (lambda ()
       (if c (begin exp1 exp2 ... (loop)) #f))])
       (loop))]))

(define (equal_impl obj1 obj2)
  (equal? obj1 obj2))

; ;; EXAMPLE
; ; (handle ([Fail_22 (Fail_109 () resume_120 (Nil_74))])
; ;       (let ((tmp86_121 ((Fail_109  Fail_22))))
; ;         (Cons_73  tmp86_121  (Nil_74))))
; (define-syntax handle
;   (syntax-rules ()
;     [(_ ((cap1 (op1 (arg1 ...) k exp ...) ...) ...) body ...)
;      (let ([P (newPrompt)])
;         (pushPrompt P
;           (let ([cap1 (cap1 (define-effect-op P (arg1 ...) k exp ...) ...)] ...)
;             body ...)))]))

(define-syntax handle
  (syntax-rules ()
    [(_ ((cap1 (op1 (arg1 ...) k exp ...) ...) ...) body)
     (let ([P (newPrompt)])
        (pushPrompt P
          (body (cap1 (define-effect-op P (arg1 ...) k exp ...) ...) ...)))]))

(define-syntax define-effect-op
  (syntax-rules ()
    [(_ P (arg1 ...) k exp ...)
     (lambda (arg1 ...)
       (shift0-at P k exp ...))]))

(define-syntax thunk
  (syntax-rules ()
    [(_ e ...) (lambda () e ...)]))

(define call/cc/base call/cc)

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
