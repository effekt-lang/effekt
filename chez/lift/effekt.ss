(define-syntax thunk
  (syntax-rules ()
    [(_ e ...) (lambda () e ...)]))

(define-syntax delayed
  (syntax-rules ()
    [(_ e ...)
      (lambda (k) (k
        (begin e ...)))]))

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

(define (equal_impl obj1 obj2)
  (equal? obj1 obj2))

(define (println_impl obj)
  (delayed
    (display (show_impl obj))
    (newline)))

; (define-syntax pure
;   (syntax-rules ()
;     [(_ v)
;       (lambda (k) (k v))]))

(define (pure v)
  (lambda (k) (k v)))


; (then m a n) -> (lambda (k) (m (lambda (a) (n k))))
(define-syntax then
  (syntax-rules ()
    [(_ m a f)
     (lambda (k) (m (lambda (a) (f k))))]))

(define ($then m f)
  (lambda (k) (m (lambda (a) ((f a) k)))))

(define (here x) x)

(define (while cond exp)
  ($then cond (lambda (c)
    (if c ($then exp (lambda (_) (while cond exp))) (pure #f)))))

; (define-syntax lift
;   (syntax-rules ()
;     [(_ m)
;      (lambda (k1)
;        (lambda (k2)
;          (m (lambda (a) ((k1 a) k2)))))]))

(define (lift m)
  (lambda (k1)
    (lambda (k2)
      (m (lambda (a) ((k1 a) k2))))))

(define (id x) x)


; (define (reset m) (m (lambda (v) (lambda (k) (k v)))))

(define-syntax reset
  (syntax-rules ()
    [(_ m)
     (m (lambda (v) (lambda (k) (k v))))]))

(define (run m) (m id))



; ;; EXAMPLE
; ; (handle ([Fail_22 (Fail_109 () resume_120 (Nil_74))])
; ;       (let ((tmp86_121 ((Fail_109  Fail_22))))
; ;         (Cons_73  tmp86_121  (Nil_74))))


; capabilities first take evidence than require selection!
(define-syntax handle
  (syntax-rules ()
    [(_ ((cap1 (op1 (arg1 ...) kid exp ...) ...) ...) body)
     (reset ((body lift)
        (lambda (ev) (cap1 (define-effect-op ev (arg1 ...) kid exp ...) ...)
         ...)))]))


(define-syntax state
  (syntax-rules ()
    [(_ effid getid setid init body)
     ($then init (lambda (s)
        (define cell (box s))
        (define (cap ev) cell)

        (define (getid c) (lambda () (lambda (k) (k (unbox c)))))
        (define (setid c) (lambda (s*) (lambda (k) (set-box! c s*) (k #f))))

        (define (lift m) (lambda (k)
          (define backup (unbox cell))
          (m (lambda (a)
            (set-box! cell backup)
            (k a)))))

        ((body lift) cap)))]))


(define-syntax define-effect-op
  (syntax-rules ()
    [(_ ev1 (arg1 ...) kid exp ...)
     (lambda (arg1 ...)
        ; we apply the outer evidence to the body of the operation
        (ev1 (lambda (resume)
          ; k itself also gets evidence!
          (let ([kid (lambda (ev) (lambda (v) (ev (resume v))))])
            exp ...))))]))

(define-syntax nested-helper
  (syntax-rules ()
    [(_ (ev) acc) (ev acc)]
    [(_ (ev1 ev2 ...) acc)
      (nested-helper (ev2 ...) (ev1 acc))]))

(define-syntax nested
  (syntax-rules ()
    [(_ ev1 ...) (lambda (m) (nested-helper (ev1 ...) m))]))

; should also work for handlers / capabilities
(define (lift-block f ev)
  (lambda (ev2) (f (nested ev ev2))))


; (define (while_impl cond body)
;   (if (cond)
;     (begin
;       (body)
;       (while_impl cond))
;     #f))

; (define-syntax while
;   (syntax-rules ()
;     ((_ cond body)
;       (do () (cond) (loop)))))


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
