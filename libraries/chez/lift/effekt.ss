(define-syntax delayed
  (syntax-rules ()
    [(_ e ...)
      (lambda (k) (k
        (begin e ...)))]))

(define (pure v)
  (lambda (k) (k v)))


; (then m a n) -> (lambda (k) (m (lambda (a) (n k))))
(define-syntax then
  (syntax-rules ()
    [(_ m a f1 ...)
     (lambda (k) (m (lambda (a) ((let () f1 ...) k))))]))

(define ($then m f)
  (lambda (k) (m (lambda (a) ((f a) k)))))

(define (here x) x)

; (define (while cond exp)
;   ($then cond (lambda (c)
;     (if c ($then exp (lambda (_) (while cond exp))) (pure #f)))))

(define-syntax while
  (syntax-rules ()
    [(_ c e)
     (let ([condition (lambda () c)])
       (letrec ([loop (lambda (u)
         ($then (condition) (lambda (condValue)
           (if condValue ($then e loop) (pure #f)))))])
         (loop #f)))]))

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
    [(_ ((cap1 (op1 (arg1 ...) kid exp) ...) ...) body)
     (reset ((body lift)
        (lambda (ev) (cap1 (define-effect-op ev (arg1 ...) kid exp) ...)) ...))]))


(define (with-region body)
  (define arena (make-arena))

  (define (lift m) (lambda (k)
    ; on suspend
    (define fields (backup arena))
    (m (lambda (a)
      ; on resume
      (restore fields)
      (k a)))))

  ((body lift) arena))


; An Arena is a pointer to a list of cells
(define (make-arena) (box '()))

(define (fresh arena init)
  (let* ([cell (box init)]
         [cells (unbox arena)])
    (set-box! arena (cons cell cells))
    cell))

; Backup = List<(Cell, Value)>

; Arena -> Backup
(define (backup arena)
  (let ([fields (unbox arena)])
    (map (lambda (cell) (cons cell (unbox cell))) fields)))

; Backup -> ()
(define (restore data)
  (for-each (lambda (cell-data)
    (set-box! (car cell-data) (cdr cell-data)))
    data))


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