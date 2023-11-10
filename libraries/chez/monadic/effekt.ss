(define-syntax delayed
  (syntax-rules ()
    [(_ e)
      (lambda (mk) (underflow e mk))]))

; Control = MetaCont -> Step

; a -> Control a
(define (pure v)
  (lambda (mk) (underflow v mk)))

; Step a =  (a | Control x), MetaCont x a

; (a | Control a), MetaCont -> a
(define (trampoline c mk)
  (if (null? mk) c
      (call-with-values
        (lambda () (c mk))
        trampoline)))

; a, MetaCont -> Step
(define (underflow v mk)
  (if (null? mk) (values v mk)
    (let* ([stack (car mk)]
           [rest  (cdr mk)]
           [frames (Stack-frames stack)]
           [arena (Stack-arena stack)]
           [prompt (Stack-prompt stack)])
      (if (null? frames) (underflow v rest)
          (values ((car frames) v) (cons (make-Stack (cdr frames) arena prompt) rest))))))

; Control, Prompt -> Control
(define (reset p c)
  (lambda (mk)
    (c (cons (make-Stack '() (make-arena) p) mk))))

; Prompt, Body -> Control
(define (shift p f)
  (lambda (mk)
    (let-values ([(k mkrest) (splitSeq p mk)])
      (let ([cont (lambda (a) (lambda (mk) (underflow a (appendSeq k mk))))])
      (values (f cont) mkrest)))))

; Frame, MetaCont -> MetaCont
(define (push-frame f mk)
  (if (null? mk) (error 'push-frame "Cannot push frame, meta cont ~s is empty" mk)
    (let ([stack (car mk)]
          [rest  (cdr mk)])
      (let ([frames (Stack-frames stack)]
            [arena (Stack-arena stack)]
            [prompt (Stack-prompt stack)])
          (cons (make-Stack (cons f frames) arena prompt) rest)))))

(define-syntax then
  (syntax-rules ()
    [(_ m f)
     (lambda (k) (values m (push-frame f k)))]))

(define toplevel 0)

; Control a -> a
(define (run c)
  (trampoline c (cons (make-Stack '() (make-arena) toplevel) '())))

(define newPrompt (lambda () (string #\p)))

(define-syntax handle
  (syntax-rules ()
    [(_ ((cap1 (op1 (arg1 ...) kid exp) ...) ...) body)
     (let ([p (newPrompt)])
       (reset p (body
        (cap1 (define-effect-op p (arg1 ...) kid exp) ...)
        ...)))]))


(define-syntax define-effect-op
  (syntax-rules ()
    [(_ p (arg1 ...) kid exp ...)
     (lambda (arg1 ...)
        (shift p (lambda (kid) exp ...)))]))

; state(init) { cell => ... }
(define (state init body)
  (with-region (lambda (arena) (body (fresh arena init)))))