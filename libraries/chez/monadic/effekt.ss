(define-syntax delayed
  (syntax-rules ()
    [(_ e)
      (lambda (mk) (underflow mk e))]))

; Control = MetaCont -> Step

; a ... -> Control a ...
(define pure
  (case-lambda
    [(v) (lambda (mk) (underflow mk v))]
    [vs (lambda (mk) (apply underflow mk vs))]))

; Step a =  (a | Control x), MetaCont x a

; (a | Control a), MetaCont -> a
(define (trampoline c mk)
  (if (null? mk) c
      (call-with-values
        (lambda () (c mk))
        trampoline)))

; MetaCont -> (Frame | #f), MetaCont
; Pops the topmost frame, or returns #f if the meta continuation is exhausted.
(define (pop-frame mk)
  (if (null? mk) (values #f mk)
    (let* ([stack (car mk)]
           [rest  (cdr mk)]
           [frames (Stack-frames stack)])
      (if (null? frames) (pop-frame rest)
          (values (car frames) (cons (make-Stack (cdr frames) (Stack-arena stack) (Stack-prompt stack)) rest))))))

; MetaCont, a ... -> Step
; The single-value case is kept separate, since it does not allocate.
(define underflow
  (case-lambda
    [(mk v)
      (let-values ([(frame rest) (pop-frame mk)])
        (if frame (values (frame v) rest) (values v rest)))]
    [(mk . vs)
      (let-values ([(frame rest) (pop-frame mk)])
        (if frame (values (apply frame vs) rest)
            (error 'underflow "cannot return ~s values to the toplevel" (length vs))))]))

; Control -> Control
(define (reset c)
  (let ([p (newPrompt)])
    (lambda (mk)
      ((c p) (cons (make-Stack '() (make-arena) p) mk)))))

; Prompt, Body -> Control
(define (shift p f)
  (lambda (mk)
    (let-values ([(k mkrest) (splitSeq p mk)])
      (let ([cont (case-lambda
                    [(a) (lambda (mk) (underflow (appendSeq k mk) a))]
                    [as (lambda (mk) (apply underflow (appendSeq k mk) as))])])
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

(define-syntax define-effect-op
  (syntax-rules ()
    [(_ p (arg1 ...) kid exp ...)
     (lambda (arg1 ...)
        (shift p (lambda (kid) exp ...)))]))

; state(init) { cell => ... }
(define (state init body)
  (with-region (lambda (arena) (body (fresh arena init)))))