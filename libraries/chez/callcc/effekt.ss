
(define-syntax while
  (syntax-rules ()
    [(_ c exp1 exp2 ...)
     (letrec ([loop (lambda ()
       (if c (begin exp1 exp2 ... (loop)) #f))])
       (loop))]))

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

(define call/cc/base call/cc)
