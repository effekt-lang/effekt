(define (reset body)
  (let ([p (newPrompt)])
    (pushPrompt p (body p))))

(define (shift p body)
  (shift0-at p k (body k)))

;(define-syntax define-effect-op
;  (syntax-rules ()
;    [(_ P (arg1 ...) k exp ...)
;     (lambda (arg1 ...)
;       (shift0-at P k exp ...))]))

; state(init) { cell => ... }
(define (state init body)
  (with-region (lambda (arena) (body (fresh arena init)))))

(define call/cc/base call/cc)
