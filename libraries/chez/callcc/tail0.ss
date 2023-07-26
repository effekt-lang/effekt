;;; tail.ss
;;; Copyright (c) 2005, R. Kent Dybvig, Simon L. Peyton Jones, and Amr Sabry

;;; Tail-recursive implementation of the operators

;(case-sensitive #t)

(define-syntax pushPrompt
  (syntax-rules ()
    [(_ p e1 e2 ...)
     ($pushPrompt p (lambda () e1 e2 ...))]))

(define-syntax pushSubCont
  (syntax-rules ()
    [(_ subk e1 e2 ...)
     ($pushSubCont subk (lambda () e1 e2 ...))]))

(define abort)
(define mk)
(define base-k)

(define ($pushPrompt p th)
  (call/cc (lambda (k)
    (pushFrame k)
    (set! mk (cons (make-Stack (list) (make-arena) p) mk))
    (abort th))))

(define (with-region body)
  (let* ([stack (car mk)]
         [arena (Stack-arena stack)])
    (body arena)))

(define (withSubCont p f)
  (call/cc (lambda (k)
    (pushFrame k)
    (let-values ([(subk mk*) (splitSeq p mk)])
      (set! mk mk*)
      (abort (lambda () (f subk)))))))

(define (pushFrame f)
  (if (eqv? f base-k) (values)
    (if (null? mk) (error "ERROR! Cannot push on empty meta cont" #f)
        (let* ([stack (car mk)]
              [frames (Stack-frames stack)]
              [arena (Stack-arena stack)]
              [prompt (Stack-prompt stack)]
              [rest  (cdr mk)])
          (set! mk (cons (make-Stack (cons f frames) arena prompt) rest))))))

(define ($pushSubCont subk th)
  (call/cc (lambda (k)
    (pushFrame k)
    (set! mk (appendSeq subk mk))
    (abort th))))

(define newPrompt (lambda () (string #\p)))

(define (run th)
  (define global (make-Stack '() (make-arena) (newPrompt)))
  (set! mk (list global))
  (underflow
   (call/cc
    (lambda (k_1)
      (set! base-k k_1)
      ((call/cc (lambda (k_2)
         (set! abort k_2)
         (abort th))))))))

(define (underflow v)
  (if (null? mk) v
    (let* ([stack (car mk)]
           [frames (Stack-frames stack)]
           [arena (Stack-arena stack)]
           [prompt (Stack-prompt stack)]
           [rest  (cdr mk)])
      (if (null? frames) (begin (set! mk rest) (underflow v))
          (begin
            (set! mk (cons (make-Stack (cdr frames) arena prompt) rest))
            ((car frames) v))))))


(define-syntax shift0-at
  (syntax-rules ()
    [(_ P id exp ...)
     (withSubCont
      P
      (lambda (s)
         (let ([id (lambda (v) (pushSubCont s v))])
           exp ...)))]))