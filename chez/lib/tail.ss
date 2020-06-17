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

(define (PushSeg/t k seq)
  (if (eqv? k base-k)
      seq
      (PushSeg k seq)))

(define ($pushPrompt p th)
  (call/cc (lambda (k)
    (set! mk (PushP p (PushSeg/t k mk)))
    (abort th))))

(define (withSubCont p f)
  (let-values ([(subk mk*) (splitSeq p mk)])
    (set! mk mk*)
    (call/cc (lambda (k)
      (abort (lambda () (f (PushSeg/t k subk))))))))

(define ($pushSubCont subk th)
  (call/cc (lambda (k)
    (set! mk (appendSeq subk (PushSeg/t k mk)))
    (abort th))))

(define newPrompt (lambda () (string #\p)))

(define (run th)
  (set! mk (EmptyS))
  (underflow
   (call/cc/base
    (lambda (k_1)
      (set! base-k k_1)
      ((call/cc (lambda (k_2)
         (set! abort k_2)
         (abort th))))))))

(define (underflow v)
  (Seq-case mk
    [(EmptyS) v]
    [(PushP _ mk*) (set! mk mk*) (underflow v)]
    [(PushSeg k mk*) (set! mk mk*) (k v)]))

(define (go)
  (new-cafe
    (lambda (x)
      (run (lambda () (eval x))))))
