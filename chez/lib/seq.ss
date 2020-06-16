;;; seq.ss
;;; Copyright (c) 2005, R. Kent Dybvig, Simon L. Peyton Jones, and Amr Sabry

;;; sequence (Seq) datatype

(define-datatype Seq
  (EmptyS)
  (PushP p Seq)
  (PushSeg k Seq))

(define (splitSeq p seq)
  (Seq-case seq
    [(EmptyS) (error 'splitSeq "prompt ~s not found on stack" p)]
    [(PushP p* sk)
     (if (not (eq? p p*))
         (let-values ([(subk sk*) (splitSeq p sk)])
           (values (PushP p* subk) sk*))
         (values (EmptyS) sk))]
    [(PushSeg k sk)
     (let-values ([(subk sk*) (splitSeq p sk)])
       (values (PushSeg k subk) sk*))]))

(define (appendSeq seq_1 seq_2)
  (Seq-case seq_1
    [(EmptyS) seq_2]
    [(PushP p subk) (PushP p (appendSeq subk seq_2))]
    [(PushSeg k subk) (PushSeg k (appendSeq subk seq_2))]))
