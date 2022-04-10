; [ [f1, f2, f3, f4| fields | prompt], [f1, f2, f3, f4| fields | prompt]... ]
(define-record Stack (
  [immutable frames]
  [immutable fields]
  [immutable prompt]))

; MetaCont = (non-empty-list-of Stack)
; SubCont = (non-empty-list-of Stack)

(define (backup fields)
  (map (lambda (ref) (cons ref (unbox ref))) fields))

(define (restore fields)
  (map (lambda (backupref)
    (set-box! (car backupref) (cdr backupref))
    (car backupref)) fields))

; Prompt MetaCont -> SubCont, MetaCont
(define (splitSeq p seq)
  (define (split sub s)
    (if (null? s)
        (error 'splitSeq "prompt ~s not found on stack" p)
        (let* ([stack (car s)]
           [prompt (Stack-prompt stack)]
           [frames (Stack-frames stack)]
           [fields (Stack-fields stack)]
           [rest  (cdr s)])
          (define sub* (cons (make-Stack frames (backup fields) prompt) sub))
          (if (not (eq? prompt p))
            (split sub* rest)
            (values sub* rest)))))
  (split (list) seq))


(define (appendSeq subcont meta)
  (if (null? subcont)
    meta
    (let* ([stack (car subcont)]
           [prompt (Stack-prompt stack)]
           [frames (Stack-frames stack)]
           [fields (Stack-fields stack)]
           [rest  (cdr subcont)])
      (appendSeq rest (cons (make-Stack frames (restore fields) prompt) meta)))))

; (define ex (list (make-Stack '(1 2 3) (list (box 1)) 4) (make-Stack '(1 2 3) (list (box 2)) 3) (make-Stack '(1 2 3) (list (box 3)) 2) (make-Stack '(4 5 6) (list (box 4)) 1)))
; (let-values ([(cont meta) (splitSeq 2 ex)]) (appendSeq cont meta))
