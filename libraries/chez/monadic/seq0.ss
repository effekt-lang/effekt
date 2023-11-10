; [ [f1, f2, f3, f4| fields | prompt], [f1, f2, f3, f4| fields | prompt]... ]
(define-record Stack (
  [immutable frames]
  [immutable arena]
  [immutable prompt]))

(define-record Substack (
  [immutable frames]
  [immutable arena]
  [immutable field-backup]
  [immutable prompt]))

; MetaCont = (non-empty-list-of Stack)
; SubCont = (non-empty-list-of Stack)


; An Arena is a pointer to a list of cells
(define (make-arena) (box '()))

(define (fresh arena init)
  (let* ([cell (box init)]
         [cells (unbox arena)])
    (set-box! arena (cons cell cells))
    cell))

; with-region { arena => ... }
(define (with-region body)
  (lambda (mk)
    (let* ([stack (car mk)]
           [arena (Stack-arena stack)])
      ((body arena) mk))))

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

; Prompt MetaCont -> SubCont, MetaCont
(define (splitSeq p seq)
  (define (split sub s)
    (if (null? s)
        (error 'splitSeq "prompt ~s not found on stack" p)
        (let* ([stack (car s)]
           [frames (Stack-frames stack)]
           [arena (Stack-arena stack)]
           [prompt (Stack-prompt stack)]
           [rest  (cdr s)])
          (define sub* (cons (make-Substack frames arena (backup arena) prompt) sub))
          (if (not (eq? prompt p))
            (split sub* rest)
            (values sub* rest)))))
  (split (list) seq))


(define (appendSeq subcont meta)
  (if (null? subcont)
    meta
    (let* ([stack (car subcont)]
           [frames  (Substack-frames stack)]
           [arena  (Substack-arena stack)]
           [fields (Substack-field-backup stack)]
           [prompt (Substack-prompt stack)]
           [_ (restore fields)]
           [rest  (cdr subcont)])
      (appendSeq rest (cons (make-Stack frames arena prompt) meta)))))

; (define ex (list (make-Stack '(1 2 3) (list (box 1)) 4) (make-Stack '(1 2 3) (list (box 2)) 3) (make-Stack '(1 2 3) (list (box 3)) 2) (make-Stack '(4 5 6) (list (box 4)) 1)))
; (let-values ([(cont meta) (splitSeq 2 ex)]) (appendSeq cont meta))
