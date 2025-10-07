; Cont a = (a, MetaCont -> #)
; StackCell = (Prompt Cont)
; MetaCont = (Prompt [StackCell])
; Prompt = Int
; Program a b = a, Cont, MetaCont -> b

; StackCell
(define-record-type stack-cell (fields prompt cont))

; MetaCont
(define-record-type meta-cont (fields prompt rest))

(define _prompt 1)
(define (new-prompt)
    (let ([prompt _prompt])
        (set! _prompt (+ prompt 1))
        prompt))

(define top-level-ks (make-meta-cont 0 '()))
(define (top-level-k x _) x)

; a, MetaCont -> #
(define (return x ks)
    (let* ([stacks (meta-cont-rest ks)]
           [current-stack-cell (car stacks)]
           [other-stack-cells (cdr stacks)]
           [k (stack-cell-cont current-stack-cell)]
           [ks (make-meta-cont (stack-cell-prompt current-stack-cell) other-stack-cells)])
    (k x ks)))
    

; Program Prompt b, MetaCont, Cont b -> #
(define (reset prog ks k)
    (let* ([prompt (new-prompt)]
           [k-prompt (meta-cont-prompt ks)]
           [k-cell (make-stack-cell k-prompt k)]
           [rest-cells (meta-cont-rest ks)]
           [ks (make-meta-cont prompt (cons k-cell rest-cells))])
        (prog prompt ks return)))

; [StackCell], Prompt -> ([StackCell] [StackCell])
(define (split-stack cs p)
    (define (worker above below)
        (let* ([current-cell (car below)]
               [new-above (cons current-cell above)]
               [new-below (cdr below)])
            (if (= (stack-cell-prompt current-cell) p)
                (cons new-above new-below)
                (worker new-above new-below))))
    (worker '() cs))

; Prompt, Program [StackCell] b, MetaCont, Cont b -> #
(define (shift p prog ks k)
    (let* ([cells (cons (make-stack-cell (meta-cont-prompt ks) k) (meta-cont-rest ks))]
           [splitted (split-stack cells p)]
           [c (car splitted)]
           [underC (cdr splitted)]
           [k-cell (car underC)]
           [k-prompt (stack-cell-prompt k-cell)]
           [k (stack-cell-cont k-cell)]
           [ks (make-meta-cont k-prompt (cdr underC))])
        (prog c ks k)))

; [StackCell], [StackCell] -> [StackCell]
(define (rewind cont cells)
    (if (null? cont)
        cells
        (rewind (cdr cont) (cons (car cont) cells))))

; [StackCell], Program, MetaCont, Cont -> #
(define (resume cont block ks k)
    (let* ([cells (cons (make-stack-cell (meta-cont-prompt ks) k) (meta-cont-rest ks))]
           [rewinded (rewind cont cells)]
           [k-cell (car rewinded)]
           [k (stack-cell-cont k-cell)]
           [k-prompt (stack-cell-prompt k-cell)]
           [ks (make-meta-cont k-prompt (cdr rewinded))])
        (block ks k)))

; Program a -> a
(define (run-top-level p)
    (p top-level-ks top-level-k))