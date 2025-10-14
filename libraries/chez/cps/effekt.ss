; Cont a = (a, MetaCont -> #)
; State = [box]
; StackCell = (Prompt Cont State)
; MetaCont = (Prompt State [StackCell])
; Prompt = Int
; Program a b = a, Cont, MetaCont -> b

; StackCell
(define-record-type stack-cell (fields prompt state cont))

; MetaCont
; Holds the prompt and state of k in addition to the Metastack
(define-record-type meta-cont 
    (fields prompt (mutable state) stacks))

(define (var init ks)
    (let* ([state (meta-cont-state ks)]
           [var (box init)])
        (meta-cont-state-set! ks (cons var state))
        var))

(define (get ref) (unbox ref))

(define (put ref value) (set-box! ref value))

(define _prompt 1)
(define (new-prompt)
    (let ([prompt _prompt])
        (set! _prompt (+ prompt 1))
        prompt))

(define top-level-ks (make-meta-cont 0 '() '()))
(define (top-level-k x _) x)

; a, MetaCont -> #
(define (return x ks)
    (let* ([stacks (meta-cont-stacks ks)]
           [current-stack-cell (car stacks)]
           [other-stack-cells (cdr stacks)]
           [k (stack-cell-cont current-stack-cell)]
           [ks (make-meta-cont (stack-cell-prompt current-stack-cell) 
                               (stack-cell-state current-stack-cell)
                               other-stack-cells)])
    (k x ks)))
    

; Program Prompt b, MetaCont, Cont b -> #
(define (reset prog ks k)
    (let* ([prompt (new-prompt)]
           [k-prompt (meta-cont-prompt ks)]
           [k-state (meta-cont-state ks)]
           [k-cell (make-stack-cell k-prompt k-state k)]
           [rest-cells (meta-cont-stacks ks)]
           [ks (make-meta-cont prompt '() (cons k-cell rest-cells))])
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
    (let* ([k-cell (make-stack-cell (meta-cont-prompt ks) (meta-cont-state ks) k)]
           [cells (cons k-cell (meta-cont-stacks ks))]
           [splitted (split-stack cells p)]
           [c (car splitted)]
           [underC (cdr splitted)]
           [new-k-cell (car underC)]
           [new-k-state (stack-cell-state new-k-cell)]
           [new-k-prompt (stack-cell-prompt new-k-cell)]
           [k (stack-cell-cont new-k-cell)]
           [ks (make-meta-cont new-k-prompt new-k-state (cdr underC))])
        (prog c ks k)))

; [StackCell], [StackCell] -> [StackCell]
(define (rewind cont cells)
    (if (null? cont)
        cells
        (rewind (cdr cont) (cons (car cont) cells))))

; [StackCell], Program, MetaCont, Cont -> #
(define (resume cont block ks k)
    (let* ([cells (cons (make-stack-cell (meta-cont-prompt ks) (meta-cont-state ks) k) (meta-cont-stacks ks))]
           [rewinded (rewind cont cells)]
           [k-cell (car rewinded)]
           [k (stack-cell-cont k-cell)]
           [k-prompt (stack-cell-prompt k-cell)]
           [k-state (stack-cell-state k-cell)]
           [ks (make-meta-cont k-prompt k-state (cdr rewinded))])
        (block ks k)))

; Program a -> a
(define (run-top-level p)
    (p top-level-ks top-level-k))