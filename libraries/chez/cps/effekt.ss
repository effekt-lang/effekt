; Cont a = (a, MetaCont -> #)
; State = [box]
; Backup = [(box value)]
; StackCell = (Prompt {State | Backup} Cont)
; MetaCont = (Prompt State [StackCell])
; Prompt = Int
; Program a b = a, Cont b, MetaCont -> #

;BackupCell
(define-record-type backup-cell (fields box value))

; StackCell
(define-record-type stack-cell (fields prompt (mutable state) cont))

; MetaCont
; Holds the prompt and state of k in addition to the Metastack
(define-record-type meta-cont 
    (fields prompt (mutable state) stacks))

; Value, MetaCont -> Box
(define (var init ks)
    (let* ([state (meta-cont-state ks)]
           [var (box init)])
        (meta-cont-state-set! ks (cons var state))
        var))

; Box -> Value
(define (get ref) (unbox ref))

; Box, Value -> ()
(define (put ref value) (set-box! ref value))

; TODO
(define (create-region ks) (void))

; TODO
(define (allocate init region) (box init))

; TODO
(define (deallocate ref) (void))

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

; StackCell -> ()
(define (create-backup cell)
    (let* ([state (stack-cell-state cell)]
           [construct-cell (lambda (r) (make-backup-cell r (unbox r)))]
           [backup (map construct-cell state)])
        (stack-cell-state-set! cell backup)))

; [StackCell], Prompt -> ([StackCell] [StackCell])
(define (split-stack cs p)
    (define (worker above below)
        (let* ([current-cell (car below)]
               [_ (create-backup current-cell)]
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

; StackCell -> StackCell
(define (restore cell)
    (let ([backup (stack-cell-state cell)]
          [restore-cell (lambda (c) (let ([v-box (backup-cell-box c)])
                                          (set-box! v-box (backup-cell-value c))
                                          v-box))])
        ;Copy stack-cell here because one could resume multiple times
        (make-stack-cell (stack-cell-prompt cell) 
                         (map restore-cell backup) 
                         (stack-cell-cont cell))))

; [StackCell], [StackCell] -> [StackCell]
(define (rewind cont cells)
    (if (null? cont)
        cells
        (let ([first-cell (restore (car cont))])
             (rewind (cdr cont) (cons first-cell cells)))))

; [StackCell], Program [StackCell] b, MetaCont, Cont b -> #
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