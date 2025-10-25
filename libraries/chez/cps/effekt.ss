; Cont a = (a, MetaCont -> #)
; State = [box]
; Backup = [(box value)]
; MetaCont = (Cont Prompt {State | Backup} rest)
; Prompt = Int
; Block b = Cont b, MetaCont -> #
; Program a b = a, Cont b, MetaCont -> #

;BackupCell
(define-record-type backup-cell (fields box value))

; MetaCont
; Holds a "copy" of k, as well as its prompt and state
(define-record-type meta-cont 
    (fields (mutable cont) prompt (mutable state) (mutable rest)))

; Value, MetaCont -> Box
(define (var init ks)
    (let* ([state (meta-cont-state ks)]
           [var (box init)])
          (meta-cont-state-set! ks (cons var state))
          var))

; Box -> Value
(define (get ref) (unbox ref))

; Box, Value -> void
(define (put ref value) (set-box! ref value))

; MetaCont -> void
(define (create-region _) (void))

; Value, Region -> Box
(define (allocate init _) (box init))

; Ref -> void
(define (deallocate _) (void))

(define _prompt 1)
(define (new-prompt)
    (let ([prompt _prompt])
         (set! _prompt (+ prompt 1))
         prompt))

(define (top-level-k x _) x)
(define top-level-ks (make-meta-cont top-level-k 0 '() '()))

; a, MetaCont -> #
(define (return x ks)
    (let* ([new-ks (meta-cont-rest ks)]
           [k (meta-cont-cont new-ks)])
    (k x new-ks)))

; Program Prompt b, MetaCont, Cont b -> #
(define (reset prog ks k)
    (let ([prompt (new-prompt)])
         (meta-cont-cont-set! ks k)
         (prog prompt (make-meta-cont return prompt '() ks) return)))

; MetaCont -> void
(define (create-backup ks)
    (let* ([state (meta-cont-state ks)]
           [construct-cell (lambda (r) (make-backup-cell r (unbox r)))]
           [backup (map construct-cell state)])
        (meta-cont-state-set! ks backup)))

; MetaCont, Prompt -> (MetaCont MetaCont)
(define (split-stack ks p)
    (define (worker above below)
        (let ([new-below (meta-cont-rest below)])
             (create-backup below)
             (meta-cont-rest-set! below above)
             (if (= (meta-cont-prompt below) p)
                 (values below new-below)
                 (worker below new-below))))
    (worker '() ks))

; Prompt, Program MetaCont b, MetaCont, Cont b -> #
(define (shift p prog ks k)
    (meta-cont-cont-set! ks k)
    (let-values ([(c underC) (split-stack ks p)])
                (prog c underC (meta-cont-cont underC))))

; MetaCont, MetaCont -> MetaCont
(define (restore cont rest)
    (let ([backup (meta-cont-state cont)]
          [restore-cell (lambda (c) (let ([v-box (backup-cell-box c)])
                                         (set-box! v-box (backup-cell-value c))
                                         v-box))])
         ;Copy here because one could resume multiple times
         (make-meta-cont (meta-cont-cont cont)
                         (meta-cont-prompt cont) 
                         (map restore-cell backup) 
                         rest)))

; MetaCont, MetaCont -> MetaCont
(define (rewind cont ks)
    (if (null? cont)
        ks
        (rewind (meta-cont-rest cont) (restore cont ks))))

; MetaCont, Block, MetaCont, Cont -> #
(define (resume cont block ks k)
    (meta-cont-cont-set! ks k)
    (let ([rewinded (rewind cont ks)])
         (block rewinded (meta-cont-cont rewinded))))

; Block b -> b
(define (run-top-level p)
    (p top-level-ks top-level-k))