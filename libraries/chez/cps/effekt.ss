; Value = Any
; Generation = Int

; Ref = Store * Value * Generation 
(define-record-type ref (fields store (mutable value) (mutable generation)))

; Node = box MEM | box Diff

; MEM
; Means the correct values are in MEMory
(define-record-type mem (fields))

; Diff = Ref * Value * Generation * Node
; Notes the DIFFerences between root and the current state in MEMory
(define-record-type diff (fields ref value generation root))

; Store = Node * Generation
(define-record-type store (fields (mutable root) (mutable generation)))

; Snapshot = Store * Node * Generation
(define-record-type snap (fields store root generation))

; -> Store
(define (create-store) (make-store (box (make-mem)) 0))

; Store -> Snapshot
(define (snapshot store)
    (let* ([sGen (store-generation store)]
           [snap (make-snap store (store-root store) sGen)])
        (store-generation-set! store (+ sGen 1))
        snap))

; Node, [box Diff] -> [box Diff]
(define (collectDiffs n acc)
    (let ([unboxedN (unbox n)])
        (cond
            [(mem? unboxedN) acc]
            [else (collectDiffs (diff-root unboxedN) (cons n acc))])))

; Node, [Diff] -> void
(define (applyDiffs n diffs)
    (cond
        [(null? diffs) (set-box! n (make-mem))]
        [else
            (let* ([currentDiff (car diffs)]
                   [realDiff (unbox currentDiff)]
                   [r (diff-ref realDiff)]
                   [oldValue (ref-value r)])
                (ref-value-set! r (diff-value realDiff))
                (set-box! n (make-diff r oldValue (ref-generation r) currentDiff))
                (applyDiffs currentDiff (cdr diffs)))]))

; Node, Node -> void
(define (reroot newRoot oldRoot)
    (applyDiffs oldRoot (collectDiffs newRoot '())))

; Store, Snapshot -> void
(define (restore snap)
    (let* ([snapRoot (snap-root snap)]
           [store (snap-store snap)]
           [storeRoot (store-root store)])
        (reroot snapRoot storeRoot)
        (store-root-set! store snapRoot)))

; Cont a = a, MetaCont -> #
; Prompt = Int

; MetaCont = Cont * Prompt * Store | Snapshot * MetaCont?
; Holds a "copy" of k, as well as its prompt and store
(define-record-type meta-cont 
    (fields (mutable cont) prompt (mutable store) (mutable rest)))

; Value, MetaCont -> Ref
(define (var init ks)
    (let ([store (meta-cont-store ks)])
        (make-ref store init (store-generation store))))

; Box -> Value
(define (get ref) (ref-value ref))

; Box, Value -> void
(define (put ref value)
    (let* ([rGen (ref-generation ref)]
           [store (ref-store ref)]
           [sGen (store-generation store)])
        (if (= rGen sGen)
            (ref-value-set! ref value)
            (let ([oldVal (ref-value ref)]
                  [newRoot (box (make-mem))]
                  [oldRoot (store-root store)])
                (ref-value-set! ref value)
                (ref-generation-set! ref sGen)
                (set-box! oldRoot (make-diff ref oldVal rGen newRoot))
                (store-root-set! store newRoot)))))

; MetaCont -> MetaCont
(define (create-region ks) ks)

; Value, MetaCont -> Box
(define allocate var)

; Ref | MetaCont -> void
(define (deallocate _) (void))

(define _prompt 1)
(define (new-prompt)
    (let ([prompt _prompt])
         (set! _prompt (+ prompt 1))
         prompt))

(define (top-level-k x _) x)
(define top-level-ks (make-meta-cont top-level-k 0 (create-store) '()))

; a, MetaCont -> #
(define (return x ks)
    (let* ([new-ks (meta-cont-rest ks)]
           [k (meta-cont-cont new-ks)])
    (k x new-ks)))

; Program a b = a, Cont b, MetaCont -> #

; Program Prompt b, MetaCont, Cont b -> #
(define (reset prog ks k)
    (let ([prompt (new-prompt)])
         (meta-cont-cont-set! ks k)
         (prog prompt (make-meta-cont return prompt (create-store) ks) return)))

; MetaCont, Prompt -> MetaCont * MetaCont
(define (split-stack ks p)
    (define (worker above below)
        (let ([new-below (meta-cont-rest below)]
              [snap (snapshot (meta-cont-store below))])
             (meta-cont-store-set! below snap)
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
(define (rewind cont ks)
    (if (null? cont)
        ks
        (let* ([snap (meta-cont-store cont)]
               [next (meta-cont-rest cont)]
               [newKs (make-meta-cont (meta-cont-cont cont)
                                      (meta-cont-prompt cont)
                                      (snap-store snap)
                                      ks)])
            (restore snap)
            (rewind next newKs))))

; Block b = Cont b, MetaCont -> #

; MetaCont, Block, MetaCont, Cont -> #
(define (resume cont block ks k)
    (meta-cont-cont-set! ks k)
    (let ([rewinded (rewind cont ks)])
         (block rewinded (meta-cont-cont rewinded))))

; Block b -> b
(define (run-top-level p)
    (p top-level-ks top-level-k))