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
                (ref-generation-set! r (diff-generation realDiff))
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
        (store-root-set!       store snapRoot)
        (store-generation-set! store (+ (snap-generation snap) 1))))

; Cont a = a, MetaCont -> #
; Prompt = Symbol

; MetaCont = Cont * Prompt * (Store | Snapshot) * (MetaCont | ThreadBoundary)?
; Holds a "copy" of k, as well as its prompt and store
(define-record-type meta-cont 
    (fields (mutable cont) prompt (mutable store) (mutable rest)))

; ThreadBoundary = Mutex * MetaCont
; A boundary that should be created when creating threads
; They can only be crossed when searching for effects,
; normal return terminates the thread
(define-record-type thread-boundary (fields mutex (mutable rest)))

; Block b, MetaCont, Cont -> b
(define (with-boundary prog ks k)
    (begin
        (set-meta-cont-cont! ks k)
        (p (make-meta-cont top-level-k
                           (gensym "thread")
                           (create-store)
                           (make-thread-boundary (make-mutex) ks))
            top-level-k)))

; Value, MetaCont -> Ref
(define (var init ks)
    (let ([store (meta-cont-store ks)])
        (make-ref store init (store-generation store))))

; Ref -> Value
(define (get ref) (ref-value ref))

; Ref, Value -> void
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

; Value, MetaCont -> Ref
(define allocate var)

; Ref | MetaCont -> void
(define (deallocate _) (void))

(define (top-level-k x _) x)
(define top-level-ks (make-meta-cont top-level-k (gensym "toplevel") (create-store) '()))

; a, MetaCont -> #
(define (return x ks)
    (let* ([new-ks (meta-cont-rest ks)]
           [k (meta-cont-cont new-ks)])
    (k x new-ks)))

; Program a b = a, Cont b, MetaCont -> #

; Program Prompt b, MetaCont, Cont b -> #
(define (reset prog ks k)
    (let ([prompt (gensym)])
         (meta-cont-cont-set! ks k)
         (prog prompt (make-meta-cont return prompt (create-store) ks) return)))

; MetaCont -> MetaCont
; Addresses potential thread boundaries
(define (get-meta-cont-rest ks)
    (if (meta-cont? ks)
        (let [rest (meta-cont-rest ks)]
            (if (meta-cont? rest)
                rest
                (thread-boundary-rest rest)))
        (begin
            (mutex-acquire (thread-boundary-mutex ks))
            (thread-boundary-rest ks))))

; MetaCont, Prompt -> MetaCont * MetaCont
(define (split-stack ks p)
    ; (MetaCont | ThreadBoundary), MetaCont -> MetaCont * MetaCont
    (define (worker captured remaining)
        (let* ([snap (snapshot (meta-cont-store remaining))]
               [remaining-rest (meta-cont-rest remaining)]
               [hit-boundary? (thread-boundary? remaining-rest)]
               [new-remaining (if hit-boundary?
                                  (thread-boundary-rest remaining-rest)
                                  remaining-rest)]
               [new-captured (begin
                                (meta-cont-store-set! remaining snap)
                                (meta-cont-rest-set! remaining captured)
                                (if hit-boundary?
                                    (let [(real-rest (thread-boundary-rest remaining-rest))]
                                        (thread-boundary-rest-set! remaining-rest remaining))
                                    remaining)
                                 )]
               [captured-prompt (meta-cont-prompt remaining)]
             (if (symbol=? captured-prompt p)
                 (values new-captured new-remaining)
                 (worker new-captured new-remaining))))
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
