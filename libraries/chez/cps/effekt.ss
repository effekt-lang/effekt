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

; Store | #f -> Snapshot | #f
(define (snapshot store)
    (and store ; no need to snapshot if the store is empty
        (let* ([sGen (store-generation store)]
               [snap (make-snap store (store-root store) sGen)])
            (store-generation-set! store (+ sGen 1))
            snap)))

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

; MetaCont = Cont * Prompt * (Store | #f) | (Snapshot | #f) * MetaCont?
; Holds a "copy" of k, as well as its prompt and store.
; A segment's store is #f when created, populated only by its first variable.
(define-record-type meta-cont 
    (fields (mutable cont) prompt (mutable store) (mutable rest)))

; MetaCont -> Store
(define (segment-store ks)
    (or (meta-cont-store ks)           ; if there is a store, use it
        (let ([store (create-store)])  ; otherwise, create it
            (meta-cont-store-set! ks store)
            store)))

; Value, MetaCont -> Ref
(define (var init ks)
    (let ([store (segment-store ks)])
        (make-ref store init (store-generation store))))

; Ref -> Value
(define (get ref) (ref-value ref))

; Ref, Value -> void
(define-syntax put
  (syntax-rules ()
    [(_ ref-expr value-expr)
     (let ([ref ref-expr] [value value-expr])
       (let ([store (ref-store ref)])
         (if (= (ref-generation ref) (store-generation store))
             (ref-value-set! ref value)
             (put-recording ref value store))))]))

; Ref, Value, Store -> void
(define (put-recording ref value store)
    (let ([rGen (ref-generation ref)]
          [sGen (store-generation store)]
          [oldVal (ref-value ref)]
          [newRoot (box (make-mem))]
          [oldRoot (store-root store)])
        (ref-value-set! ref value)
        (ref-generation-set! ref sGen)
        (set-box! oldRoot (make-diff ref oldVal rGen newRoot))
        (store-root-set! store newRoot)))

; MetaCont -> Store
(define (create-region ks) (segment-store ks))

; Value, Store -> Ref
(define (allocate init store) (make-ref store init (store-generation store)))

; Ref | Store -> void
(define (deallocate _) (void))

(define (top-level-k x _) x)
(define top-level-ks (make-meta-cont top-level-k (gensym "toplevel") #f '()))

; Program a b = a, Cont b, MetaCont -> #

; Program Prompt b, (b, MetaCont -> #), MetaCont, Cont b -> #
(define (reset prog ret ks k)
    (let ([prompt (gensym)])
         (meta-cont-cont-set! ks k)
         (prog prompt (make-meta-cont ret prompt #f ks) ret)))

; MetaCont, Prompt -> MetaCont * MetaCont
(define (split-stack ks p)
    (define (worker above below)
        (let ([new-below (meta-cont-rest below)]
              [snap (snapshot (meta-cont-store below))])
             (meta-cont-store-set! below snap)
             (meta-cont-rest-set! below above)
             (if (symbol=? (meta-cont-prompt below) p)
                 (values below new-below)
                 (worker below new-below))))
    (worker '() ks))

; Prompt, Program MetaCont b, MetaCont, Cont b -> #
(define-syntax shift
  (syntax-rules ()
    [(_ p-expr prog-expr ks-expr k-expr)
     (let ([p p-expr] [prog prog-expr] [ks ks-expr] [k k-expr])
       (meta-cont-cont-set! ks k)
       (let-values ([(c underC) (split-stack ks p)])
         (prog c underC (meta-cont-cont underC))))]))

; MetaCont, MetaCont -> MetaCont
(define (rewind cont ks)
    (if (null? cont)
        ks
        (let* ([snap (meta-cont-store cont)]
               [next (meta-cont-rest cont)]
               [newKs (make-meta-cont (meta-cont-cont cont)
                                      (meta-cont-prompt cont)
                                      (and snap (snap-store snap))
                                      ks)])
            (when snap (restore snap))
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
