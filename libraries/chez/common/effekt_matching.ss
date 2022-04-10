; Matcher = (SCRUTINEE, ANS -> () -> R, () -> R, (ANS -> () -> R) -> R) -> R

(define done (lambda (matched) (matched)))

(define (any m matched failed k)
  (k (matched m)))

(define (ignore m matched failed k)
  (k matched))

(define (literal l)
  (lambda (m matched failed k)
    (if (equal_impl m l)
        (k matched)
        (failed))))

(define (bind p)
  (lambda (m matched failed k)
    (p m (matched m) failed k)))

;; for this record
; (define-record Pair (fst snd))

;; this is what we need to generate
; (define (match-Pair p1 p2)
;   (lambda (m matched failed k)
;     (if (Pair? m)
;       (p1 (Pair-fst m) matched failed (lambda (matched)
;         (p2 (Pair-snd m) matched failed k)))
;       (failed))))


(define-syntax define-matcher
  (syntax-rules ()
    [(_ name pred ())
      (define (name)
        (lambda (sc matched failed k)
          (if (pred sc) (k matched) (failed))))]
    [(_ name pred ((p1 sel1) (p2 sel2) ...))
     (define (name p1 p2 ...)
       (lambda (m matched failed k)
             ;; has correct tag?
             (if (pred m)
                 (match-fields m matched failed k ([p1 sel1] [p2 sel2] ...))
                 (failed))))]))

(define-syntax match-fields
  (syntax-rules ()
    [(_ m matched failed k ()) (k matched)]
    [(_ m matched failed k ([p1 sel1] [p2 sel2] ...))
     (p1 (sel1 m) matched failed (lambda (matched)
       (match-fields m matched failed k ([p2 sel2] ...))))]))


; forces the pattern match
(define-syntax pattern-match
  (syntax-rules ()
    [(_ m ()) (raise "no patterns provided")]
    [(_ m ([p1 k1] [p2 k2]...))
      (p1 m k1 (lambda () (pattern-match m ([p2 k2] ...))) done)]))

;; Examples

; (define-matcher match-Pair2 Pair? ([p1 Pair-fst] [p2 Pair-snd]))

; (define (match sc p matched)
;   (p sc matched abort done))

; (display (match (make-Pair 1 2)
;   (match-Pair2 any any)
;   (lambda (x) (lambda (y) (lambda () (+ x y))))))

; (display (match 3
;   (match-Pair2 any any)
;   (lambda (x) (lambda (y) (lambda () (+ x y))))))

; (display (match (make-Pair 1 2)
;   (match-Pair2 any (match-Pair2 ignore any))
;   (lambda (x) (lambda (y) (lambda () (+ x y))))))

; (display (match (make-Pair 1 (make-Pair 2 3))
;   (match-Pair2 any (match-Pair2 ignore any))
;   (lambda (x) (lambda (y) (lambda () (+ x y))))))

; (display (match (make-Pair (make-Pair 1 2) (make-Pair 3 4))
;   (match-Pair (match-Pair ignore any) (match-Pair ignore any))
;   (lambda (x) (lambda (y) (lambda () (+ x y))))))

; (display (match (make-Pair (make-Pair 1 2) (make-Pair 10 15))
;   (match-Pair2 (match-Pair2 any ignore) (match-Pair2 ignore any))
;   (lambda (x) (lambda (y) (lambda () (+ x y))))))

; (display (match (make-Pair (make-Pair 1 2) (make-Pair 10 15))
;   (match-Pair2 (match-Pair2 any ignore) (match-Pair2 ignore any))
;   (lambda (x) (lambda (y) (lambda () (+ x y))))))

; (display (pattern-match (make-Pair 1 2)
;   ([(match-Pair2 (match-Pair2 any ignore) (match-Pair2 ignore any))
;       (lambda (x) (lambda (y) (lambda () (+ x y))))]
;    [any (lambda (x) (lambda () (Pair-snd x)))])))
