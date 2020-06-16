(define (any matched)
  (lambda (failed)
    (lambda (x)
      (matched x))))

(define (ignore matched)
  (lambda (failed)
    (lambda (x)
      matched)))

(define (literal l)
  (lambda (matched)
    (lambda (failed)
      (lambda (x)
        (if (equal_impl x l) matched (failed))))))

(define (bind m)
  (lambda (matched)
    (lambda (failed)
      (lambda (x)
        (((m (lambda (y) ((matched x) y))) failed) x)))))


;; to be used like
;;   (define-matcher match-Pair Pair?
;;     ([p1 Pair-first]
;;      [p2 Pair-second]))
;;
;; will expand to
;;  (define (match-Pair p1 p2)
;;    (lambda (matched0)
;;      (lambda (failed)
;;        (lambda (x)
;;          ;; -- tag
;;          (if (Pair? x)
;;              ;; selectors
;;              (let* ([matched2 (((p2 matched0) failed) (Pair-second x))]
;;                     [matched1 (((p1 matched2) failed) (Pair-first x))])
;;                matched1)
;;
;;              ;; wrong tag
;;              (failed))))))
(define-syntax define-matcher
  (syntax-rules ()
    [(_ name pred ())
      (define (name)
        (lambda (matched)
          (lambda (failed)
            (lambda (sc)
              (if (pred sc) matched (failed))))))]
    [(_ name pred ((p1 sel1) (p2 sel2) ...))
     (define (name p1 p2 ...)
       (lambda (matched)
         (lambda (failed)
           (lambda (sc)
             ;; has correct tag?
             (if (pred sc)
                 (match-fields matched failed sc ((p1 sel1) (p2 sel2) ...))
                 (failed))))))]))

(define-syntax match-fields
  (syntax-rules ()
    [(_ matched failed sc ()) matched]
    [(_ matched failed sc ((p1 sel1) (p2 sel2) ...))
     (((p1 (match-fields matched failed sc ((p2 sel2) ...))) failed) (sel1 sc))]))

(define-syntax pattern-match
  (syntax-rules ()
    [(_ sc ()) (raise "match failed")]
    [(_ sc ((p1 k1) (p2 k2) ...))
     (((p1 k1) (lambda () (pattern-match sc ((p2 k2) ...)))) sc)]))