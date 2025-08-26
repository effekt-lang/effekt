(define (show_impl obj)
  (cond
    [(number? obj) (show-number obj)]
    [(string? obj) obj]
    [(boolean? obj) (if obj "true" "false")]
    [(char? obj) (show-char obj)]
    ; [(record? obj)
    ;   (let* ([rtd (record-rtd obj)]
    ;          [name (symbol->string (record-type-name rtd))])
    ;     ;; how can we show the fields?
    ;     (string-append name))]
    [(list? obj) (map show_impl obj)]
    [(record? obj) (show-record obj)]
    [(eqv? obj (void)) "()"]
    [else (generic-show obj)]))

(define (generic-show obj)
  (define out (open-output-string))
  (write obj out)
  (get-output-string out))

; conform with the JS way of printing numbers
(define (show-number n)
  (if (integer? n) (number->string (exact n)) (number->string n)))

(define (show-char c)
  (string c))

; here we use eval to find the show function defined with the record...
; (define (show-record rec)
;   (let* ([rtd (record-rtd rec)]
;          [showName (string-append "show" (generic-show (record-type-name rtd)))]
;          [showFun (eval (string->symbol showName))])
;   (showFun rec)))

; we needed to add a unique id to the types in order to prevent duplicate definitions
; now for printing, we need to strip the unique id (starting with $) again.
(define (strip-type-name tpe)
  (define out "")
  (define found #f)
  (for-each
    (lambda (el)
      (if (char=? el #\$) (set! found #t) #f)
      (if found #f (set! out (string-append out (string el)))))
    (string->list tpe))
  out)

(define (show-record rec)
  (let* ([rtd (record-rtd rec)]
         [unique-tpe (generic-show (record-type-name rtd))]
         [tpe (strip-type-name unique-tpe)]
         [fields (record-type-field-names rtd)]
         [n (vector-length fields)])
     (define out (string-append tpe "("))
     (do ([i 0 (+ i 1)])
         ((= i n))
       (set! out (string-append out (show_impl ((record-accessor rtd i) rec))))
       (if (< i (- n 1)) (set! out (string-append out ", "))))
     (set! out (string-append out ")"))
     out))



(define (println_impl str)
  (display str)
  (newline))

; Custom structural equality that properly handles records
(define (equal_impl obj1 obj2)
  (cond
    ; Same object reference (fast path)
    [(eq? obj1 obj2) #t]
    
    ; If both are records, compare them structurally
    [(and (record? obj1) (record? obj2))
     (let* ([rtd1 (record-rtd obj1)]
            [rtd2 (record-rtd obj2)])
       ; Check if same record type
       (if (eq? rtd1 rtd2)
           (let* ([n (vector-length (record-type-field-names rtd1))]
                  [result #t])
             ; Compare all fields recursively
             (do ([i 0 (+ i 1)])
                 ((or (= i n) (not result)) result)
               (let ([field1 ((record-accessor rtd1 i) obj1)]
                     [field2 ((record-accessor rtd2 i) obj2)])
                 (if (not (equal_impl field1 field2))
                     (set! result #f)))))
           #f))]
    
    ; For lists, compare elements recursively
    [(and (list? obj1) (list? obj2))
     (and (= (length obj1) (length obj2))
          (let loop ([l1 obj1] [l2 obj2])
            (or (null? l1)
                (and (equal_impl (car l1) (car l2))
                     (loop (cdr l1) (cdr l2))))))]
    
    ; For all other types, use Scheme's built-in equal?
    [else (equal? obj1 obj2)]))

(define-syntax thunk
  (syntax-rules ()
    [(_ e ...) (lambda () e ...)]))

;; Benchmarking utils

; time in milliseconds
(define (timed block)
  (let ([before (current-time)])
    (block)
    (let ([after (current-time)])
      (seconds (time-difference after before)))))

(define (seconds diff)
  (+ (time-second diff) (/ (time-nanosecond diff) 1000000000.0)))

(define (timestamp)
  (let ([t (current-time)])
    (+ (* (time-second t) 1000000000) (time-nanosecond t))))

(define (measure block warmup iterations)
  (define (run n)
    (if (<= n 0)
        '()
        (begin
          (collect)
          (cons (timed block) (run (- n 1))))))
  (begin
    (run warmup)
    (run iterations)))

(define (hole pos)
  (raise
    (condition
      (make-error)
      (make-message-condition (string-append pos " not implemented yet" )))))