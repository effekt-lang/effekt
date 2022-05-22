;;; datatype.ss
;;; Copyright (c) 2005, R. Kent Dybvig, Simon L. Peyton Jones, and Amr Sabry

;;; Code for defining datatypes, based on record subtyping.

#|

Description
-----------

A define-datatype form creates a new datatype with zero or more variants,
each with its own set of fields.  It produces definitions for a predicate,
constructors, and a case form.

Syntax:

  <datatype definition> -> (define-datatype <datatype name> <variant>*)

  <datatype name> -> identifier

  <variant> -> (<variant name> <field name>*)
  <variant name> -> identifier
  <field name> -> identifier

Products:
  The definition

    (define-datatype dtname (vname vfield ...))

  produces the following set of variable definitions:

    dtname?       a predicate true only of datatype instances
    variant ...   a set of constructors for the different variants
    dtname-case   a case construct for the datatype

  Each constructor accepts one argument for each field of the variant.
  The case construct has the following syntax:

    (dtname-case <expression> <clause> ...)

  where each <clause> is of the form:

    [(<variant name> <field name>*) <expression>+]

  except that the last must be an else clause of the form

    [else <expression>+]

  if any of the datatype's variants are not represented in the set of
  clauses.  The clauses may appear in any order and the field names
  need not be the same as those given in the datatype definition.
  (They are instead specified positionally.)
  
Example:

  (case-sensitive #t)

  (define-datatype AST
    (Const datum)
    (Ref var)
    (If tst thn els)
    (Fun fmls body)
    (Call exp0 args))

  (define (parse x)
    (cond
      [(symbol? x) (Ref x)]
      [(pair? x)
       (case (car x)
         [(if) (If (parse (cadr x)) (parse (caddr x)) (parse (cadddr x)))]
         [(fun) (Fun (cadr x) (parse (caddr x)))]
         [else (Call (parse (car x)) (map parse (cdr x)))])]
      [else (Const x)]))
    
  (define (ev x r)
    (AST-case x
      [(Ref v) (cdr (assq v r))]
      [(Const x) x]
      [(If tst thn els) (ev (if (ev tst r) thn els) r)]
      [(Fun fmls body)
       (lambda args
         (ev body (append (map cons fmls args) r)))]
      [(Call proc actuals)
        (apply (ev proc r) (map (lambda (x) (ev x r)) actuals))]))
  
  (AST? (parse #'(fun (x) x)))

  (define (run x) (ev (parse x) `((- . ,-) (* . ,*) (= . ,=))))

  (run '((fun (f) (f f 10))
         (fun (f x)
           (if (= x 0)
               1
               (* x (f f (- x 1))))))) ;=> 3628800

|#

(define-syntax define-datatype
  (lambda (x)
    (define construct-name
      (lambda (template-identifier . args)
        (datum->syntax-object
          template-identifier
          (string->symbol
            (apply string-append
                   (map (lambda (x)
                          (if (string? x)
                              x
                              (symbol->string (syntax-object->datum x))))
                        args))))))
    (define iota
      (case-lambda
        [(n) (iota 0 n)]
        [(i n) (if (= n 0) '() (cons i (iota (+ i 1) (- n 1))))]))
    (syntax-case x ()
      [(_ dtname (vname field ...) ...)
       (and (identifier? #'dtname) (andmap identifier? (syntax->list #'(vname ...))))
       (with-syntax ([dtname? (construct-name #'dtname #'dtname "?")]
                     [dtname-case (construct-name #'dtname #'dtname "-case")]
                     [dtname-variant (construct-name #'dtname #'dtname "-variant")]
                     [((vname-field ...) ...)
                      (map (lambda (vname fields)
                             (map (lambda (field)
                                    (construct-name #'dtname
                                      vname "-" field))
                                  fields))
                           (syntax->list #'(vname ...))
                           (map syntax->list (syntax->list #'((field ...) ...))))]
                     [(make-vname ...)
                      (map (lambda (x)
                             (construct-name #'dtname
                               "make-" x))
                           (syntax->list #'(vname ...)))]
                     [(i ...) (iota (length (syntax->list #'(vname ...))))])
         #'(module (dtname? (dtname-case dtname-variant vname-field ... ...) vname ...)
             (define-record dtname ((immutable variant)))
             (module (make-vname vname-field ...)
               (define-record vname dtname ((immutable field) ...) ()))
             ...
             (define-syntax dtname-case
               (lambda (x)
                 (define make-clause
                   (lambda (x)
                     (syntax-case x (vname ...)
                       [(vname (field ...) e1 e2 (... ...))
                        #'((i) (let ([field (vname-field t)] ...)
                                 e1 e2 (... ...)))]
                       ...)))
                 (syntax-case x (else)
                   [(__ e0
                        ((v fld (... ...)) e1 e2 (... ...))
                        (... ...)
                        (else e3 e4 (... ...)))
                    (with-syntax ([(clause (... ...))
                                   (map make-clause
                                        #'((v (fld (... ...)) e1 e2 (... ...))
                                           (... ...)))])
                      #'(let ([t e0])
                          (case (dtname-variant t)
                            clause
                            (... ...)
                            (else e3 e4 (... ...)))))]
                   [(__ e0
                        ((v fld (... ...)) e1 e2 (... ...))
                        (... ...))
                    (let f ([ls1 (list #'vname ...)])
                      (or (null? ls1)
                          (and (let g ([ls2 (syntax->list #'(v (... ...)))])
                                 (if (null? ls2)
                                     (syntax-error x
                                       (format "unhandled `~s' variant in"
                                         (syntax-object->datum (car ls1))))
                                     (or (literal-identifier=? (car ls1) (car ls2))
                                         (g (cdr ls2)))))
                               (f (cdr ls1)))))
                    (with-syntax ([(clause (... ...))
                                   (map make-clause
                                        (syntax->list
                                         #'((v (fld (... ...)) e1 e2 (... ...))
                                            (... ...))))])
                      #'(let ([t e0])
                          (case (dtname-variant t)
                            clause
                            (... ...))))])))
             (define vname
               (lambda (field ...)
                 (make-vname i field ...)))
             ...))])))
