(load "chez-init.ss") ; put this file in the same folder, or add a pathname

; This is a parser for simple Scheme expressions, 
; such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

(define-datatype expression expression?
  [var-exp
   (id symbol?)]
  [lit-exp
    (lit (lambda (x) #t))]
  [lambda-exp
   (id (lambda (x) (or (symbol? x) (list-of expression?))))
   (body (list-of expression?))]
  [let-exp
    (vars (list-of (list-of expression?)))
    (body (list-of expression?))]
  [letrec-exp
    (vars (list-of (list-of expression?)))
    (body (list-of expression?))]
  [let*-exp
    (vars (list-of (list-of expression?)))
    (body (list-of expression?))]
  [set!-exp
    (id symbol?)
    (body expression?)]
  [if-exp
    (test expression?)
    (result expression?)]
  [if-else-exp
    (test expression?)
    (thencase expression?)
    (elsecase expression?)]
  [app-exp
   (rator expression?)
   (rand (list-of expression?))])


; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr);added

(define parse-exp         
  (lambda (datum)
    (cond
      [(symbol? datum) (var-exp datum)]
      [(number? datum) (lit-exp datum)]
      [(string? datum) (lit-exp datum)]
      [(vector? datum) (lit-exp datum)]
      [(boolean? datum) (lit-exp datum)]
      [(pair? datum)
      (cond
        [(not (list? datum))
         (eopl:error 'parse-exp "Error in parse-exp: lambda expression: not a proper list:" datum)]         
        [(eqv? (1st datum) 'lambda)
        (cond
          [(not (> (length datum) 2))
            (eopl:error 'parse-exp "Error in parse-exp: incorrect length:" datum)]
          [(symbol? (2nd datum)) 
            (lambda-exp (parse-exp (2nd datum)) (map parse-exp (cddr datum)))]
          [(not (andmap symbol? (2nd datum)))
           (eopl:error 'parse-exp "invalid arguement" datum)]
          [else 
            (lambda-exp (map parse-exp (2nd  datum)) (map parse-exp (cddr datum)))])]
          [(eqv? (1st datum) 'let)
          (cond
            [(< (length datum) 3)
            (eopl:error 'parse-exp "incorrect length: ~s" datum)]
            [(not (list? (2nd datum)))
            (eopl:error 'parse-exp "not all proper list: ~s" (2nd datum))]
            [(not (andmap list? (2nd datum)))
            (eopl:error 'parse-exp "not a proper list: ~s" (2nd datum))]
            [(not (andmap (lambda (x) (= 2 (length x))) (2nd datum)))
            (eopl:error 'parse-exp "not all length 2: ~s" (2nd datum))]
            [(not (andmap symbol? (map 1st (2nd datum))))
            (eopl:error 'parse-exp "first members must be symbols: ~s" datum)]
            [else
             (let-exp (map list (map parse-exp (map car (2nd datum))) (map parse-exp (map 2nd (2nd datum)))) (map parse-exp (cddr datum)))])]
          [(eqv? (1st datum) 'letrec)
           (cond
            [(< (length datum) 3)
            (eopl:error 'parse-exp "incorrect length: ~s" datum)]
            [(not (list? (2nd datum)))
            (eopl:error 'parse-exp "not all proper list: ~s" (2nd datum))]
            [(not (andmap list? (2nd datum)))
            (eopl:error 'parse-exp "not a proper list: ~s" (2nd datum))]
            [(not (andmap (lambda (x) (= 2 (length x))) (2nd datum)))
            (eopl:error 'parse-exp "not all length 2: ~s" (2nd datum))]
            [(not (andmap symbol? (map 1st (2nd datum))))
            (eopl:error 'parse-exp "first members must be symbols: ~s" datum)]
            [else
             (letrec-exp (map list (map parse-exp (map car (2nd datum))) (map parse-exp (map 2nd (2nd datum)))) (map parse-exp (cddr datum)))])]
          [(eqv? (1st datum) 'let*)
          (cond
            [(< (length datum) 3)
            (eopl:error 'parse-exp "incorrect length: ~s" datum)]
            [(not (list? (2nd datum)))
            (eopl:error 'parse-exp "not all proper list: ~s" (2nd datum))]
            [(not (andmap list? (2nd datum)))
            (eopl:error 'parse-exp "not a proper list: ~s" (2nd datum))]
            [(not (andmap (lambda (x) (= 2 (length x))) (2nd datum)))
            (eopl:error 'parse-exp "not all length 2: ~s" (2nd datum))]
            [(not (andmap symbol? (map 1st (2nd datum))))
            (eopl:error 'parse-exp "first members must be symbols: ~s" datum)]
            [else
             (let*-exp (map list (map parse-exp (map car (2nd datum))) (map parse-exp (map 2nd (2nd datum)))) (map parse-exp (cddr datum)))])]
          [(eqv? (1st datum) 'if)
          (cond
            [(= 2 (length datum))
            (eopl:error 'parse-exp "missing then or else clauses: ~s" datum)]
            [(= 3 (length datum))
            (if-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)))]
            [(> (length datum) 4)
            (eopl:error 'parse-exp "too many parts: ~s" datum)]
            [else
            (if-else-exp (parse-exp (2nd datum))
              (parse-exp (3rd datum))
              (parse-exp (4th datum)))])]
          [(eqv? (1st datum) 'set!)
          (cond
            [(< (length datum) 3)
            (eopl:error 'parse-exp "missing expression: ~s" datum)]
            [(> (length datum) 3)
            (eopl:error 'parse-exp "too many parts: ~s" datum)]
            [else
             (set!-exp (2nd datum) (parse-exp (3rd datum)))])]
        
          [else (app-exp (parse-exp (1st datum))
          (map parse-exp (cdr datum)))])]
      [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))


(define unparse-exp
  (lambda (exp)
    (cases expression exp
      [var-exp (id) id]
      [lit-exp (id) id]
      [lambda-exp (id body)
        (cond
          [(null? id)
            (cons 'lambda (cons (map unparse-exp id) (map unparse-exp body)))]
          [(symbol? (car id))
            (cons 'lambda (cons (unparse-exp id) (map unparse-exp body)))]
          [else
            (cons 'lambda (cons (map unparse-exp id) (map unparse-exp body)))])]
      [let-exp (vars body)
        (cons 'let 
              (cons (map list (map unparse-exp (map 1st vars)) (map unparse-exp (map 2nd vars)))
                    (map unparse-exp body)))]
      [letrec-exp (vars body)
        (cons 'letrec (cons (map list (map unparse-exp (map 1st vars)) (map unparse-exp (map 2nd vars))) (map unparse-exp body)))]
      [let*-exp (vars body)
        (cons 'let* (cons (map list (map unparse-exp (map 1st vars)) (map unparse-exp (map 2nd vars))) (map unparse-exp body)))]
      [set!-exp (id body)
        (list 'set! (unparse-exp id) (unparse-exp body))]
      [if-exp (test result)
        (list 'if (unparse-exp test) (unparse-exp result))]
      [if-else-exp (test thencase elsecase)
        (list 'if (unparse-exp test) (unparse-exp thencase) (unparse-exp elsecase))]
      [app-exp (rator rand)
        (cons (unparse-exp rator)
          (map unparse-exp rand))])))