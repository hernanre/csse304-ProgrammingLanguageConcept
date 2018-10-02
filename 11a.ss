(load "chez-init.ss")
(define-datatype bintree bintree?
 (leaf-node
 (num integer?))
 (interior-node
 (key symbol?)
 (left-tree bintree?)
 (right-tree bintree?)))
;Linjie Zha A11a

;Problem1
;a
(define-syntax my-let
	(syntax-rules ()
		[(_ ([x e] ...) e1 e2 ...)
		((lambda (x ...)
			e1 e2 ...)
		e ...)]
		[(_ n ([x e] ...) e1 e2 ...)
		(letrec ([n 
					(lambda (x ...)
						e1 e2 ...)])
		(n e ...))]))

;b
(define-syntax my-or
	(syntax-rules ()
		[(_) #f]
		[(_ e) e]
		[(_ e1 e2 ...)
		(let ([x e1])
			(if x
				x
				(my-or e2 ...)))]))

;c
(define-syntax +=
	(syntax-rules ()
		[(_ x y)
		(begin (set! x (+ x y))
			   x)]))

;d
(define-syntax return-first
	(syntax-rules ()
		[(_ e1 e2 ...)
		(let ([first e1])
			(begin e2 ...)
			first)]))

;Problem2
(define bintree-to-list
	(lambda (t)
		(cases bintree t
			[leaf-node (datun)
			(list 'leaf-node datun)]
			[interior-node (key left right)
			(list 'interior-node key (bintree-to-list left) (bintree-to-list right))])))

;Problem3
(define max-interior
	(lambda (tree)
		(letrec ([helper 
					(lambda (tree)
						(cases bintree tree
      						[leaf-node (datum) datum]
      						[interior-node (key left right)
        					(let ([left-ls (helper left)]
              					  [right-ls (helper right)])
        					(cond
          						[(and (number? left-ls) (number? right-ls))
           						(list key (+ left-ls right-ls) key (+ left-ls right-ls))]
          						[(and (number? left-ls) (list? right-ls))
           						(let ([rkey (car right-ls)]
                 					  [r-max (cadr right-ls)]
                 					  [r-cur (cadddr right-ls)])
             					(if (> r-max (+ left-ls r-cur))
                 					(list rkey r-max key (+ left-ls r-cur))
                 					(list key (+ left-ls r-cur) key (+ left-ls r-cur))))]
          						[(and (number? right-ls) (list? left-ls))
           						(let ([lkey (car left-ls)]
                 					  [l-max (cadr left-ls)]
                 					  [l-cur (cadddr left-ls)])
             					(if (< l-max (+ right-ls l-cur))
                 					(list key (+ right-ls l-cur) key (+ right-ls l-cur))
                 					(list lkey l-max key (+ right-ls l-cur))))]
          						[else
            						(let ([lkey (car left-ls)]
                  						  [rkey (car right-ls)]
                  						  [cur-max (+ (cadddr left-ls) (cadddr right-ls))]
                  						  [l-max (cadr left-ls)]
                  						  [r-max (cadr right-ls)])
              						(if (>= l-max r-max)
                  						(if (> l-max cur-max)
                      						(list lkey l-max key cur-max)
                      						(list key cur-max key cur-max))
                  						(if (< r-max cur-max)
                      						(list key cur-max key cur-max)
                      						(list rkey r-max key cur-max))))]))]))])
		(car (helper tree)))))




(define-datatype expression expression?
  [var-exp
   (id symbol?)]
  [lit-exp
    (lit (lambda (x) #t))]
  [lambda-exp
   (id (lambda (v) (or (symbol? v) (list-of expression?))))
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
(define 4th cadddr)

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
         (eopl:error 'parse-exp "not a proper list: ~s" datum)]         
        [(eqv? (1st datum) 'lambda)
        (cond
          [(not (> (length datum) 2))
            (eopl:error 'parse-exp "incorrect length:" datum)]
          [(symbol? (2nd datum)) 
            (lambda-exp (parse-exp (2nd datum)) (map parse-exp (cddr datum)))]
          [(not (andmap symbol? (2nd datum)))
           (eopl:error 'parse-exp "invalid arguement " datum)]
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
        [(eqv? (1st datum) 'set!)
         (cond
           [(< (length datum) 3)
            (eopl:error 'parse-exp "missing expression: ~s" datum)]
           [(> (length datum) 3)
            (eopl:error 'parse-exp "too many parts: ~s" datum)]
           [else
             (set!-exp (2nd datum) (parse-exp (3rd datum)))])]
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
        (cons 'let (cons 
          (map list (map unparse-exp (map 1st vars)) (map unparse-exp (map 2nd vars)))
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