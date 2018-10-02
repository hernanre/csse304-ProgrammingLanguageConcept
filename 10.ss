;Linjie Zha A10

;Problem1
;every element will only occur once and will remain the first occurence
(define delete-repeat
	(lambda (ls)
		(cond
			[(null? ls) '()]
			[(member (car ls) (cdr ls))
			(delete-repeat (cdr ls))]
			[else
			(cons (car ls) (delete-repeat (cdr ls)))])))
;recursion when it's a lambda calculus expression
;three cases for lambda calculus expression
(define free-vars
	(lambda (e)
		(letrec ([free
					(lambda (e vars)
						(cond
							[(null? e) '()]
							[(symbol? e)
							(if (member e vars);variable
								'()
								(list e))]
							[(eq? (car e) 'lambda);abstraction
							(free (caddr e) (append vars (cadr e)))]
							[else;application
							(append (free (car e) vars) (free (cadr e) vars))]))])
		(delete-repeat (free e '())))))

(define bound-vars
	(lambda (e)
		(letrec ([bound
					(lambda (e vars)
						(cond
							[(null? e) '()]
							[(symbol? e)
							(if (member e vars)
								(list e)
								'())]
							[(eq? (car e) 'lambda)
							(bound (caddr e) (append vars (cadr e)))]
							[else
							(append (bound (car e) vars) (bound (cadr e) vars))]))])
		(delete-repeat (bound e '())))))

;Problem2
(define occurs-free?
	(lambda (var exp)
		(cond
			[(null? exp) #f];to allow 0 arg
			[(symbol? exp) (eq? exp var)]
			[(eq? (car exp) 'lambda)
			(and (if (symbol? (caddr exp))
					#t
					(occurs-free? var (caddr exp)))
				(andmap (lambda (x) (not (equal? x var)));all arguments has to be different than var
					(cadr exp)))]
			[(eq? (car exp) 'if)
			(and (if (not (symbol? (caddr exp)))
					(occurs-free? var (caddr exp))
					(not (occurs-free? var (caddr exp))))
				 (if (not (symbol? (cadddr exp)))
				 	(occurs-free? var (cadddr exp))
				 	(not (occurs-free? var (cadddr exp)))))]
			[(eq? (car exp) 'let)
			(and (ormap (lambda (x) (if (symbol? x)
										(eq? var x)
										(occurs-free? var x)))
					(map cadr (cadr exp)))
				 (occurs-free? var (caddr exp)))]
			[(eq? (car exp) 'let*)
			(and (andmap (lambda (x) (if (not (eq? (car x) var));car here is the variable
										#t
										(occurs-free? var (cadr x))))
						(cadr exp))
				(occurs-free? var (caddr exp)))]
			[(eq? (car exp) 'set!)
			(if (member var (cddr exp))
				#t
				#f)]
			[else
			(ormap (lambda (x)
						(let ([free? (occurs-free? var x)])
							(if (and (symbol? x) free?)
								#t
								(not free?))))
			exp)])))
(define let->application
	(lambda (letexp)
		(append (list (list 'lambda (map car (cadr letexp)) (cadr (cdr letexp)))) (map cadr (cadr letexp)))))


(define occurs-bound?
	(lambda (var exp)
		(cond
			[(null? exp) #f]
			[(symbol? exp) #f]
			[(eq? (car exp) 'lambda)
			(or (and (ormap (lambda (x) (equal? x var)) (cadr exp))
					 (occurs-free? var (caddr exp)))
				(occurs-bound? car (caddr exp)))]
			[(eq? (car exp) 'if)
			(or (occurs-bound? var (caddr exp))
				(occurs-bound? var (cadddr exp)))]
			[(eq? (car exp) 'let)
			(occurs-bound? var (let->application exp))]
			[(eq? (car exp) 'let*)
			(or (occurs-bound? var (caddr exp))
				 (ormap (lambda (x)
				 			 (and (equal? (car x) var) (ormap (lambda (y) 
				 			 										(if (symbol? y)
				 														(equal? y var)
				 														(occurs-bound? var y)))
				 											(map cadr (cadr exp)))))
				 		(cadr exp)))]
			;(occurs-bound? var (let->application (let*->let exp)))]
			[(eq? (car exp) 'set!)
			(if (member var (cddr exp))
				#t
				#f)]
			[else
			(ormap (lambda (x) (occurs-bound? var x)) exp)])))

;Problem3
(define get-pos
	(lambda (ls item)
		(cond
			[(null? ls) -1]
			[(eq? (car ls) item) 0]
			[else
			(if (= (get-pos (cdr ls) item) -1)
				-1
				(add1 (get-pos (cdr ls) item)))])))
(define bound
	(lambda (item vars depth)
		(if (null? vars)
			(list ': 'free item)
			(let ([pos (get-pos (car vars) item)])
				(if (= -1 pos)
					(bound item (cdr vars) (add1 depth))
					(list ': depth pos))))))

(define lexical-address
	(lambda (exp)
		(letrec ([create-addr
					(lambda (exp vars)
						(cond
							[(symbol? exp) (bound exp vars 0)]
							[(eq? (car exp) 'lambda)
							(list 'lambda (cadr exp) (create-addr (caddr exp) (cons (cadr exp) vars)))]
							[(eq? (car exp) 'if)
							(cons'if (create-addr (cdr exp) vars))]
							[(eq? (car exp) 'let)
							(let ([new-vars (map car (cadr exp))])
								(list 'let
									  (map (lambda (x) (list (car x) (create-addr (cadr x) vars)))
									  	   (cadr exp))
									  (create-addr (caddr exp) (cons new-vars vars))))]
							[(eq? (car exp) 'set!)
							(list 'set! (cadr exp) (create-addr (caddr exp) vars))]
							[else
							(map (lambda (x) (create-addr x vars)) exp)]))])
		(create-addr exp '()))))

;Problem4
(define un-lexical-address
	(lambda (exp)
		(letrec ([helper 
					(lambda (exp vars)
						(cond
							[(null? (cdr exp))
							(list (helper (car exp) vars))]
							[(and (eq? (car exp) ':)
								  (eq? (cadr exp) 'free))
							(caddr exp)]
							[(and (eq? (car exp) ':)
								  (number? (cadr exp)))
							(list-ref (list-ref vars (cadr exp)) (caddr exp))]
							[(eq? (car exp) 'lambda)
							(list 'lambda (cadr exp) (helper (caddr exp) (cons (cadr exp) vars)))]
							[(eq? (car exp) 'if)
							(cons 'if
								  (cons (helper (cadr exp) vars)
										(helper (cddr exp) vars)))]
							[(eq? (car exp) 'let)
							(list 'let 
								  (map (lambda (x) (list (car x) (helper (cadr x) vars))) (cadr exp))
								  (helper (caddr exp) (cons (map car (cadr exp)) vars)))]
							[(eq? (car exp) 'set!)
							(cons 'set! (cons (cadr exp) (helper (cddr exp) vars)))]
							[else
							(map (lambda (x) (helper x vars)) exp)]))])
		(helper exp '()))))

