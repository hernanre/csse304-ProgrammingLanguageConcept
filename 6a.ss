;Linjie Zha A6a

;Problem1
(define curry2
	(lambda (proc)
		(lambda (x)
			(lambda (y)
				(proc x y)))))

;Problem2
(define curried-compose
	(lambda (proc1)
		(lambda (proc2)
			(lambda (x)
				(proc1 (proc2 x))))))

;Problem3
(define compose
	(lambda list-of-functions
		(if (null? (cdr list-of-functions))
			(car list-of-functions)
			(lambda (ls) ((car list-of-functions) ((apply compose (cdr list-of-functions)) ls))))))

;Problem4
(define make-list-c
	(lambda (n)
		(if (zero? n)
			(lambda (ls)
				'())
			(lambda (ls)
				(cons ls ((make-list-c (sub1 n)) ls))))))

;Problem5
(define let->application
	(lambda (letexp)
		(append (list (list 'lambda (map car (cadr letexp)) (cadr (cdr letexp)))) (map cadr (cadr letexp)))))

;Problem6
(define let*->let
	(lambda (let-star-exp)
		(letrec ([one-let
					(lambda (let-exp e)
						(if (null? let-exp)
							e
							(list 'let (list (car let-exp)) (one-let (cdr let-exp) e))))])
		(one-let (cadr let-star-exp) (caddr let-star-exp)))))


;Problem7
(define filter-in
	(lambda (pred? ls)
		(cond
			[(null? ls) '()]
			[(pred? (car ls)) (cons (car ls) (filter-in pred? (cdr ls)))]
			[else (filter-in pred? (cdr ls))])))

;Problem8
(define filter-out
	(lambda (pred? ls)
		(cond
			[(null? ls) '()]
			[(not (pred? (car ls)))
			(cons (car ls) (filter-out pred? (cdr ls)))]
			[else (filter-out pred? (cdr ls))])))

;Problem9
(define sort-list-of-symbols
	(lambda (los)
		(map string->symbol (sort string<? (map symbol->string los)))))

;Problem10
(define invert
	(lambda (ls)
		(cond
			[(null? ls) '()]
			[else (cons (list (cadr (car ls)) (car (car ls))) (invert (cdr ls)))])))

;Problem11
(define exists?
	(lambda (pred ls)
		(member #t (map pred ls))))

(define vector-index
	(lambda (pred vec)
		(letrec ([list-index
					(lambda (pred ls)
						(cond
							[(not (exists? pred ls)) #f]
							[(pred (car ls)) 0]
							[(list-index pred (cdr ls))
							(add1 (list-index pred (cdr ls)))]
							[else #f]))])
		(list-index pred (vector->list vec)))))

;Problem12
(define ribassoc
	(lambda (s los v fail-value)
		(cond
			[(member s los)
			 (vector-ref v (vector-index (lambda (x)
											(equal? s x)) (list->vector los)))]
			[else fail-value])))
