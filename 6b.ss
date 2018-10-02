;Linjie Zha A6b

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

