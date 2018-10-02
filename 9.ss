;Linjie Zha A9

;Problem1
(define snlist-recur
	(lambda (base-val ls-proc nls-proc)
		(letrec ([helper
					(lambda (slist)
						(if (null? slist)
							base-val
							(if (list? (car slist))
								(ls-proc (helper (car slist)) (helper (cdr slist)))
								(nls-proc (car slist) (helper (cdr slist))))))])
		helper)))

;a
(define sn-list-sum
	(snlist-recur 0 + +))

;b
(define sn-list-map
	(lambda (proc slist)
		((snlist-recur '()
						cons
						(lambda (x y)
							(cons (proc x) y)))
		slist)))

;c
(define sn-list-paren-count
	(snlist-recur 2
				  +
				  (lambda (x y) y)))

;d
(define sn-list-reverse
	(snlist-recur '()
				   (lambda (x y)
				   	(append y (list x)))
				   (lambda (x y)
				   	(append y (list x)))))

;e
(define sn-list-occur
	(lambda (s slist)
		((snlist-recur 0
					  +
					  (lambda (x y)
					  	(if (eq? x s)
					  		(add1 y)
					  		y)))
		slist)))

;f
(define sn-list-depth
	(snlist-recur 1
				  (lambda (x y)
				  	(if (< x y)
				  		y
				  		(add1 x)))
				  (lambda (x y)
				  	y)))

;Problem2
(define bt-recur
	(lambda (base-val num-proc sym-proc)
		(letrec ([helper
					(lambda (bt)
						(if (null? bt)
							base-val
							(if (number? bt)
								(num-proc bt)
								(sym-proc (car bt) (helper (cadr bt)) (helper (caddr bt))))))])
		helper)))

(define bt-sum
	(bt-recur 0
			  +
			  (lambda (x y z)
			  	(+ y z))))

(define bt-inorder
	(bt-recur '()
			   (lambda (x) 
			   	'())
			   (lambda (x y z)
			   	(append y (list x) z))))

;Problem3
(define compose
	(case-lambda
 		[() (lambda (x) x)]
 		[(first . rest)
 			(let ([composed-rest (apply compose rest)])
 				(lambda (x) (first (composed-rest x))))]))

(define make-c...r
	(lambda (str)
		(letrec ([get-args-list
					(lambda (str)
						(let ([ls (string->list str)])
							(map (lambda (x)
									(if (eq? x #\a)
										car
										cdr))
							ls)))])
		(let ([args-list (get-args-list str)])
			(cond
				[(null? args-list)
				(lambda (x) x)]
				[(null? (cdr args-list))
				(car args-list)]
				[else
				(apply compose args-list)])))))