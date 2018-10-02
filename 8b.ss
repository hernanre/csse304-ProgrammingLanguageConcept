;Linjie Zha A8b

;Problem 4
(define get-rest-groups
	(lambda (ls n)
		(if (= n 0)
			ls
			(get-rest-groups (cdr ls) (sub1 n)))))

(define group-by-n
	(lambda (ls n)
		(letrec ([get-first-group
					(lambda (ls n)
						(if (= n 0)
							'()
							(cons (car ls) (get-first-group (cdr ls) (sub1 n)))))])
		(cond
			[(null? ls) ls]
			[(< (length ls) n) (list ls)]
			[else
			(cons (get-first-group ls n) (group-by-n (get-rest-groups ls n) n))]))
		))

;Problem 5
;The helper procedure will create a list which the car will be a boolean value to show
;if a substituion have been made and the cdr is just the list I need to return
(define subst-leftmost
	(lambda (new old slist equality-pred?)
		(letrec ([helper
					(lambda (new old slist equality-pred?)
						(cond
							[(null? slist) (cons #f slist)]
							[(symbol? (car slist))
							(if (equality-pred? old (car slist))
								(cons #t (cons new (cdr slist)))
								(let ([helper-list-cdr
										(helper new old (cdr slist) equality-pred?)])
								(cons (car helper-list-cdr) (cons (car slist) (cdr helper-list-cdr)))))];check first, then call recursive procedure on the cdr
							[else
							(let ([helper-list-car;the case that the car of the slist is a slist
									(helper new old (car slist) equality-pred?)])
							(if (car helper-list-car)
								(cons #t (cons (cdr helper-list-car) (cdr slist)))
								(let ([helper-list-cdr
										(helper new old (cdr slist) equality-pred?)])
										(cons (car helper-list-cdr) (cons (cdr helper-list-car) (cdr helper-list-cdr))))))]))])
		(cdr (helper new old slist equality-pred?)))))
							