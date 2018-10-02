;Linjie Zha A8a

;Problem1 a
(define (slist-map proc slist)
	(let map-proc ([slist slist])
		(cond
			[(null? slist) '()]
			[(symbol? (car slist))
			(cons (proc (car slist)) (map-proc (cdr slist)))]
			[else
			(cons (map-proc (car slist)) (map-proc (cdr slist)))])))

;1b
(define (slist-reverse slist)
	(let reverse-slist ([slist slist])
		(cond
			[(null? slist) '()]
			[(symbol? (car slist))
			(append (reverse-slist (cdr slist)) (list (car slist)))]
			[else
			(append (reverse-slist (cdr slist)) (list (reverse-slist (car slist))))])))

;1c
(define (slist-paren-count slist)
	(let count ([slist slist])
		(cond
			[(null? slist) 2]
			[(symbol? (car slist))
			(count (cdr slist))]
			[else
			(+ (count (car slist)) (count (cdr slist)))])))

;1d
(define (slist-depth slist)
	(let get-depth ([slist slist])
		(cond
			[(null? slist) 1]
			[(symbol? (car slist))
			(get-depth (cdr slist))]
			[else 
			(max (add1 (get-depth (car slist))) (get-depth (cdr slist)))])))

;1e
(define (slist-symbols-at-depth slist d)
	(let get-symbols ([slist slist]
					  [depth 1])
		(cond
			[(null? slist) '()]
			[(symbol? (car slist))
			(if (= d depth)
				(append (list (car slist)) (get-symbols (cdr slist) depth))
				(get-symbols (cdr slist) depth))]
			[else
			(append (get-symbols (car slist) (add1 depth)) (get-symbols (cdr slist) depth))])))

;Problem2
(define group-by-two
	(lambda (ls)
		(cond
			[(null? ls) '()]
			[(null? (cdr ls)) (list ls)]
			[else
			(cons (list (car ls) (cadr ls)) (group-by-two (cddr ls)))])))

;Problem3
(define make-stack
	(lambda ()
 		(let ([stk '()])
	 		(lambda (msg . args )
				(case msg ; Scheme's case is a similar to switch in some other languages.
					[(empty?) (null? stk)]
 					[(push) (set! stk (cons (car args) stk))]
					[(pop) (let ([top (car stk)])
							(set! stk (cdr stk))
 							top)]
 					[else (errorf 'stack "illegal message to stack object: ~a" msg)])))))

(define next
	(lambda (stack)
		(if (stack 'empty?)
			#f
			(let ([cur (stack 'pop)])
				(cond
					[(null? cur) (next stack)]
					[(symbol? cur) cur]
					[else (begin (stack 'push (cdr cur))
								 (stack 'push (car cur))
								 (next stack))])))))

(define make-slist-leaf-iterator
	(lambda (slist)
		(let ([stk (make-stack)])
			(begin (stk 'push slist)
					(lambda (msg)
						(case msg
							[(next) (next stk)]))))))

(define make-queue22
  (lambda ()
    (let ([q-f '()] [q-r '()])
    	(lambda (msg . args)
			(case msg
				[(front-null?) (front-null? q-f q-r)]
	  			[(empty?) 'front-null?
	   						(null? q-f)]
	  			[(enqueue) (set! q-r (cons (car args) q-r))]
	  			[(dequeue) 'front-null?
	   						(cond
	    						[(null? q-f) (errorf 'queue "attempt to dequeue from empty queue")]
	    						[else (let ([h (car q-f)])
	    								(set! q-f (cdr q-f))
		    						h)])]
	  			[(peek)
	   				'front-null?
	   					(cond
	    				[(null? q-f) (errorf 'queue "attempt to peek from empty queue")]
	    				[else (car q-f)])]
	  					[else (errorf 'queue "illegal message to queue object: ~a" msg)])))))