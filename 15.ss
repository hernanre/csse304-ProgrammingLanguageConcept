;Linjie Zha A15

;Problem2
;The one written in this assignment can also be used for other calculations besides fib.
;For the in-class version fib, since we have the max stored we don't need to look over the whole list.

;Problem1
(define apply-continuation
  (lambda (k v)
    (k v)))

;a
(define member?-cps
	(lambda (item ls k)
		(cond
			[(null? ls) (apply-continuation k #f)]
			[(eq? (car ls) item) (apply-continuation k #t)]
			[else (member?-cps item (cdr ls) k)])))

;b
(define set?-cps
	(lambda (ls k)
		(cond
			[(null? ls) (apply-continuation k #t)]
			[(not (pair? ls)) (apply-continuation k #f)]
			[else (set?-cps (cdr ls) (lambda (cdr-ls)
										(member?-cps (car ls) (cdr ls) (lambda (appeared?)
																			(if appeared?
																				(apply-continuation k #f)
																				(apply-continuation k cdr-ls))))))])))

(define 1st-cps
	(lambda (ls k)
		(apply-continuation k (car ls))))

(define set-of-cps
  (lambda (s k)
    (if (null? s)
        (apply-continuation k '())
        (set-of-cps (cdr s) (lambda (cdr-s)
                               (member?-cps (car s) (cdr s) (lambda (appeared?)
                                                                (if appeared?
                                                                    (apply-continuation k cdr-s)
                                                                    (apply-continuation k (cons (car s) cdr-s))))))))))

(define map-cps
	(lambda (proc-cps l k)
		(cond
			[(null? l) (apply-continuation k '())]
			[else (map-cps proc-cps (cdr l) (lambda (cdr-ls)
												(proc-cps (car l) (lambda (res)
																	(apply-continuation k (cons res cdr-ls))))))])))

(define domain-cps
	(lambda (rel k)
		(map-cps 1st-cps rel (lambda (domain-ls)
								(set-of-cps domain-ls k)))))



;c
(define make-cps
	(lambda (proc)
		(lambda (arg k)
			(apply-continuation k (proc arg)))))

;d
(define andmap-cps
	(lambda (pred-cps ls k)
		(cond
			[(null? ls)(apply-continuation k #t)]
			[(pred-cps (car ls) (lambda (still-true)
									(if still-true
										(andmap-cps pred-cps (cdr ls) k)
										(apply-continuation k #f))))])))

;e
(define cps-snlist-recur
	(lambda (base-value item-proc-cps list-proc-cps)
    	(letrec ([helper (lambda (ls k)
                       		(if (null? ls)
                           		(apply-continuation k base-value)
                           		(let ([c (car ls)])
                           			(if (or (pair? c) (null? c))
                                 		(helper c (lambda (first-res)
                                             		(helper (cdr ls) (lambda (other-res)
                                                                		(list-proc-cps first-res other-res k)))))
                                		(helper (cdr ls) (lambda (other-res)
                                                    		(item-proc-cps c other-res k)))))))])
      helper)))

(define +-cps
	(lambda (a b k)
		(apply-continuation k (+ a b))))

(define max-cps
	(lambda (a b k)
		(apply-continuation k (max a b))))

(define sn-list-depth-cps
  (cps-snlist-recur 1
    (lambda (a b k) (apply-continuation k b))
    (lambda (a b k) (max-cps (+ a 1) b k))))



(define append-cps
	(lambda (ls1 ls2 k)
    	(if (null? ls2)
        	(apply-continuation k ls1)
        	(append-cps (reverse (cons (car ls2) (reverse ls1))) (cdr ls2) k))))


(define sn-list-reverse-cps
	(cps-snlist-recur '()
					  (lambda (a b k) (append-cps b (list a) k))
					  (lambda (a b k) (append-cps b (list a) k))))


(define sn-list-occur-cps
	(lambda (item ls k)
		((cps-snlist-recur 0
    					   (lambda (a acc k)
    					   		(if (eq? a item)
            						(+-cps 1 acc k)
            						(apply-continuation k acc)))
      						+-cps)
      	ls k)))

;Problem2
(define memoize
	(lambda (f hash equiv?)
		(let ([hash-table (make-hashtable hash equiv?)])
			(lambda args
				(let ([checked? (hashtable-ref hash-table args #f)])
					(if checked?
						checked?
						(let ([res (apply f args)])
							(hashtable-set! hash-table args res) res)))))))

;Problem3
(define-syntax with-values
	(syntax-rules ()
		[(_ expr consumer)
			(call-with-values
				(lambda () expr) consumer)]))

(define-syntax mv-let
	(syntax-rules ()
		((_ ((x ...) e0) e1 e2 ...)
			(with-values e0
				(lambda (x ...) e1 e2 ...)))))

(define subst-leftmost
	(letrec ([helper
				(lambda (new old slist proc)
					(cond
						[(null? slist) (values #f slist)]
						[(symbol? (car slist))
						(if (proc (car slist) old)
							(values #t (cons new (cdr slist)))
							(with-values (helper new old (cdr slist) proc)
								(lambda (x y)
									(values x (cons (car slist) y)))))]
						[else
						(with-values (helper new old (car slist) proc)
							(lambda (pred cur)
								(if pred
									(values #t (cons cur (cdr slist)))
									(with-values (helper new old (cdr slist) proc)
										(lambda (car-ls cdr-ls)
											(values car-ls (cons cur cdr-ls)))))))]))])
	(lambda (new old slist proc)
		(with-values (helper new old slist proc)
			(lambda (a b) b)))))

