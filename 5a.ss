;Linjie Zha A05

;problem1
(define interval-contains?
  (lambda (interval number)
    (if (< number (car interval))
        #f
        (if (> number (cadr interval))
            #f
            #t))))

(define interval-intersects?
  (lambda (i1 i2)
    (if (interval-contains? i1 (car i2))
        #t
        (if (interval-contains? i2 (car i1))
            #t
            #f))))

(define interval-union-mod
  (lambda (i1 i2)
        (list (min (car i1) (car i2)) (max (cadr i1) (cadr i2)))))

(define sort-first
  (lambda (i1 i2)
    (< (car i1) (car i2))))

(define minimize-interval-list-helper
  (lambda (ls)
  	(cond
  		[(null? (cdr ls)) ls]
  		[(not (interval-intersects? (car ls) (cadr ls))) (cons (car ls) (minimize-interval-list-helper (cdr ls)))]
  		[else (minimize-interval-list-helper (cons (interval-union-mod (car ls) (cadr ls)) (cddr ls)))])))

(define minimize-interval-list
  (lambda (ls)
    (minimize-interval-list-helper (list-sort sort-first ls))))

;Problem2
(define exists?
	(lambda (pred ls)
		(member #t (map pred ls))))

;Problem3
(define list-index
	(lambda (pred ls)
		(cond
			[(not (exists? pred ls)) #f]
			[(pred (car ls)) 0]
			[(list-index pred (cdr ls)) (+ 1 (list-index pred (cdr ls)))]
			[else #f])))

;Problem4
(define pascal-triangle-helper
  (lambda (ls)
    (if (equal? 1 (length ls))
        '()
        (cons (+ (car ls) (cadr ls)) (pascal-triangle-helper (cdr ls))))))
    
(define pascal-next
  (lambda (ls)
    (append (cons '1 (pascal-triangle-helper (car ls))) '(1))))

(define pascal-triangle
	(lambda (n)
		(cond
			[(negative? n) '()]
			[(zero? n) '((1))]
			[else (cons (pascal-next (pascal-triangle (- n 1))) (pascal-triangle (- n 1)))])))

;Problem5
(define product
	(lambda (s1 s2)
		(cond
			[(or (null? s1) (null? s2)) '()]
			[(append (map (lambda (x) (list (car s1) x)) s2) (product (cdr s1) s2))])))

;Problem6
(define max-edges
  (lambda (n)
    (/ (* n (- n 1)) 2)))

;Problem7
(define num-of-edges
  (lambda (ls)
    (cond
      [(null? ls) 0]
      [(null? (cdr ls)) (car ls)]
      [else (+ (car ls) (num-of-edges (cdr ls)))])))

(define get-all-edges-notself
  (lambda (ls)
    (map (lambda (x) (length (cadr x))) ls)))

(define complete?
  (lambda (G)
    (cond
      [(null? G) #t]
      [(null? (cdr G)) #t]
      [else (= (/ (num-of-edges (get-all-edges-notself G)) 2) (max-edges (length G)))])))

;Problem8
(define get-edges-notself
  (lambda (e ls)
    (cond
      [(null? ls) '()]
      [(equal? e (car ls)) (cdr ls)]
      [else (cons (car ls) (get-edges-notself e (cdr ls)))])))

(define complete
  (lambda (ls)
    (letrec ([make-complete (lambda (x)
                              (list x (get-edges-notself x ls)))])
      (map make-complete ls))))

;Problem9
(define replace
  (lambda (old new ls)
    (cond
      [(null? ls) '()]
      [(equal? old (car ls))
       (cons new (replace old new (cdr ls)))]
      [else (cons (car ls) (replace old new (cdr ls)))])))

;Problem10
(define remove-first
  (lambda (element ls)
    (cond
      [(null? ls) '()]
      [(equal? element (car ls)) (cdr ls)]
      [else (cons (car ls) (remove-first element (cdr ls)))])))

;Problem11
(define remove-last
  (lambda (element ls)
    (cond
      [(null? ls) '()]
      [(and (equal? (car ls) element) (not (member element (cdr ls))))
       (cdr ls)]
      [else (cons (car ls) (remove-last element (cdr ls)))])))