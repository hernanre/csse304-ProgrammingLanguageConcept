;Linjie Zha A7a

;Problem1
(define vector-append-list
	(lambda (v lst)
		 (letrec ([copy-from-vector
             (lambda (v1 v2 pos)
                 (if (equal? (vector-length v2) pos) v1
                 (and (vector-set! v1 pos (vector-ref v2 pos))
                       (copy-from-vector v1 v2 (add1 pos)) v1)))]
           [copy-from-list
             (lambda (v ls pos)
                 (if (null? ls) v
                 (and (vector-set! v pos (car ls))
                       (copy-from-list v (cdr ls) (add1 pos)) v)))])
	(let ([new-vector (make-vector (+ (vector-length v) (length lst)))])
      (copy-from-vector new-vector v 0)
      (copy-from-list new-vector lst (vector-length v))
      new-vector))))

;Problem2
(define qsort
	(lambda (pred ls)
		(letrec ([sort-low
             		(lambda (pivot ls pred)
               			(if (null? ls)
                    	ls
                   		(if (pred (car ls) pivot)
                       		(cons (car ls) (sort-low pivot (cdr ls) pred))
                       		(sort-low pivot (cdr ls) pred))))]
           		[sort-high
             		(lambda (pivot ls pred)
               			(if (null? ls)
                   		ls
                 		(if (pred (car ls) pivot)
                     		(sort-high pivot (cdr ls) pred)
                     		(cons (car ls) (sort-high pivot (cdr ls) pred)))))])   
		(if (< (length ls) 2)
        ls
        (append (qsort pred (sort-low (car ls) (cdr ls) pred))
                (cons (car ls) (qsort pred (sort-high (car ls) (cdr ls) pred))))))))

;Problem3
(define sort-graph
  (lambda (g)
    (list-sort (lambda (x y) (string<? (symbol->string (car x)) (symbol->string (car y)))) g)))

(define sort-list-of-symbols
  (lambda (los)
    (map string->symbol (sort string<? (map symbol->string los)))))
    
(define list-intersect?
  (lambda (ls1 ls2)
    (cond
      [(null? ls1) #f]
      [(member (car ls1) ls2) #t]
      [else (list-intersect? (cdr ls1) ls2)])))

(define union
  (lambda (ls1 ls2)
    (cond
      [(null? ls2) ls1]
      [(member (car ls2) ls1) (union ls1 (cdr ls2))]
      [else (cons (car ls2) (union (cdr ls2) ls1))])))

(define merge?
  (lambda (g)
    (if (null? (cdr g))
        #t
        (if (not (list-intersect? (car g) (cadr g)))
            #f
            (merge? (cons (union (car g) (cadr g)) (cddr g)))))))

(define change
  (lambda (ls)
    (map (lambda (x) (cons (car x) (list (cdr x)))) ls)))

(define connected?
  (lambda (g)
    (if (null? g) 
        #t
        (merge? (sort-graph (map sort-list-of-symbols (map (lambda (x) (cons (car x) (cadr x))) g)))))))

;Problem4
(define reverse-it
  (letrec ([reverse-helper
             (lambda (ls1 ls2)
               (if (null? ls1)
                   ls2
                   (reverse-helper (cdr ls1) (cons (car ls1) ls2))))])
  (lambda (lst)
    (reverse-helper lst '()))))

;Problem5
(define empty-BST
	(lambda ()
		'()))

(define empty-BST?
	(lambda (obj)
		(null? obj)))

(define BST-insert
	(lambda (num bst)
		(if (empty-BST? bst)
			(list num '() '())
			(cond
				[(= (car bst) num) bst]
				[(> (car bst) num) 
				(list (car bst) (BST-insert num (cadr bst)) (caddr bst))]
				[(< (car bst) num)
				(list (car bst) (cadr bst) (BST-insert num (caddr bst)))]))))

(define BST-inorder
	(lambda (bst)
		(if (empty-BST? bst)
			'()
			(append (BST-inorder (cadr bst)) (list (car bst)) (BST-inorder (caddr bst))))))

(define BST?
  (lambda (obj)
    (cond 
      [(null? obj) #t]
      [(not (list? obj)) #f]
      [(not (= (length obj) 3)) #f]
      [(not (number? (car obj))) #f]
      [(not (list? (cadr obj))) #f]
      [(not (list? (caddr obj))) #f]
      [(not (apply < (BST-inorder obj))) #f]
      [else (and (BST? (cadr obj)) 
      			 (BST? (caddr obj)))])))

(define BST-element
	(lambda (bst)
		(car bst)))

(define BST-left
	(lambda (bst)
		(cadr bst)))

(define BST-right
	(lambda (bst)
		(caddr bst)))

(define BST-insert-nodes
	(lambda (bst nums)
		(if (null? nums)
			bst
			(BST-insert-nodes (BST-insert (car nums) bst) (cdr nums)))))

(define BST-contains?
  (lambda (bst num)
    (cond
      [(empty-BST? bst) #f]
      [(= (car bst) num) #t]
      [(> (car bst) num) (BST-contains? (BST-left bst) num)]
      [(< (car bst) num) (BST-contains? (BST-right bst) num)])))

(define BST-height
	(lambda (bst)
		(if (null? bst)
			-1
			(add1 (max (BST-height (BST-left bst)) (BST-height (BST-right bst)))))))


;Problem6
(define map-by-position
  (lambda (fn-list arg-list)
    (map (lambda (x y) (x y))
      fn-list arg-list)))

;Problem7
(define bt-leaf-sum
  (lambda (T)
    (if (number? T)
      T
      (+ (bt-leaf-sum (cadr T)) (bt-leaf-sum (caddr T))))))

(define bt-inorder-list
  (lambda (T)
      (if (number? T)
        '()
        (append (bt-inorder-list (cadr T)) (list (car T)) (bt-inorder-list (caddr T))))))

(define bt-max
  (lambda (T)
    (if (number? T)
      T
      (max (bt-max (cadr T)) (bt-max (caddr T))))))