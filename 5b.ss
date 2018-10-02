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
;Same as get-edges-notself
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