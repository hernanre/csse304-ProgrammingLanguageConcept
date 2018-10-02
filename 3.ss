;Linjie Zha A03

;Problem1
(define sum-of-squares
  (lambda (lon)
    (cond
      [(null? lon) 0]
      [else (+ (* (car lon) (car lon)) (sum-of-squares (cdr lon)))])))

(define make-vec-from-points
  (lambda (p1 p2)
    (cond
      [(null? p1) '()]
      [else (cons (- (car p2) (car p1)) (make-vec-from-points (cdr p1) (cdr p2)))])))

(define vec-length
  (lambda (v)
    (sqrt (sum-of-squares v))))

(define distance
  (lambda (p1 p2)
    (vec-length (make-vec-from-points p1 p2))))

(define nearest-point
  (lambda (p lop)
    (cond
      [(null? lop) '()]
      [(null? (cdr lop)) (car lop)]
      [(> (distance p (car lop)) (distance p (cadr lop)))
       (nearest-point p (cdr lop))]
      [else (nearest-point p (cons (car lop) (cddr lop)))])))

;Problem2
(define union
  (lambda (s1 s2)
    (cond
      [(null? s1) s2]
      [(member (car s1) s2) (union (cdr s1) s2)]
      [else (union (cdr s1) (cons (car s1) s2))])))

;Problem3
(define intersection
  (lambda (s1 s2)
    (cond
      [(null? s1) '()]
      [(member (car s1) s2)
       (cons (car s1) (intersection (cdr s1) s2))]
      [else (intersection (cdr s1) s2)])))

;Problem4
(define subset?
  (lambda (s1 s2)
    (cond
      [(null? s1) #t]
      [(not (member (car s1) s2)) #f]
      [else (subset? (cdr s1) s2)])))

;Problem5
(define set?
  (lambda (ls)
    (cond
      [(null? ls) #t]
      [(member (car ls) (cdr ls)) #f]
      [else (set? (cdr ls))])))

(define relation?
  (lambda (obj)
    (cond
      [(null? obj) #t]
      [(not (list? obj)) #f]
      [(not (set? obj)) #f]
      [(not (list? (car obj))) #f]
      [(not (null? (cddar obj))) #f]
      [else (relation? (cdr obj))])))

;Problem6
(define domain
  (lambda (r)
    (cond
      [(null? r) '()]
      [(member (caar r) (domain (cdr r)))
       (domain (cdr r))]
      [else (cons (caar r) (domain (cdr r)))])))
;Problem7
(define range
  (lambda (r)
    (cond
      [(null? r) '()]
      [(member (cadar r) (range (cdr r)))
       (range (cdr r))]
      [else (cons (cadar r) (range (cdr r)))])))

(define domain-range
  (lambda (r)
    (union (domain r) (range r))))

(define check
  (lambda (ls r)
    (cond
      [(null? ls) #t]
      [(not (member (list (car ls) (car ls)) r)) #f]
      [else (check (cdr ls) r)])))

(define reflexive?
  (lambda (r)
    (cond
      [(null? r) #t]
      [else (check (domain-range r) r)])))

;Problem8
(define hailstone-step-count
  (lambda (n)
    (cond
      [(= n 1) 0]
      [(= (modulo n 2) 0)
       (+ (hailstone-step-count (/ n 2)) 1)]
      [else (+ (hailstone-step-count (+ (* 3 n) 1)) 1)])))