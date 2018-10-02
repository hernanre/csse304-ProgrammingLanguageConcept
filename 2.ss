;Linjie Zha A02

;Problem1
(define fact
  (lambda (n)
    (cond
      [(zero? n) 1]
      [else (* n (fact (- n 1)))])))

(define choose
  (lambda (n k)
    (/ (fact n) (* (fact k) (fact (- n k))))))

;Problem2
(define range
  (lambda (m n)
    (cond
      [(<= n m) '()]
      [else (cons m (range (+ m 1) n))])))

;Problem3
(define set?
  (lambda (ls)
    (cond
      [(null? ls) #t]
      [(member (car ls) (cdr ls)) #f]
      [else (set? (cdr ls))])))

;Problem4
(define sum-of-squares
  (lambda (lon)
    (cond
      [(null? lon) 0]
      [else (+ (* (car lon) (car lon)) (sum-of-squares (cdr lon)))])))

;Problem5
(define make-vec-from-points
  (lambda (p1 p2)
    (cond
      [(null? p1) '()]
      [else (cons (- (car p2) (car p1)) (make-vec-from-points (cdr p1) (cdr p2)))])))

;Problem6
(define dot-product
  (lambda (v1 v2)
    (cond
      [(null? v1) 0]
      [else (+ (* (car v1) (car v2)) (dot-product (cdr v1) (cdr v2)))])))

;Problem7
(define vec-length
  (lambda (v)
    (sqrt (sum-of-squares v))))

;Problem8
(define distance
  (lambda (p1 p2)
    (vec-length (make-vec-from-points p1 p2))))

;Problem9
(define cross-product
  (lambda (v1 v2)
    (list (- (* (cadr v1) (caddr v2)) (* (caddr v1) (cadr v2))) 
      (- (* (caddr v1) (car v2)) (* (car v1) (caddr v2)))
      (- (* (car v1) (cadr v2)) (* (cadr v1) (car v2))))))

;Problem10
(define parallel?
  (lambda (v1 v2)
    (equal? '(0 0 0) (cross-product v1 v2))))

;Problem11
(define collinear?
  (lambda (p1 p2 p3)
    (equal? 0 (vec-length (cross-product (make-vec-from-points p1 p2) (make-vec-from-points p1 p3))))))
