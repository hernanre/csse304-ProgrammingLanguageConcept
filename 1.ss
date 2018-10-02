;Linjie Zha A01

;Problem1
(define Fahrenheit->Celsius
  (lambda (temp)
    (/ (* 5 (- temp 32)) 9)))

;Problem2
(define interval-contains?
  (lambda (interval number)
    (if (< number (car interval))
        #f
        (if (> number (cadr interval))
            #f
            #t))))

;Problem3
(define interval-intersects?
  (lambda (i1 i2)
    (if (interval-contains? i1 (car i2))
        #t
        (if (interval-contains? i2 (car i1))
            #t
            #f))))

;Problem4
(define interval-union
  (lambda (i1 i2)
    (if (not (interval-intersects? i1 i2))
        (list i1 i2)
        (list (list (min (car i1) (car i2)) (max (cadr i1) (cadr i2)))))))

;Problem5
(define divisible-by-7?
  (lambda (num)
    (eq? 0 (modulo num 7))))

;Problem6
(define ends-with-7?
  (lambda (num)
    (eq? 0 (modulo (- num 7) 10))))

;Problem7
(define first
  (lambda (ls)
    (car ls)))

(define second
  (lambda (ls)
    (cadr ls)))

(define third
  (lambda (ls)
    (caddr ls)))