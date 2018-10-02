;Linjie Zha A05

;Problem1
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

(define check
  (lambda (r)
    (cond
      [(null? r) #t]
      [(not (symbol? (car r))) #f]
      [(not (number? (cadr r))) #f]
      [(not (positive? (cadr r))) #f]
      [else #t])))

(define multi-set?
  (lambda (obj)
    (cond
      [(not (relation? obj)) #f]
      [(member #f (map check obj)) #f]
      [else (set? (map car obj))])))

;Problem2
(define ms-size
  (lambda (ms)
    (apply + (map cadr ms))))

;Problem3
(define matrix-ref
  (lambda (m row col)
    (list-ref (list-ref m row) col)))

;Problem4
(define mat-format?
  (lambda (lol n)
    (cond
      [(null? lol) #t]
      [(= n 0) #f]
      [(= n (length (car lol)))
       (mat-format? (cdr lol) n)]
      [else #f])))

(define matrix?
  (lambda (obj)
    (cond
      [(not (list? obj)) #f]
      [(not (list? (car obj))) #f]
      [else (mat-format? obj (length (car obj)))])))

;Problem5
(define matrix-transpose
  (lambda (m)
    (cond
      [(null? (car m)) '()]
      [else (cons (map car m) (matrix-transpose (map cdr m)))])))

;Problem6
(define last
  (lambda (ls)
    (cond
      [(null? (cdr ls)) (car ls)]
      [else (last (cdr ls))])))

;Problem7
(define all-but-last
  (lambda (ls)
    (cond
      [(null? (cdr ls)) '()]
      [else (cons (car ls) (all-but-last (cdr ls)))])))