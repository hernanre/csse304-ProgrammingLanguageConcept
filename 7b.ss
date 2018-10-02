;Linjie Zha A7b

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

;n is the number you need to add to the current max
(define go-right
  (lambda (sym n ls)
    (if (> (cadr ls) (+ n (cadddr ls)))
        (list (car ls) (cadr ls) sym (+ n (cadddr ls)));in this case the new sym from the recursive call on the right subtree will be the max interior
        (list sym (+ n (cadddr ls)) sym (+ n (cadddr ls))))))

(define go-left;similiar idea with go-right except the right child is a number 
  (lambda (sym n ls)
    (if (< (cadr ls) (+ n (cadddr ls)))
        (list sym (+ n (cadddr ls)) sym (+ n (cadddr ls)))
        (list (car ls) (cadr ls) sym (+ n (cadddr ls))))))

;the case that both children are lists
(define both-list
  (lambda (sym ls1 ls2)
    (if (>= (cadr ls1) (cadr ls2))
        (if (> (cadr ls1) (+ (cadddr ls1) (cadddr ls2)))
            (list (car ls1) (cadr ls1) sym (+ (cadddr ls1) (cadddr ls2)))
            (list sym (+ (cadddr ls1) (cadddr ls2)) sym (+ (cadddr ls1) (cadddr ls2))))
        (if (< (cadr ls2) (+ (cadddr ls1) (cadddr ls2)))
            (list sym (+ (cadddr ls1) (cadddr ls2)) sym (+ (cadddr ls1) (cadddr ls2)))
            (list (car ls2) (cadr ls2) sym (+ (cadddr ls1) (cadddr ls2)))))))

;A list of four elements and the first one will be the max interior 
(define bt-max-interior-helper
  (lambda (T)
    (cond
      [(and (number? (cadr T)) (number? (caddr T))) 
       (list (car T) (+ (cadr T) (caddr T)) (car T) (+ (cadr T) (caddr T)))];if both children are numbers, then the current symbol is the max interior for this tree
      [(and (number? (cadr T)) (list? (caddr T)))
       (go-right (car T) (cadr T) (bt-max-interior-helper (caddr T)))];let root be the current sym, left child which is a number be n, the recursion on the right subtree be the ls
      [(and (list? (cadr T)) (number? (caddr T)))
       (go-left (car T) (caddr T) (bt-max-interior-helper (cadr T)))]
      [(and (list? (cadr T)) (list? (caddr T)))
       (both-list (car T) (bt-max-interior-helper (cadr T)) (bt-max-interior-helper (caddr T)))])))

(define bt-max-interior
  (lambda (T)
    (car (bt-max-interior-helper T))))