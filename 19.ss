(load "chez-init.ss")

(define v)
(define k)
(define ls)
(define L1)
(define L2)

(define-datatype kontinuation kontinuation?
  [init-k]
  [flatten-cdr-k (ls list?) (k kontinuation?)]
  [flatten-car-k  (flattened-cdr list?)
		  (k kontinuation?)]
  [append-k (car-L1 symbol?) (k kontinuation?)]
)


(define apply-k 
  (lambda ()
	 (cases kontinuation k
	    [init-k ()
	       v]
	    [flatten-cdr-k (ls-cdr cdr-k)
	    	(if (list? (car ls-cdr))
	    		(begin
	    			(set! ls (car ls-cdr))
	    			(set! k (flatten-car-k v cdr-k))
	    			(flatten-cps))
	    		(begin
	    			(set! v (cons (car ls-cdr) v))
					(set! k cdr-k)
					(apply-k)))]
	    [flatten-car-k (flattened-cdr flat-k)
	    	(begin
	    		(set! L1 v)
	    		(set! L2 flattened-cdr)
	    		(set! k flat-k)
	    		(append-cps))]
	    [append-k (car-L1 car-k)
	    	(begin
	    		(set! k car-k)
	    		(set! v (cons car-L1 v))
	    		(apply-k))])))





(define append-cps
	(lambda ()
		(if (null? L1)
			(begin
				(set! v L2)
				(apply-k))
			(begin
				(set! k (append-k (car L1) k))
				(set! L1 (cdr L1))
				(append-cps)))))

;'(trace append-cps flatten-cps apply-k)


;(define read-flatten-print
;  (lambda ()
;    (display "enter slist to flatten: ")
;    (let ([slist (read)])
;      (unless (eq? slist 'exit)
;	(flatten-cps slist (init-k))))))

(define flatten-cps
  (lambda ()
    (if (null? ls)
    	(begin
    		(set! v ls)
    		(apply-k))
    	(begin
    		(set! k (flatten-cdr-k ls k))
    		(set! ls (cdr ls))
    		(flatten-cps)))))






