;:  Single-file version of the interpreter.
;; Easier to submit to server, probably harder to use in the development process

(load "chez-init.ss") 

;-------------------+
;                   |
;    DATATYPES      |
;                   |
;-------------------+

; parsed expression

(define-datatype expression expression?
  [var-exp        
   (id symbol?)]
  [lit-exp        
   (datum
    (lambda (x)
      (ormap 
       (lambda (pred) (pred x))
       (list number? vector? boolean? symbol? string? pair? null?))))]
  [lambda-exp
   (id (lambda (v) (or (symbol? v) (list-of expression?))))
   (body (list-of expression?))]
  [lambda-sym-exp
    (id symbol?)
    (body (list-of expression?))]
  [lambda-exp-improper
    (id pair?)
    (bodies (list-of expression?))]
  [let-exp
    (vars (list-of (list-of expression?)))
    (body (list-of expression?))]
  [let*-exp
    (vars (list-of (list-of expression?)))
    (body (list-of expression?))]
  [if-exp
    (bool expression?)
    (thencase expression?)]
  [if-else-exp
    (bool expression?)
    (thencase expression?)
    (elsecase expression?)]
  [app-exp        ; applications
   (rator expression?)
   (rands (list-of expression?))]
  [and-exp
    (body (list-of expression?))]
  [or-exp
    (body (list-of expression?))]
  [cond-exp
    (body (list-of (list-of expression?)))]
  [case-exp
    (arg expression?)
    (body (list-of (lambda (x) (expression? (2nd x)))))]
  [begin-exp
    (body (list-of expression?))]
  [while-exp
    (test expression?)
    (body expression?)]
  [letrec-exp
    (names (list-of symbol?))
    (idss (list-of (lambda (x) (or (list-of symbol? x) (pair? x)))))
    (bodiess (list-of (list-of expression?)))
    (letrec-bodies (list-of expression?))]
  [named-let-exp
    (name symbol?)
    (ids (list-of symbol?))
    (bodies (list-of expression?))
    (named-let-bodies (list-of expression?))]
  [set!-exp
    (vars symbol?)
    (body expression?)]
  [define-exp
    (vars symbol?)
    (body expression?)])


;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype continuation continuation?
  [id-k
    (proc procedure?)]
  [if-k
    (thencase expression?)
    (env environment?)
    (k continuation?)]
  [if-else-k
    (thencase expression?)
    (elsecase expression?)
    (env environment?)
    (k continuation?)]
  [rator-k
    (rands (list-of expression?))
    (env environment?)
    (k continuation?)]
  [rands-k
    (proc-value proc-val?)
    (k continuation?)]
  [or-k
    (bodies (list-of expression))
    (env environment?)
    (k continuation?)]
  [while-k
    (test expression?)
    (body (list-of expression?))
    (env environment?)
    (k continuation?)]
  [eval-bodies-k
    (bodies (list-of expression?))
    (env environment?)
    (k continuation?)]
  [set!-k
    (body expression?)
    (env environment?)
    (k continuation?)]
  [set!-box-k
    (boks box?)
    (k continuation?)]
  [add-to-global-k
    (var symbol?)
    (k continuation?)]
  [exit-k])

(define-datatype environment environment?
  [empty-env-record]
  [extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)]
  [recursively-extended-env-record
    (names (list-of symbol?))
    (idss (list-of (lambda (x) (or (list-of symbol? x) (pair? x)))))
    (bodiess (list-of (list-of expression?)))
    (env environment?)])

(define-datatype proc-val proc-val?
  [continuation-proc
    (cp continuation?)]
  [exit-proc]
  [prim-proc
   (name symbol?)]
  [closure
    (ids (list-of symbol?))
    (bodies (list-of expression?))
    (env environment?)]
  [closure-sym
    (ids (list-of symbol?))
    (bodies (list-of expression?))
    (env environment?)]
  [closure-improper
    (ids pair?)
    (bodies (list-of expression?))
    (env environment?)])



(define apply-k
  (lambda (k val)
    (cases continuation k
      [id-k (proc) (proc val)]
      [if-k (thencase env k)
        (if val
          (eval-exp thencase env k))]
      [if-else-k (thencase elsecase env k)
        (if val
          (eval-exp thencase env k)
          (eval-exp elsecase env k))]
      [rator-k (rands env k)
        (eval-rands rands env (rands-k val k))]
      [rands-k (proc-value k)
        (apply-proc proc-value val k)]
      [or-k (bodies env k)
        (if val
          (apply-k k val)
          (if (null? bodies)
            (apply-k k #f)
            (eval-exp (car body) env (or-k (cdr bodies) env k))))]
      [eval-bodies-k (bodies env k)
        (eval-bodies bodies env k)]
      [set!-k (body env k)
        (eval-exp body env (set!-box-k val k))]
      [set!-box-k (boks k)
        (apply-k k (set-box! boks val))]
      [add-to-global-k (var k)
        (apply-k k (add-to-global var var val))]
      [exit-k () val]
        )))

   
  

;-------------------+
;                   |
;    PARSER         |
;                   |
;-------------------+


; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.
; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr);added

(define parse-exp         
  (lambda (datum)
    (cond
      [(symbol? datum) (var-exp datum)]
      [(number? datum) (lit-exp datum)]
      [(string? datum) (lit-exp datum)]
      [(vector? datum) (lit-exp datum)]
      [(boolean? datum) (lit-exp datum)]
      [(char? datum) (lit-exp datum)]
      [(null? datum) (lit-exp datum)] 
      [(list? datum)
      (cond         
        [(andmap (lambda (x);elements in the list are all the same lit type
                  (ormap 
                    (lambda (pred) (pred x))
                    (list number? string? vector? boolean?))) datum) (lit-exp datum)]
        [(eqv? (1st datum) 'quote) (lit-exp (2nd datum))]
        [(eqv? (1st datum) 'lambda)
        (cond
          [(not (> (length datum) 2))
            (eopl:error 'parse-exp "Error in parse-exp: incorrect length:" datum)]
          [(symbol? (2nd datum)) 
            (lambda-sym-exp (2nd datum) (map parse-exp (cddr datum)))]
          [(and (not (list? (2nd datum))) (pair? (2nd datum)))
            (lambda-exp-improper (2nd datum) (map parse-exp (cddr datum)))]
          [(not (andmap symbol? (2nd datum)))
           (eopl:error 'parse-exp "invalid arguement" datum)]
          [else 
            (lambda-exp (map parse-exp (2nd  datum)) (map parse-exp (cddr datum)))])]
          
          [(eqv? (1st datum) 'let)
          (cond
            [(< (length datum) 3)
            (eopl:error 'parse-exp "incorrect length: ~s" datum)]
            [(and (symbol? (2nd datum)) (andmap symbol? (map 1st (3rd datum))))
            (named-let-exp (2nd datum) (map 1st (3rd datum)) (map parse-exp (map 2nd (3rd datum))) (map parse-exp (cdddr datum)))]
            [(not (list? (2nd datum)))
            (eopl:error 'parse-exp "not all proper list: ~s" (2nd datum))]
            [(not (andmap list? (2nd datum)))
            (eopl:error 'parse-exp "not a proper list: ~s" (2nd datum))]
            [(not (andmap (lambda (x) (= 2 (length x))) (2nd datum)))
            (eopl:error 'parse-exp "not all length 2: ~s" (2nd datum))]
            [(not (andmap symbol? (map 1st (2nd datum))))
            (eopl:error 'parse-exp "first members must be symbols: ~s" datum)]
            [else
             (let-exp (map list (map parse-exp (map car (2nd datum))) (map parse-exp (map 2nd (2nd datum)))) (map parse-exp (cddr datum)))])]
          [(eqv? (1st datum) 'letrec)
           (cond
            [(not (> (length datum) 2))
            (eopl:error 'parse-exp "incorrect length: ~s" datum)]
            [(not (list? (2nd datum)))
            (eopl:error 'parse-exp "invalid idss: ~s" (2nd datum))]
            [(not (andmap list? (2nd datum)))
            (eopl:error 'parse-exp "invalid arguments for idss: ~s" (2nd datum))]
            [(not (andmap (lambda (x) (= 2 (length x))) (2nd datum)))
            (eopl:error 'parse-exp "not all length 2: ~s" (2nd datum))]
            [(not (andmap symbol? (map 1st (2nd datum))))
            (eopl:error 'parse-exp "first members must be symbols: ~s" datum)]
            [else (letrec-exp (map 1st (2nd datum));names
                              (map 2nd (map 2nd (2nd datum)));idss
                              (map list (map parse-exp (map 3rd (map 2nd (2nd datum)))));bodiess
                              (map parse-exp (cddr datum)))])];'letrec-body
          [(eqv? (1st datum) 'let*)
          (cond
            [(< (length datum) 3)
            (eopl:error 'parse-exp "incorrect length: ~s" datum)]
            [(not (list? (2nd datum)))
            (eopl:error 'parse-exp "not all proper list: ~s" (2nd datum))]
            [(not (andmap list? (2nd datum)))
            (eopl:error 'parse-exp "not a proper list: ~s" (2nd datum))]
            [(not (andmap (lambda (x) (= 2 (length x))) (2nd datum)))
            (eopl:error 'parse-exp "not all length 2: ~s" (2nd datum))]
            [(not (andmap symbol? (map 1st (2nd datum))))
            (eopl:error 'parse-exp "first members must be symbols: ~s" datum)]
            [else
             (let*-exp (map list (map parse-exp (map car (2nd datum))) (map parse-exp (map 2nd (2nd datum)))) (map parse-exp (cddr datum)))])]
          [(eqv? (1st datum) 'if)
          (cond
            [(= 2 (length datum))
            (eopl:error 'parse-exp "missing then or else clauses: ~s" datum)]
            [(= 3 (length datum))
            (if-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)))]
            [(> (length datum) 4)
            (eopl:error 'parse-exp "too many parts: ~s" datum)]
            [else
            (if-else-exp (parse-exp (2nd datum))
              (parse-exp (3rd datum))
              (parse-exp (4th datum)))])]
          [(eqv? (1st datum) 'and)
            (and-exp (map parse-exp (cdr datum)))]
          [(eqv? (1st datum) 'or)
            (or-exp (map parse-exp (cdr datum)))]
          [(eqv? (1st datum) 'cond)
            (cond-exp (map (lambda (x) (list (parse-exp (1st x)) (parse-exp (2nd x)))) (cdr datum)))]
          [(eqv? (1st datum) 'case)
            (case-exp (parse-exp (2nd datum)) (map parse-exp (cddr datum)))]
          [(eqv? (1st datum) 'begin)
            (begin-exp (map parse-exp (cdr datum)))]
          [(eqv? (1st datum) 'while)
            (while-exp (parse-exp (2nd datum)) (begin-exp (map parse-exp (cddr datum))))]
          [(eq? (1st datum) 'set!)
          (cond
            [(> (length datum) 3)
            (eopl:error 'parse-exp "Error in parse-exp: set!: Too many parts: " datum)]
            [(< (length datum) 2)
            (eopl:error 'parse-exp "Error in parse-exp: set!: missing parts: " datum)]
            [(and (eq? (length datum) 3) (symbol? (2nd datum)))                       
              (set!-exp (2nd datum) (parse-exp (3rd datum)))]
            [else
              (eopl:error 'parse-exp "Error in parse-exp: set!: " datum)])]
          [(eq? (1st datum) 'define)
          (cond 
            [(> (length datum) 3)
            (eopl:error 'parse-exp "Error in parse-exp: define!: Too many parts: " datum)]
            [(< (length datum) 2)
            (eopl:error 'parse-exp "Error in parse-exp: define!: missing parts: " datum)]
            [(and (eq? (length datum) 3) 
              (symbol? (2nd datum)))
              (define-exp (2nd datum) (parse-exp (3rd datum)))]
            [else 
            (eopl:error 'parse-exp "Error in parse-exp: define!: " datum)])]
          [else (app-exp (parse-exp (1st datum))
          (map parse-exp (cdr datum)))])]
      [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))


(define unparse-exp
  (lambda (exp)
    (cases expression exp
      [var-exp (id) id]
      [lit-exp (id) id]
      [lambda-exp (id body)
        (cond
          [(null? id)
            (cons 'lambda (cons (map unparse-exp id) (map unparse-exp body)))]
          [(symbol? (car id))
            (cons 'lambda (cons (unparse-exp id) (map unparse-exp body)))]
          [else
            (cons 'lambda (cons (map unparse-exp id) (map unparse-exp body)))])]
      [let-exp (vars body)
        (cons 'let 
              (cons (map list (map unparse-exp (map 1st vars)) (map unparse-exp (map 2nd vars)))
                    (map unparse-exp body)))]
      [letrec-exp (vars body)
        (cons 'letrec (cons (map list (map unparse-exp (map 1st vars)) (map unparse-exp (map 2nd vars))) (map unparse-exp body)))]
      [let*-exp (vars body)
        (cons 'let* (cons (map list (map unparse-exp (map 1st vars)) (map unparse-exp (map 2nd vars))) (map unparse-exp body)))]
      [set!-exp (id body)
        (list 'set! (unparse-exp id) (unparse-exp body))]
      [if-exp (test result)
        (list 'if (unparse-exp test) (unparse-exp result))]
      [if-else-exp (test thencase elsecase)
        (list 'if (unparse-exp test) (unparse-exp thencase) (unparse-exp elsecase))]
      [app-exp (rator rand)
        (cons (unparse-exp rator)
          (map unparse-exp rand))])))

;-------------------+
;                   |
;   ENVIRONMENTS    |
;                   |
;-------------------+





; Environment definitions for CSSE 304 Scheme interpreter.  
; Based on EoPL sections 2.2 and 2.3

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (map box vals) env)))

(define extend-env-recursively
  (lambda (names idss bodiess old-env)
    (recursively-extended-env-record
      names idss bodiess old-env)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (xsym) (eqv? sym xsym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
     ((null? ls) #f)
     ((pred (car ls)) 0)
     (else (let ((list-index-r (list-index pred (cdr ls))))
       (if (number? list-index-r)
     (+ 1 list-index-r)
     #f))))))

(define apply-init-env
  (lambda (sym succeed fail)
    (let ((pos (list-find-position sym *prim-proc-names*)))
          (if (number? pos)
            (succeed (list-ref (3rd init-env) pos))
            (fail)))))

(define apply-env-ref
  (lambda (env sym succeed fail) ; succeed and fail are "callback procedures, 
    (cases environment env       ;  succeed is appluied if sym is found, otherwise 
      [empty-env-record ()       ;  fail is applied.
        (check-global-null global-env sym succeed (lambda (x) (apply-init-env x succeed fail)))]
      [extended-env-record (syms vals env)
        (let ((pos (list-find-position sym syms)))
          (if   (number? pos)
        (succeed (list-ref vals pos))
        (apply-env-ref env sym succeed fail)))]
      [recursively-extended-env-record
        (names idss bodiess old-env)
        (let ([pos (list-find-position sym names)])
          (if (number? pos)
            (let ([id (list-ref idss pos)])
              (if (and (not (list? id)) (pair? id))
                (box (closure-improper id (list-ref bodiess pos) env))
                (box (closure id (list-ref bodiess pos) env))))
            (apply-env-ref old-env sym succeed fail)))])))

(define apply-env
  (lambda (env sym succeed fail)
    (unbox (apply-env-ref env sym succeed fail))))






;-----------------------+
;                       |
;   SYNTAX EXPANSION    |
;                       |
;-----------------------+



; To be added later

(define syntax-expand
  (lambda (exp)
    (cases expression exp
      [let-exp (vars body)
        (app-exp (lambda-exp (map 1st vars) (map syntax-expand body)) (map 2nd vars))]
      [let*-exp (vars body)
        (syntax-expand (let*-loop vars body))]
      [begin-exp (body)
        (app-exp (lambda-exp '() (map syntax-expand (2nd exp))) '())]
      [cond-exp (body)
        (cond-loop body)]
      [case-exp (arg body)
        (syntax-expand (let-exp (list (list (var-exp 'x) arg)) (list (case-loop arg body))))]
      [and-exp (body)
        (if (null? body)
           (lit-exp #t)
           (if-else-exp (syntax-expand (car body)) (cons 'and-exp (list (cdr body))) (lit-exp #f)))]
      [named-let-exp (name ids bodies named-let-bodies)
        (letrec-exp (list name) (list ids) (list named-let-bodies) (list (app-exp (var-exp name) bodies)))]
      [set!-exp (vars body)
        (set!-exp vars (syntax-expand body))]
      [define-exp (vars body)
        (define-exp vars (syntax-expand body))]
      [else exp])))


(define cond-loop
  (lambda (body)
    (if (null? (cdr body))
        (if-exp (caar body) (cadar body))
        (if-else-exp (caar body) (cadar body) (cond-loop (cdr body))))))

(define case-loop
  (lambda (condi body)
    (if (= 1 (length body))
        (1st (caddar body))
        (if-else-exp [app-exp (var-exp 'member) (list (var-exp 'x) (cadar body))]
                     (1st (caddar body)) (case-loop condi (cdr body))))))

(define case-loop
  (lambda (arg body)
    (if (null? (cdr body))
        (1st (caddar body))
        (if-else-exp (app-exp (var-exp 'member) (list (var-exp 'x) (cadar body)))
         (1st (caddar body)) (case-loop arg (cdr body))))))

(define let*-loop
  (lambda (vars body)
    (if (null? vars)
      (let-exp '() body)
      (let-exp (list (car vars)) (list (syntax-expand (let*-exp (cdr vars) body)))))))




;-------------------+
;                   |
;   INTERPRETER    |
;                   |
;-------------------+

; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form (empty-env) (exit-k))))

; eval-exp is the main component of the interpreter

(define eval-bodies;changed
  (lambda (bodies env k)
    (if (null? (cdr bodies))
        (eval-exp (syntax-expand (car bodies)) env k)
        (eval-exp (car bodies) env (eval-bodies-k (cdr bodies) env k)))))
        ;(begin
        ;  (eval-exp (1st bodies) env)
        ;  (eval-bodies (cdr bodies) env)))))

(define eval-exp
  (lambda (exp env k)
    (cases expression exp
      [lit-exp (apply-k k datum)];changed
      [var-exp (id);changed
        (apply-env env id; look up its value.
           k
           (lambda () (eopl:error 'apply-env ; procedure to call if it is not in env
              "variable not found in environment: ~s"
         exp)))]
      [if-exp (bool thencase);changed
        (if (equal? bool '(var-exp else))
          (eval-exp thencase env k)
          (eval-exp bool env (if-k thencase env k)))]
      [if-else-exp (bool thencase elsecase);changed
        (eval-exp bool env (if-else-k thencase elsecase env k))]
      [lambda-exp (id body);changed
        (apply-k k (closure (map 2nd id) body env))]
      [lambda-sym-exp (id body);changed
        (apply-k k (closure-sym (list id) body env))]
      [lambda-exp-improper (id body);changed
        (apply-k k (closure-improper id body env))]
      [let-exp (vars body);changed
        (eval-exp (syntax-expand exp) env k)]
      [app-exp (rator rands);changed
        (eval-exp rator env (rator-k rands env k))]
      [and-exp (body);changed
        (eval-exp (syntax-expand (list 'and-exp body)) env k)]
      [or-exp (body);changed
        (if (null? body)
           (apply-k k #f)
           (eval-exp (car body) env (or-k (cdr body) env k)))]
      [begin-exp (body);changed
        (eval-exp (syntax-expand exp) env k)]
      [while-exp (test body);changed
        (eval-exp test env (while-k test body env k))]
      [letrec-exp (names idss bodiess letrec-bodies);k added
        (eval-bodies letrec-bodies
         (extend-env-recursively
          names idss (map list (map syntax-expand (map car bodiess))) env) k)]
      [set!-exp (vars body);changed
        (apply-k (set!-k body env k)
          (apply-env-ref
            env
            vars
            (set!-k body env k)
            (lambda () (eopl:error 'apply-env "variable not found" vars))))]
      [define-exp (vars body);changed
        (eval-exp (syntax-expand body) env (add-to-global-k vars k))]
        ;(add-to-global (eval-exp (syntax-expand body) env) vars)]
      [named-let-exp (names ids bodies named-let-bodies);changed
        (eval-exp (syntax-expand exp) env k)]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands env k)
    (map-cps (lambda (x new-k) (eval-exp (syntax-expand x) env new-k)) rands k)))

(define map-cps
  (lambda (proc ls k)
    (if (null? ls)
      (apply-k k '())
      (map-cps proc (cdr ls) (id-k (lambda (cdr-res)
                                      (proc (car ls) (id-k (lambda (car-res)
                                                            (apply-k k (cons car-res cdr-res)))))))))))
;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args k)
    (cases proc-val proc-value
      [continuation-proc (cp) (apply-k cp (1st args))];added
      [exit-proc ()
        (apply-k (exit-k) args)]
      [prim-proc (op) (apply-prim-proc op args k)];changed
      ; You will add other cases
      [closure (id bodies env);changed
        (eval-bodies bodies (extend-env id args env) k)]
      [closure-sym (id bodies env);changed
        (eval-bodies bodies (extend-env id (list args) env) k)]
      [closure-improper (id bodies env)
        (let ([my-list (letrec ([helper (lambda (x ls)
                                      (cond [(symbol? (cdr x)) (list (list (car x) (cdr x)) (list (car ls) (cdr ls)))]
                                            [(pair? (cdr x)) (let ([result (helper (cdr x) (cdr ls))])
                                                              (list (cons (car x) (car result)) (cons (car ls) (cadr result))))]))])
                     (helper id args))])
          (eval-bodies bodies (extend-env (1st my-list) (2nd my-list) env) k))]
      [else (eopl:error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define *prim-proc-names* '(+ - * / add1 sub1 cons = < > <= >= not zero? car cdr caar cadr cdar cddr caaar cadar cdaar cdddr caddr caadr cddar 
  list null? assq eq? equal? atom? length list->vector list? pair? procedure? vector->list vector make-vector vector-ref vector? number? symbol? 
  set-car! set-cdr! vector-set! display newline apply map member quotient list-tail eqv? append call/cc exit-list))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
     (empty-env)))

(define global-env (list (2nd init-env) (3rd init-env)))

(define reset-global-env
  (lambda () (set! global-env (list (2nd init-env) (3rd init-env)))))

(define check-global-null
  (lambda (env sym succeed fail)
    (let ((pos (list-find-position sym (car global-env))))
          (if (number? pos)
            (succeed (list-ref (2nd global-env) pos))
            (fail)))))

(define add-to-global
  (lambda (exp sym)
    (set! global-env (list (cons sym (1st global-env)) (cons (box exp) (2nd global-env))))))
; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
  (lambda (prim-proc args)
    (case prim-proc
        [(+) (apply-k k (apply + args))]
        [(-) (apply-k k (apply - args))]
        [(*) (apply-k k (apply * args))]
        [(/) (apply-k k (apply / args))]
        [(add1) (apply-k k (+ (1st args) 1))]
        [(sub1) (apply-k k (- (1st args) 1))]
        [(cons) (apply-k k (cons (1st args) (2nd args)))]
        [(=) (apply-k k (apply = args))]
        [(<) (apply-k k (apply < args))]
        [(>) (apply-k k (apply > args))]
        [(<=) (apply-k k (apply <= args))]
        [(>=) (apply-k k (apply >= args))]
        [(not) (apply-k k (not (1st args)))]
        [(zero?) (apply-k k (= 0 (1st args)))]
        [(car) (apply-k k (car (1st args)))]
        [(cdr) (apply-k k (cdr (1st args)))]
        [(caar) (apply-k k (car (car (1st args))))] 
        [(caaar) (apply-k k (car (car (car (1st args)))))]
        [(cddr) (apply-k k (cdr (cdr (1st args))))] 
        [(cdddr) (apply-k k (cdr (cdr (cdr (1st args)))))]
        [(cadr) (apply-k k (cadr (1st args)))]
        [(cdar) (apply-k k (cdar (1st args)))]
        [(caaar) (apply-k k (caaar (1st args)))]
        [(cadar) (apply-k k (cadar (1st args)))]
        [(cdaar) (apply-k k (cdaar (1st args)))]
        [(cdddr) (apply-k k (cdddr (1st args)))]
        [(caddr) (apply-k k (car (cdr (cdr (1st args)))))] 
        [(cdr) (apply-k k (cdr (1st args)))]
        [(caadr) (apply-k k (car (car (cdr args))))] 
        [(cadar) (apply-k k (car (cdr (car (1st args)))))]
        [(cddar) (apply-k k (cdr (cdr (car (1st args)))))]
        [(list) (apply-k k (apply list args))]
        [(null?) (apply-k k (apply null? args))]
        [(assq) (apply-k k (apply assq args))]
        [(eq?) (apply-k k (if (null? (cdr args)) 
                            (error 'apply-prim-proc "eq? requires 2 args")
                            (eq? (1st args) (2nd args))))]
        [(equal?) (apply-k k (if (null? (cdr args)) 
                            (error 'apply-prim-proc "equal? requires 2 args")
                            (equal? (1st args) (2nd args))))]
        [(atom?) (apply-k k (not (pair? args)))]
        [(length) (apply-k k (apply length args))]
        [(list->vector) (apply-k k (apply list->vector args))]
        [(list?) (apply-k k (apply list? args))]
        [(pair?) (apply-k k (apply pair? args))]
        [(procedure?) (apply-k k (apply proc-val? args))]
        [(vector->list) (apply-k k (apply vector->list args))]
        [(vector) (apply-k k (apply vector args))]
        [(make-vector) (apply-k k (if (number? (1st args))
                            (if (null? (cdr args))
                                (make-vector (1st args))
                                (make-vector (1st args) (2nd args)))))]
        [(vector-ref) (apply-k k (vector-ref (1st args) (2nd args)))]
        [(vector?) (apply-k k (apply vector? args))]
        [(number?) (apply-k k (if (= (length args) 1)
                            (number? (1st args))))]
        [(symbol?) (apply-k k (if (= (length args) 1)
                            (symbol? (1st args))))]
        [(set-car!) (apply-k k (set-car! (1st args) (2nd args)))]
        [(set-cdr!) (apply-k k (set-cdr! (1st args) (2nd args)))]
        [(vector-set!) (apply-k k (vector-set! (1st args) (2nd args) (3rd args)))]
        [(display) (apply-k k (apply display args))]
        [(newline) (apply-k k (apply newline args))]
        [(apply) (apply-k k (apply apply-proc args))]
        [(map) (apply-k k (apply map (lambda (x) (apply-proc (1st args) (list x))) (cdr args)))]
        [(member) (apply-k k (member (1st args) (2nd args)))]
        [(quotient) (apply-k k (apply quotient args))]
        [(list-tail) (apply-k k (apply list-tail args))]
        [(eqv?) (apply-k k (apply eqv? args))]
        [(append) (apply-k k (apply append args))]
        [(call/cc) (apply-proc (1st args) (list (continuation-proc k)) k)]
        [(exit-list) (apply-k (exit-proc) args (exit-k))]
        [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-proc)])))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (syntax-expand (parse-exp x)))))



