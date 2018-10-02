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
    (eval-exp form (empty-env))))

; eval-exp is the main component of the interpreter

(define eval-bodies
  (lambda (bodies env)
    (if (null? (cdr bodies))
        (eval-exp (car bodies) env)
        (begin
          (eval-exp (1st bodies) env)
          (eval-bodies (cdr bodies) env)))))

(define eval-exp
  (lambda (exp env)
    (cases expression exp
      [lit-exp (datum) datum]
      [var-exp (id)
        (apply-env env id; look up its value.
           (lambda (x) x) ; procedure to call if it is in the environment 
           (lambda () (eopl:error 'apply-env ; procedure to call if it is not in env
              "variable not found in environment: ~s"
         id)))]
      [if-exp (bool thencase)
        (if (equal? bool '(var-exp else))
          (eval-exp (syntax-expand thencase) env)
          (if (eval-exp bool env) (eval-exp (syntax-expand thencase) env)))]
      [if-else-exp (bool thencase elsecase)
        (if (eval-exp bool env)
          (eval-exp (syntax-expand thencase) env)
          (eval-exp (syntax-expand elsecase) env))]
      [lambda-exp (id body)
      (closure (map 2nd id) body env)]
      [lambda-sym-exp (id body)
        (closure-sym (list id) body env)]
      [lambda-exp-improper (id body)
      (closure-improper id body env)]
      [let-exp (vars body)
        (eval-exp (syntax-expand exp) env)]
      [app-exp (rator rands)
        (let ([proc-value (eval-exp rator env)]
              [args (eval-rands rands env)])
          (apply-proc proc-value args))]
      [and-exp (body)
        (eval-exp (syntax-expand (list 'and-exp body)) env)]
      [or-exp (body)
        (if (null? body)
           #f
           (let ([next (eval-exp (syntax-expand (car body)) env)])
             (if next next (eval-exp [or-exp (cdr body)] env))))]
      [begin-exp (body)
        (for-each (lambda (x) (eval-exp x env)) (map syntax-expand body))]
      [while-exp (test body)
        (letrec ([loop (lambda ()
                         (if (eval-exp test env)
                             (begin 
                               (eval-exp (syntax-expand body) env)
                               (loop))
                             ))])
          (loop))]
      [letrec-exp (names idss bodiess letrec-bodies)
        (eval-bodies letrec-bodies
         (extend-env-recursively
          names idss (map list (map syntax-expand (map car bodiess))) env))]
      [set!-exp (vars body)
        (if (not (expression? (eval-exp (var-exp vars) env)))
          (set-box! (apply-env-ref env vars (lambda (x) x)
                                           (lambda () (eopl:error 'apply-env "variable not found" vars)))
          (eval-exp (syntax-expand body) env)))]
      [define-exp (vars body)
        (add-to-global (eval-exp (syntax-expand body) env) vars)]
      [named-let-exp (names ids bodies named-let-bodies)
      (eval-exp (syntax-expand exp) env)]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-exp x env)) rands)))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args)]
      ; You will add other cases
      [closure (id bodies env)
      (eval-bodies bodies (extend-env id args env))]
      [closure-sym (id bodies env)
        (eval-bodies bodies (extend-env id (list args) env))]
      [closure-improper (id bodies env)
        (let ([my-list (letrec ([helper (lambda (x ls)
                                      (cond [(symbol? (cdr x)) (list (list (car x) (cdr x)) (list (car ls) (cdr ls)))]
                                            [(pair? (cdr x)) (let ([result (helper (cdr x) (cdr ls))])
                                                              (list (cons (car x) (car result)) (cons (car ls) (cadr result))))]))])
                     (helper id args))])
          (eval-bodies bodies (extend-env (1st my-list) (2nd my-list) env)))]
      [else (eopl:error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define *prim-proc-names* '(+ - * / add1 sub1 cons = < > <= >= not zero? car cdr caar cadr cdar cddr caaar cadar cdaar cdddr caddr caadr cddar 
  list null? assq eq? equal? atom? length list->vector list? pair? procedure? vector->list vector make-vector vector-ref vector? number? symbol? 
  set-car! set-cdr! vector-set! display newline apply map member quotient list-tail eqv? append))

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
;        [(+) (+ (1st args) (2nd args))]
;        [(-) (- (1st args) (2nd args))]
;        [(*) (* (1st args) (2nd args))]
;        [(/) (/ (1st args) (2nd args))]
        [(+) (apply + args)]
        [(-) (apply - args)]
        [(*) (apply * args)]
        [(/) (apply / args)]
        [(add1) (+ (1st args) 1)]
        [(sub1) (- (1st args) 1)]
        [(cons) (cons (1st args) (2nd args))]
;        [(=) (= (1st args) (2nd args))]
        [(=) (apply = args)]
        [(<) (apply < args)]
        [(>) (apply > args)]
        [(<=) (apply <= args)]
        [(>=) (apply >= args)]
        [(not) (not (1st args))]
        [(zero?) (= 0 (1st args))]
        [(car) (car (1st args))]
        [(cdr) (cdr (1st args))]
        [(caar) (car (car (1st args)))] [(caaar) (car (car (car (1st args))))]
        [(cddr) (cdr (cdr (1st args)))] [(cdddr) (cdr (cdr (cdr (1st args))))]
        [(cadr) (cadr (1st args))]
        [(cdar) (cdar (1st args))]
        [(caaar) (caaar (1st args))]
        [(cadar) (cadar (1st args))]
        [(cdaar) (cdaar (1st args))]
        [(cdddr) (cdddr (1st args))]
        [(caddr) (car (cdr (cdr (1st args))))] [(cdr) (cdr (1st args))]
        [(caadr) (car (car (cdr args)))] [(cadar) (car (cdr (car (1st args))))]
        [(cddar) (cdr (cdr (car (1st args))))]
        [(list) (apply list args)]
        [(null?) (apply null? args)]
        [(assq) (apply assq args)]
        [(eq?) (if (null? (cdr args)) 
                            (error 'apply-prim-proc "eq? requires 2 args")
                            (eq? (1st args) (2nd args)))]
        [(equal?) (if (null? (cdr args)) 
                            (error 'apply-prim-proc "equal? requires 2 args")
                            (equal? (1st args) (2nd args)))]
        [(atom?) (not (pair? args))]
        [(length) (apply length args)]
        [(list->vector) (apply list->vector args)]
        [(list?) (apply list? args)]
        [(pair?) (apply pair? args)]
        [(procedure?) (apply proc-val? args)]
        [(vector->list) (apply vector->list args)]
        [(vector) (apply vector args)]
        [(make-vector) (if (number? (1st args))
                            (if (null? (cdr args))
                                (make-vector (1st args))
                                (make-vector (1st args) (2nd args))))]
        [(vector-ref) (vector-ref (1st args) (2nd args))]
        [(vector?) (apply vector? args)]
        [(number?) (if (= (length args) 1)
                            (number? (1st args)))]
        [(symbol?) (if (= (length args) 1)
                            (symbol? (1st args)))]
        [(set-car!) (set-car! (1st args) (2nd args))]
        [(set-cdr!) (set-cdr! (1st args) (2nd args))]
        [(vector-set!) (vector-set! (1st args) (2nd args) (3rd args))]
        [(display) (apply display args)]
        [(newline) (apply newline args)]
        [(apply) (apply apply-proc args)]
        [(map) (apply map (lambda (x) (apply-proc (1st args) (list x))) (cdr args))]
        [(member) (member (1st args) (2nd args))]
        [(quotient) (apply quotient args)]
        [(list-tail) (apply list-tail args)]
        [(eqv?) (apply eqv? args)]
        [(append) (apply append args)]
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