#lang typed/racket
(require typed/rackunit)

; --------------- Type Definitions ----------------------
(define-type ExprC (U NumC NumV IdC StrV If LamC AppC))
(define-type Value (U NumV BoolV StrV CloV PrimV))
(define-type Binding Bind)
(define-type Env (Listof Bind))

;-------------------------- Struct Definitions ---------------------
(struct NumV ([num : Real]) #:transparent)
(struct NumC ([num : Real]) #:transparent)
(struct IdC ([sym : Symbol]) #:transparent)
(struct StrV ([str : String]) #:transparent)
(struct If ([expr : ExprC] [ifTrue : ExprC] [else : ExprC]) #:transparent)
(struct LamC ([arg : (Listof IdC)] [body : ExprC]) #:transparent)
(struct AppC ([fun : ExprC] [args : (Listof ExprC)]) #:transparent)
(struct PrimV ([op : Symbol]) #:transparent)
(struct BoolV ([boo : Boolean]) #:transparent)
(struct CloV ([args : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)
(struct Bind ([name : Symbol] [val : Value]) #:transparent)

;(struct local 
;(struct % ([ids : (Listof IdC)] [body : ExprC]) #:transparent)
;(struct CloV ([params : (Listof IdC)] [body : ExprC] [env : Env]) #:transparent)

;(define top-env empty)

;all prim operations should be added, nvm need to add error

(define top-env (list (Bind 'true (BoolV #t)) (Bind 'false (BoolV #f))
                      (Bind '+ (PrimV '+)) (Bind '- (PrimV '-))
                      (Bind '* (PrimV '*)) (Bind '/ (PrimV '/))
                      (Bind '<= (PrimV '<=)) (Bind 'substring (PrimV 'substring))
                      (Bind 'strlen (PrimV 'strlen)) (Bind 'equal? (PrimV 'equal?))))
(define extend-env cons)

; ----------------------------------------Serialize -------------------------------------------
; Added primitive ops  - M
(define (serialize [val : Value]) : String
  (match val
    [(BoolV b) (if b "true" "false")]
    [(NumV n) (~v n)]
    [(StrV s) (~v s)]
    [(CloV _ _ _) "#<procedure>"]
    [(PrimV _ ) "#<primop>"]))

; Test serializing numbers
(check-equal? (serialize (NumV 5)) "5")
(check-equal? (serialize (NumV -3)) "-3")
(check-equal? (serialize (NumV 2.5)) "2.5")

; Test serializing booleans
(check-equal? (serialize (BoolV #t)) "true")
(check-equal? (serialize (BoolV #f)) "false")

; Test serializing strings
(check-equal? (serialize (StrV "hello")) "\"hello\"")
(check-equal? (serialize (StrV "DRMG")) "\"DRMG\"")

; Test serializing closures
(check-equal?
 (serialize (CloV empty (NumV 5) empty))"#<procedure>")

; Test serializing primitive operators
(check-equal? (serialize (PrimV '+)) "#<primop>")
; -------------------------------------------------------------------------------------------------

;???

; ----------------------------------------Look Up for Variable in Environment-------------------------------------------------
(define (lookup [sym : Symbol] [env : Env]) : Value
    (cond
      [(empty? env) (error 'lookup "DRMG4: Binding was not found")]
      [(symbol=? sym (Bind-name (first env))) (Bind-val (first env))]
      [else (lookup sym (rest env))]))


(check-equal?(lookup 'b (list (Bind 'a (NumV 5)) (Bind 'b (NumV 12)))) (NumV 12))
(check-exn (regexp (regexp-quote " DRMG4: Binding was not found")) (lambda () (lookup 'c (list (Bind 'a (NumV 5)) (Bind 'b (NumV 12))))))

(check-equal? (lookup '+ top-env)(PrimV '+))
(check-equal? (lookup 'x (list (Bind 'x (StrV "test")))) (StrV "test"))

; -----------------------------------Parser---------------------------------------------------

(define (parse-multiple [exprs : (Listof Sexp)]) : (Listof ExprC)
  (match exprs
    ['() '()]
    [(cons f r) (cons (parse f) (parse-multiple r))]
  ))


;Takes in a Sexp and parses it into an ExprC

;	{ local [‹id›=‹expr›]* do ‹expr› }
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? n) (NumC n)]
    [(? string? s) (StrV s)]
    [(? symbol? sym) (IdC sym)]
    [(list 'if condition ifTrue ifFalse) (If (parse condition) (parse ifTrue) (parse ifFalse))]
    [(list '% (list (? symbol? params)...) '->  body ) (LamC (map IdC (cast params (Listof Symbol))) (parse body))] 

    ;desugaring version of local, idk if its do or in, instructions mix them around
    [(list 'local (list (list (? symbol? vars) '= expr) ...) 'in expr2)
     (AppC (LamC (map IdC (cast vars (Listof Symbol))) (parse expr2)) (parse-multiple (cast expr (Listof Sexp))))]
    [(list fun args ...) (AppC (parse fun) (map parse args))]
    [_ (error "catch all error")]
  ))

(check-equal? (parse 4) {NumC 4})
(check-equal? (parse "test") {StrV "test"})
(check-equal? (parse 'Hello) {IdC 'Hello})
(check-equal? (parse '{if true 10 20})(If (IdC 'true)(NumC 10) (NumC 20)))
(check-equal? (parse '{% (x y) -> x}) (LamC (list (IdC 'x) (IdC 'y)) (IdC 'x)))
(check-equal?(parse '{+ 2 3})(AppC (IdC '+)(list (NumC 2) (NumC 3))))
(parse '{local {{x = 4} {y = {+ 4 9}}} in {* x y}})

(parse '{if true 4})

;(parse '{{% (x) -> x} 10})
;------- interp helper ---------

;Takes in a list of args and a list of functions and it evaluates the args
;into values using interp
(define (interp-args [args : (Listof ExprC)] [env : Env]) : (Listof Value)
  (match args
    ['()'()]
    [(cons first rest)
     (cons (interp first env)
           (interp-args rest env))]))

;------------------------ Interp for Primitive Values ----------------------------------------


(define (interp-prim [op : Symbol] [args : (Listof Value)]) : Value
  (match args
    [(list (NumV a) (NumV b))
      (cond
        [(equal? op '+) (NumV (+ a b))]
        [(equal? op '-) (NumV (- a b))]
        [(equal? op '*) (NumV (* a b))]
        [(equal? op '/) (if (zero? b)(error 'interp-prim "DRMG4: Division by zero")(NumV (/ a b)))]
        [(equal? op '<=) (BoolV (<= a b))]
        [else (error 'interp-prim "DRMG4: Unknown primitive operator")]
        )]
     [(list (StrV s)) (if (equal? op 'strlen) (NumV (string-length s))
                             (error 'interp-prim "DRMG4: Unknown primitive operator"))]
    [(list (StrV s) (NumV n1) (NumV n2))
     (cond
       [(not(and (exact-nonnegative-integer? n1) (exact-nonnegative-integer? n2)))
             (error 'interp-prim "DRMG4: Unknown primitive operator")]
       [(not (string? s))
             (error 'interp-prim "DRMG4: Input it not a string")]
       [(<= (string-length s) (- n2 n1))
             (error 'interp-prim "DRMG4: Range Provided is Longer than string length")]
       [(< n2 n1)
              (error 'interp-prim "DRMG4: Second input is less than first input")]
       [else
          (StrV (substring s n1 n2)) ])]
         
     
    [_ (error 'interp-prim "DRMG4: Expected two numbers")]))
 
 
    

; checking primitive operators 
(check-equal?(interp-prim '+ (list (NumV 2) (NumV 3)))(NumV 5)) 
(check-equal? (interp-prim '- (list (NumV 10) (NumV 3))) (NumV 7))
(check-equal? (interp-prim '- (list (NumV 3) (NumV 10))) (NumV -7))
(check-equal? (interp-prim '* (list (NumV 4) (NumV 3))) (NumV 12))
(check-equal? (interp-prim '* (list (NumV 5) (NumV 0))) (NumV 0))
(check-equal? (interp-prim '/ (list (NumV 12) (NumV 3))) (NumV 4))
(check-equal? (interp-prim '/ (list (NumV 5) (NumV 2))) (NumV 5/2))
(check-equal? (interp-prim '<= (list (NumV 3) (NumV 5))) (BoolV #t))
(check-equal? (interp-prim '<= (list (NumV 5) (NumV 3))) (BoolV #f))
(check-equal? (interp-prim '<= (list (NumV 4) (NumV 4))) (BoolV #t))

;add test cases for rest of prim ops


  

;checking primitive operator errors 
(check-exn (regexp (regexp-quote "DRMG4: Division by zero"))
 (lambda ()
   (interp-prim '/ (list (NumV 5) (NumV 0)))))

(check-exn
 (regexp (regexp-quote "DRMG4: Expected two numbers"))
 (lambda ()
   (interp-prim '+ (list (NumV 2) (StrV "hello")))))

(check-exn
 (regexp (regexp-quote "DRMG4: Expected two numbers"))
 (lambda ()
   (interp-prim '- (list (NumV 2) (StrV "hello")))))
 
(check-exn
 (regexp (regexp-quote "DRMG4: Expected two numbers"))
 (lambda ()
   (interp-prim '* (list (NumV 2) (StrV "hello")))))
(check-exn
 (regexp (regexp-quote "DRMG4: Expected two numbers"))
 (lambda ()  
   (interp-prim '/ (list (NumV 2) (StrV "hello")))))
(check-exn
 (regexp (regexp-quote "DRMG4: Expected two numbers"))
 (lambda ()
   (interp-prim '<= (list (NumV 2) (StrV "hello")))))

(check-exn
 (regexp (regexp-quote "DRMG4: Unknown primitive operator"))
 (lambda () 
   (interp-prim '? (list (NumV 2) (NumV 10)))))

;Checking string length function 
(check-equal? (interp-prim 'strlen (list (StrV "hello"))) (NumV 5))
(check-equal? (interp-prim 'strlen (list (StrV ""))) (NumV 0))


; ------------------------------- Interp ------------------------------------------------

;(define (interp-if [condition : ExprC] 

(define (interp [e : ExprC] [env : Env]) : Value
  (match e
    [(IdC s) (lookup s env)]
    [(NumC n) (NumV n)]
    [(StrV s) (StrV s)]
    
    [(If condition ifTrue else)
     (define result (interp condition env))
     (match result
       [(BoolV #t) (interp ifTrue env)]
       [(BoolV #f) (interp else env)]
       [_ (error 'interp "DRMG4: condition is not a boolean value")])

     ]
    [(LamC args body) (CloV (map IdC-sym args) body env)]
    [(AppC func args)
     (define fundef (interp func env))
             (match fundef
               [(CloV params body closure-env) ; Interp returns Value so we have to match to see if its specifically CloV
                
                (cond
                  [(equal? (length params) (length args)) ; if the lengths of arguements are right we interp the values, making in bindings, and create the new env 
                   (define arg-values (interp-args args env))
                   (define bindings (map Bind params arg-values))
                   (define new-env (append bindings closure-env))
                   (interp body new-env)]
            
                  [else (error 'interp "DRMG4: Number of Arguments are not equal" args)])]
 
                 [(PrimV op) (interp-prim op (interp-args args env))]

               [_ (error 'interp "DRMG4: Attempted to call a non-function")])
             ]

  ))
#;(check-exn
 (regexp "DRMG4: Primitive operators not implemented yet")
 (lambda () (interp (AppC (IdC '+) (list (NumC 2) (NumC 3)))top-env)))

;(define top-env (list (Bind 'true (BoolV #t)) (Bind 'false (BoolV #f)) (Bind 'x {NumV 4})  ))
(check-equal? (interp (NumC 5) top-env) (NumV 5)) 
(check-equal? (interp (IdC 'x) (list (Bind 'x {NumV 4}))) (NumV 4))
(check-equal?
  (interp 
    (AppC (LamC (list (IdC 'x))
                (IdC 'x))
          (list (NumC 10)))
    {list (Bind 'x {NumV 10})})
  (NumV 10))

(check-equal?
  (interp
    (AppC
      (LamC (list (IdC 'y))
            (IdC 'x))
      (list (NumC 10)))
    (list (Bind 'x {NumV 4}))) 
  (NumV 4))

(check-equal? (interp (If {AppC (IdC '<=) {list {NumC 2} {NumC 3}}} {NumC 4} {NumC 21}) top-env) {NumV 4})
(check-equal? (interp (If {AppC (IdC '<=) {list {NumC 10} {NumC 0}}} {NumC 2} {NumC 100}) top-env) {NumV 100})
(check-equal? (interp (If (IdC 'true) (NumC 1) (NumC 2)) top-env) {NumV 1})

;testing local
(check-equal? (interp (parse '{local {{x = 4} {y = {+ 4 9}}} in {* x y}}) top-env) {NumV 52})

;testing out of scope vars
(check-exn #rx"Binding was not found" (lambda () (interp (parse '{local {{x = 4} {y = {+ 4 x}}} in {* x y}}) top-env)))


(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))


;make sure to move ALL yes ALL test cases here...



