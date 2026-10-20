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


(define top-env (list (Bind 'true (BoolV #t)) (Bind 'false (BoolV #f))
                      (Bind '+ (PrimV '+)) (Bind '- (PrimV '-))
                      (Bind '* (PrimV '*)) (Bind '/ (PrimV '/))
                      (Bind '<= (PrimV '<=)) (Bind 'substring (PrimV 'substring))
                      (Bind 'strlen (PrimV 'strlen)) (Bind 'equal? (PrimV 'equal?))
                      (Bind 'error (PrimV 'error))))

;not sure if we need this
(define extend-env cons)

; ----------------------------------------Serialize -------------------------------------------

;takes in a value and returns a serialized string
(define (serialize [val : Value]) : String
  (match val
    [(BoolV b) (if b "true" "false")]
    [(NumV n) (~v n)]
    [(StrV s) (~v s)]
    [(CloV _ _ _) "#<procedure>"]
    [(PrimV _ ) "#<primop>"]))


; ----------------------------------------Look Up for Variable in Environment-------------------------------------------------
;takes in a symbol and an environment and it looks into the environment
;and tries to look if the given symbol is inside the environment
;and then it returns the value associated with the symbol
(define (lookup [sym : Symbol] [env : Env]) : Value
   (cond
      [(empty? env) (error 'lookup "DRMG4: Binding was not found got ~v" sym)]
      [(symbol=? sym (Bind-name (first env))) (Bind-val (first env))]
      [else (lookup sym (rest env))]))


;------------------------------- Helper func to find duplicate variables ----------------------

;; has-duplicates? checks whether a list contains repeated symbols.
(define (has-duplicates? [symbols : (Listof Symbol)]) : Boolean
  (if (empty? symbols)
      #f
      (if (member (first symbols) (rest symbols))
          #t
          (has-duplicates? (rest symbols))) ))

(check-equal? (has-duplicates? '(x y z)) #f)
(check-equal? (has-duplicates? '(x y x)) #t)
(check-equal? (has-duplicates? '()) #f)
; -----------------------------------Parser---------------------------------------------------

;---------- Parse Multiple-----
;takes in a list of Sexp and parses it into a list of ExprC
(define (parse-multiple [exprs : (Listof Sexp)]) : (Listof ExprC)
  (match exprs
    ['() '()]
    [(cons f r) (cons (parse f) (parse-multiple r))]))

;-----------------------------
;Takes in a Sexp and parses it into an ExprC
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? n) (NumC n)]
    [(? string? s) (StrV s)]
    [(? symbol? sym) (IdC sym)]
    [(list 'if condition ifTrue ifFalse) (If (parse condition) (parse ifTrue) (parse ifFalse))]
    [(list 'if _ ...)
     (error 'parse "DRMG4: Invalid syntax for if")] 
    [(list '% (? symbol? params) ... '-> body)
         (if (has-duplicates? (cast params (Listof Symbol)))
             (error 'parse "DRMG4: Duplicate function parameters")
             (LamC (map IdC (cast params (Listof Symbol)))
                   (parse body)))] 
    [(list 'local (list (? symbol? vars) '= expr) ... 'in expr2)
      (AppC (LamC (map IdC (cast vars (Listof Symbol))) (parse expr2)) (parse-multiple (cast expr (Listof Sexp))))]
    [(list fun args ...) (AppC (parse fun) (map parse args))]
    [_ (error 'parse "DRMG4: Invalid syntax")]))


;------- interp helper for multiple arguements ---------

;Takes in a list of args and the environment and it evaluates the args
;into values using interp
(define (interp-args [args : (Listof ExprC)] [env : Env]) : (Listof Value)
  (match args
    ['()'()]
    [(cons first rest)
     (cons (interp first env)
           (interp-args rest env))]))

;------------------------ Interp for Primitive Values ----------------------------------------

;takes in a symbol and a list of values and it performs an operation based
;on the symbol that was given
(define (interp-prim [op : Symbol] [args : (Listof Value)]) : Value
  (match op
    ['+
     (match args
       [(list (NumV a) (NumV b)) (NumV (+ a b))]
       [_ (error 'interp-prim "DRMG4: Expected two numbers")])]
    ['-
     (match args
       [(list (NumV a) (NumV b)) (NumV (- a b))]
       [_ (error 'interp-prim "DRMG4: Expected two numbers")])]
    ['*
     (match args
       [(list (NumV a) (NumV b)) (NumV (* a b))]
       [_ (error 'interp-prim "DRMG4: Expected two numbers")])]
    ['/
     (match args
       [(list (NumV a) (NumV b))
        (if (zero? b)
            (error 'interp-prim "DRMG4: Division by zero")
            (NumV (/ a b)))]
       [_ (error 'interp-prim "DRMG4: Expected two numbers")])]
    ['<=
     (match args
       [(list (NumV a) (NumV b)) (BoolV (<= a b))]
       [_ (error 'interp-prim "DRMG4: Expected two numbers")])]
    ['strlen
     (match args
       [(list (StrV s)) (NumV (string-length s))]
       [_ (error 'interp-prim "DRMG4: strlen expects one string")])]
    ['substring
     (match args
       [(list (StrV s) (NumV n1) (NumV n2))
        (cond
          [(not (and (exact-nonnegative-integer? n1)
                     (exact-nonnegative-integer? n2)))
           (error 'interp-prim "DRMG4: Invalid substring indices")]
          [(< n2 n1)
           (error 'interp-prim "DRMG4: Second input is less than first input")] 
          [(> n2 (string-length s))
           (error 'interp-prim "DRMG4: Range exceeds string length ")]
          [else (StrV (substring s n1 n2))])]
       [_ (error 'interp-prim "DRMG4: Invalid substring arguments")])] 
    ['equal?
     (match args
       [(list v1 v2)
        (BoolV
         (match* (v1 v2)
           [((NumV a) (NumV b)) (= a b)]
           [((BoolV a) (BoolV b)) (equal? a b)]
           [((StrV a) (StrV b)) (string=? a b)]
           [(_ _) #f]))]
       [_ (error 'interp-prim "DRMG4: equal? expects two arguments")])]
    ['error
     (match args
       [(list (StrV s)) (error s)]
       [_ (error 'interp-prim "DRMG4: error expects one string argument")])]
    [_ (error 'interp-prim "DRMG4: Unknown primitive operator")])) 

    
; ------------------------------- Interp ------------------------------------------------

;takes in an exprC and an environment and it uses the
;environment to interpret the expression into a value
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
       [_ (error 'interp "DRMG4: condition is not a boolean value")])]
    [(LamC args body) (CloV (map IdC-sym args) body env)]
    [(AppC func args)
     (define fundef (interp func env))
             (match fundef
               ; Interp returns Value so we have to match to see if its specifically CloV
               [(CloV params body closure-env)
                (cond
                  ;if the lengths of arguements are right we interp the values,
                  ;making in bindings, and create the new env
                  [(equal? (length params) (length args))
                   (define arg-values (interp-args args env))
                   (define bindings (map Bind params arg-values))
                   (define new-env (append bindings closure-env))
                   (interp body new-env)]
                  [else (error 'interp "DRMG4: Number of Arguments are not equal")])]
               [(PrimV op) (interp-prim op (interp-args args env))]
               [_ (error 'interp "DRMG4: Attempted to call a non-function")])]))

;----------------------------- top interp -------------------------------

;takes in a sexp and returns a serialized string containing a value
(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))  



;make sure to move ALL yes ALL test cases here... ( Test cases?? what is that?? )


;-------------------------------- Testing -----------------------------

;------------------------ Look Up Function -------------------

(check-equal? (lookup 'b (list (Bind 'a (NumV 5)) (Bind 'b (NumV 12)))) (NumV 12))
(check-exn (regexp (regexp-quote "DRMG4: Binding was not found")) (lambda () (lookup 'c (list (Bind 'a (NumV 5)) (Bind 'b (NumV 12))))))

(check-equal? (lookup '+ top-env) (PrimV '+))
(check-equal? (lookup 'x (list (Bind 'x (StrV "test")))) (StrV "test"))


; Top-interp tests

(check-equal? (top-interp "hi") "\"hi\"")
(check-equal? (top-interp 'true) "true")
(check-equal? (top-interp 'false) "false")
(check-equal? (top-interp '{if {<= {* 5 2} 11} 10 11}) "10")
(check-equal? (top-interp '{local [x = 4] in {+ x 4}}) "8")
(check-equal? (top-interp '{local [z = {+ 9 14}] [y = 98] in {+ z y}}) "121")
(check-equal? (top-interp '{% x -> {+ x 1}}) "#<procedure>")
(check-equal? (top-interp '{{% x -> {+ x 1}} 4}) "5")
(check-equal? (top-interp '{{% -> {+ 4 10}}}) "14")
(check-equal? (top-interp '({% x -> {+ x 1}} {* 2 10})) "21")


;----------------------- Serialize Tests----------------------
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

;---------------------- Parser Test ---------------------------
;Basic Test
(check-equal? (parse 4) {NumC 4})
(check-equal? (parse "test") {StrV "test"})
(check-equal? (parse 'Hello) {IdC 'Hello})
(check-equal? (parse '{if true 10 20}) (If (IdC 'true) (NumC 10) (NumC 20)))
(check-equal? (parse '{% x y -> x}) (LamC (list (IdC 'x) (IdC 'y)) (IdC 'x)))
(check-equal? (parse '{+ 2 3}) (AppC (IdC '+) (list (NumC 2) (NumC 3))))
(check-equal? (parse '{% x y -> x}) (LamC (list (IdC 'x) (IdC 'y)) (IdC 'x))) 
; Invalid Syntax 
(check-exn (regexp (regexp-quote "DRMG4: Invalid syntax")) (lambda () (parse '{})))
(check-exn (regexp (regexp-quote "DRMG4: Invalid syntax for if")) (lambda () (parse '{if true 4})))
;Error
(check-exn (regexp "DRMG4: Duplicate function parameters") (lambda () (parse '{% x x -> x})))

; ------------------------------- Interp Test Cases -----------------------
;Normal 
(check-equal? (interp (NumC 5) top-env) (NumV 5)) 
(check-equal? (interp (IdC 'x) (list (Bind 'x {NumV 4}))) (NumV 4))
;String
(check-equal? (interp (StrV "hello") top-env) (StrV "hello"))
;AppC
(check-equal? (interp (AppC (LamC (list (IdC 'x)) (IdC 'x)) (list (NumC 10))) {list (Bind 'x {NumV 10})})  (NumV 10))
(check-equal? (interp(AppC(LamC (list (IdC 'y)) (IdC 'x)) (list (NumC 10))) (list (Bind 'x {NumV 4}))) (NumV 4))
;IF 
(check-equal? (interp (If {AppC (IdC '<=) {list {NumC 2} {NumC 3}}} {NumC 4} {NumC 21}) top-env) {NumV 4})
(check-equal? (interp (If {AppC (IdC '<=) {list {NumC 10} {NumC 0}}} {NumC 2} {NumC 100}) top-env) {NumV 100})
(check-equal? (interp (If (IdC 'true) (NumC 1) (NumC 2)) top-env) {NumV 1})
(check-equal? (interp (If (IdC 'true) (NumC 10) (NumC 20)) top-env) (NumV 10))
(check-equal? (interp (If (IdC 'false) (NumC 10) (NumC 20)) top-env) (NumV 20))
(check-equal?
(interp (If (IdC 'true) (NumC 10) (AppC (IdC 'error) (list (StrV "custom error")))) top-env) (NumV 10))

(check-exn (regexp (regexp-quote "custom error"))
           (lambda ()
             (interp
              (If {AppC (IdC '<=) {list (NumC 4) (NumC 0)}}
                  (NumC 10) (AppC {IdC 'error}
                                  {list (StrV "custom error")})) top-env)))

(check-exn (regexp (regexp-quote "DRMG4: condition is not a boolean value"))
 (lambda () (interp (If (NumC 5) (NumC 10) (NumC 20)) top-env)))

;testing local
(check-equal? (interp (parse '{local {x = 4} {y = {+ 4 9}} in {* x y}}) top-env) {NumV 52})

;testing out of scope vars
(check-exn #rx"Binding was not found" (lambda () (interp (parse '{local {x = 4} {y = {+ 4 x}} in {* x y}}) top-env)))

;Extra 
(check-exn (regexp (regexp-quote "DRMG4: Number of Arguments are not equal")) (lambda () (interp (parse '{{% x y -> x} 5}) top-env)))
(check-exn (regexp "DRMG4: Attempted to call a non-function") (lambda () (interp (parse'{5 10}) top-env)))



;---------------- Interp Prims Test Cases -------------------------

; checking primitive operators 
(check-equal? (interp-prim '+ (list (NumV 2) (NumV 3))) (NumV 5)) 
(check-equal? (interp-prim '- (list (NumV 10) (NumV 3))) (NumV 7))
(check-equal? (interp-prim '- (list (NumV 3) (NumV 10))) (NumV -7))
(check-equal? (interp-prim '* (list (NumV 4) (NumV 3))) (NumV 12))
(check-equal? (interp-prim '* (list (NumV 5) (NumV 0))) (NumV 0))
(check-equal? (interp-prim '/ (list (NumV 12) (NumV 3))) (NumV 4))
(check-equal? (interp-prim '/ (list (NumV 5) (NumV 2))) (NumV 5/2))
(check-equal? (interp-prim '<= (list (NumV 3) (NumV 5))) (BoolV #t))
(check-equal? (interp-prim '<= (list (NumV 5) (NumV 3))) (BoolV #f))
(check-equal? (interp-prim '<= (list (NumV 4) (NumV 4))) (BoolV #t))

(check-equal? (interp-prim 'equal? (list (NumV 5) (NumV 5))) (BoolV #t))
(check-equal? (interp-prim 'equal? (list (NumV 5) (StrV "5"))) (BoolV #f))
(check-equal? (interp-prim 'equal? (list (StrV "hello") (StrV "hello"))) (BoolV #t))
(check-equal? (interp-prim 'equal? (list (BoolV #t) (BoolV #f))) (BoolV #f))


;checking primitive operator errors 
(check-exn (regexp (regexp-quote "DRMG4: Division by zero"))
 (lambda ()
   (interp-prim '/ (list (NumV 5) (NumV 0)))))

(check-exn
 (regexp "Expected two numbers" )
 (lambda ()
   (interp-prim '+ (list (NumV 2) (StrV "hello")))))

(check-exn
 (regexp "Expected two numbers" )
 (lambda ()
   (interp-prim '- (list (NumV 2) (StrV "hello")))))
 
(check-exn
 (regexp "Expected two numbers" )
 (lambda ()
   (interp-prim '* (list (NumV 2) (StrV "hello")))))
(check-exn
 (regexp "Expected two numbers" )
 (lambda ()  
   (interp-prim '/ (list (NumV 2) (StrV "hello")))))
(check-exn
 (regexp "Expected two numbers" )
 (lambda ()
   (interp-prim '<= (list (NumV 2) (StrV "hello")))))
(check-exn
 (regexp (regexp-quote "DRMG4: Expected two numbers"))
 (lambda ()
   (interp-prim '+ (list (NumV 2)))))

(check-exn
 (regexp (regexp-quote "DRMG4: Expected two numbers"))
 (lambda ()
   (interp-prim '* (list (NumV 2) (NumV 3) (NumV 4)))))

(check-exn
 (regexp (regexp-quote "DRMG4: Unknown primitive operator"))
 (lambda () 
   (interp-prim '? (list (NumV 2) (NumV 10)))))

;testing custom errors
(check-exn
 (regexp (regexp-quote "my custom error"))
 (lambda ()
   (interp-prim 'error (list (StrV "my custom error")))))

(check-exn
 (regexp (regexp-quote "DRMG4: error expects one string argument"))
 (lambda ()
   (interp-prim 'error (list (NumV 3)))))

;Checking string length function 
(check-equal? (interp-prim 'strlen (list (StrV "hello"))) (NumV 5))
(check-equal? (interp-prim 'strlen (list (StrV ""))) (NumV 0))
(check-equal? (interp-prim 'strlen (list (StrV "hello world"))) (NumV 11))

(check-exn
 (regexp "strlen expects one string" )
 (lambda ()
   (interp-prim 'strlen (list (NumV 5)))))

(check-exn
 (regexp (regexp-quote "DRMG4: Unknown primitive operator"))
 (lambda ()
   (interp-prim 'unknown (list (StrV "hello")))))
 
;Checks for substring function
; Substring tests
(check-equal? (interp-prim 'substring (list (StrV "hello") (NumV 1) (NumV 4))) (StrV "ell"))
(check-equal? (interp-prim 'substring (list (StrV "hello") (NumV 0) (NumV 5))) (StrV "hello"))
(check-equal? (interp-prim 'substring (list (StrV "hello") (NumV 0) (NumV 0))) (StrV ""))
(check-equal? (interp-prim 'substring (list (StrV "hello") (NumV 2) (NumV 2))) (StrV ""))
(check-equal? (interp-prim 'substring (list (StrV "hello") (NumV 2) (NumV 5))) (StrV "llo"))
  
; Negative starting index
(check-exn (regexp (regexp-quote "DRMG4: Invalid substring indices"))
 (lambda () (interp-prim 'substring (list (StrV "hello") (NumV -1) (NumV 2)))))
 
; Negative ending index
(check-exn (regexp (regexp-quote "DRMG4: Invalid substring indices"))
 (lambda () (interp-prim 'substring (list (StrV "hello") (NumV 1) (NumV -1)))))

; Ending index exceeds string length
(check-exn (regexp (regexp-quote "DRMG4: Range exceeds string length"))
 (lambda () (interp-prim 'substring (list (StrV "hello") (NumV 1) (NumV 6)))))

; Ending index is less than starting index
(check-exn (regexp (regexp-quote "DRMG4: Second input is less than first input")) (lambda ()
   (interp-prim 'substring (list (StrV "hello") (NumV 4) (NumV 2)))))

; Wrong number of arguments
(check-exn (regexp "Invalid substring arguments")
 (lambda () (interp-prim 'substring (list (StrV "hello") (NumV 1)))))

(check-exn(regexp (regexp-quote "DRMG4: Unknown primitive operator"))
 (lambda () (interp-prim 'function (list (StrV "hello") (NumV 1) (NumV 3)))))

;; equal? — numbers
(check-equal? (interp-prim 'equal? (list (NumV 5) (NumV 5))) (BoolV #t))
(check-equal? (interp-prim 'equal? (list (NumV 5) (NumV 6))) (BoolV #f))

;; equal? — booleans
(check-equal? (interp-prim 'equal? (list (BoolV #t) (BoolV #t))) (BoolV #t))
(check-equal? (interp-prim 'equal? (list (BoolV #t) (BoolV #f))) (BoolV #f))

;; equal? — strings
(check-equal? (interp-prim 'equal? (list (StrV "hello") (StrV "hello"))) (BoolV #t))
(check-equal? (interp-prim 'equal? (list (StrV "hello") (StrV "world"))) (BoolV #f))

;; equal? — different types
(check-equal? (interp-prim 'equal? (list (NumV 5) (StrV "5"))) (BoolV #f))
(check-equal? (interp-prim 'equal? (list (BoolV #t) (NumV 1))) (BoolV #f))

;; equal? — closures and primitive operators
(check-equal? (interp-prim 'equal? 
(list (CloV '() (NumC 5) top-env) (CloV '() (NumC 5) top-env))) (BoolV #f))

(check-equal? (interp-prim 'equal? (list (PrimV '+) (PrimV '+))) (BoolV #f))
;; Wrong number of arguments
(check-exn (regexp "interp-prim: DRMG4: equal\\? expects two arguments")
 (lambda () (interp-prim 'equal? (list (NumV 5)))))

(check-exn (regexp "equal\\? expects two arguments") (lambda () (interp-prim 'equal? (list (NumV 5)))))

;; Wrong number of arguments
(check-exn
 (regexp "equal\\? expects two arguments")
 (lambda ()
   (interp-prim 'equal? (list (NumV 5)))))

(check-exn (regexp "equal\\? expects two arguments") (lambda () (interp-prim 'equal? '())))