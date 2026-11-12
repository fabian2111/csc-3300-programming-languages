#lang typed/racket
(require typed/rackunit)

;Full project implemented

; --------------- Type Definitions ----------------------
(define-type ExprC (U NumC IdC StrC If LamC AppC MutC RocalC))
(define-type Value (U NumV BoolV StrV CloV PrimV NullV ArrV))
(define-type Binding Bind)
(define-type Env (Listof Bind))

;-------------------------- Struct Definitions ---------------------
(struct NumV ([num : Real]) #:transparent)
(struct NumC ([num : Real]) #:transparent)
(struct IdC ([sym : Symbol]) #:transparent)
(struct StrC ([str : String]) #:transparent)
(struct StrV ([str : String]) #:transparent)
(struct If ([expr : ExprC] [ifTrue : ExprC] [else : ExprC]) #:transparent)
(struct LamC ([arg : (Listof IdC)] [body : ExprC]) #:transparent)
(struct AppC ([fun : ExprC] [args : (Listof ExprC)]) #:transparent)
(struct PrimV ([op : Symbol]) #:transparent)
(struct BoolV ([boo : Boolean]) #:transparent)
(struct CloV ([args : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)

;guessing at this, actually idk if ArrC is apart of ExprC, ArrV is def aprt of values tho
(struct ArrC ([values : (Listof ExprC)]) #:transparent)
(struct ArrV ([loc : Location] [len : Natural]) #:transparent)
(struct NullV () #:transparent)
;mutc (mutation) i think its like this
(struct MutC ([var : IdC] [val : ExprC]) #:transparent)
(struct RocalC ([name : IdC] [rhs : ExprC] [body : ExprC]) #:transparent)



(define-type Location Natural)
;store
;(define-type Store (Vector Cell))

(struct Cell ([location : Integer] [val : Value]) #:transparent)

(define-type Store (Vectorof Value))
(struct InitialBinding ([name : Symbol] [val : Value]) #:transparent)
(struct Bind ([name : Symbol] [location : Location]) #:transparent)

(define top-env
  (list
    (InitialBinding 'true (BoolV #t))
    (InitialBinding 'false (BoolV #f))
    (InitialBinding '+ (PrimV '+))
    (InitialBinding '- (PrimV '-))
    (InitialBinding '* (PrimV '*))
    (InitialBinding '/ (PrimV '/))
    (InitialBinding '<= (PrimV '<=))
    (InitialBinding 'substring (PrimV 'substring))
    (InitialBinding 'strlen (PrimV 'strlen))
    (InitialBinding 'equal? (PrimV 'equal?))
    (InitialBinding 'error (PrimV 'error))
    (InitialBinding 'println (PrimV 'println))
    (InitialBinding 'read-num (PrimV 'read-num))
    (InitialBinding 'read-str (PrimV 'read-str))
    (InitialBinding 'begin (PrimV 'begin))
    (InitialBinding '++ (PrimV '++))
    (InitialBinding 'random (PrimV 'random))

    ; DRMG6 new functions
    (InitialBinding 'make-array (PrimV 'make-array))
    (InitialBinding 'array (PrimV 'array))
    (InitialBinding 'aref (PrimV 'aref))
    (InitialBinding 'aset! (PrimV 'aset!))))

#;(define top-env
  (list
    (Bind 'true 0)
    (Bind 'false 1)
    (Bind '+ 2)
    (Bind '- 3)
    (Bind '* 4)
    (Bind '/ 5)
    (Bind '<= 6)
    (Bind 'substring 7)
    (Bind 'strlen 8)
    (Bind 'equal? 9)
    (Bind 'error 10)
    (Bind 'println 11)
    (Bind 'read-num 12)
    (Bind 'read-str 13)
    (Bind 'begin 14)
    (Bind '++ 15)
    ;(Bind 'random 16)

    ; DRMG6 new functions
    (Bind 'make-array 16)
    (Bind 'array 17)
    (Bind 'aref 18)
    (Bind 'aset! 19)))



;reserved keywords
(define reserved '(if = local % -> in := rocal))

;-----------------  store ------------------
;(define-type Store (something))
;(define sto ( something idk))


; ---------------- store functions ----------

(define (add-to-store [sto : Store] [value : Value]) : Location
  ;(match value
   ; [(ArrV loc len)
  (define loc (allocate sto 1))
  (vector-set! sto loc value)
  loc)
  


(define (make-initial-store [memsize : Natural]) : Store
  (define store : Store (make-vector memsize (NullV)))
  ;(vector-set! store 0 1)
  (populate-store store top-env)
  store)
;-------------

#;(define (populate-store [vec : Vector] [env : Env] [location : Integer]) : Store

  )
(define (populate-store [sto : Store] [bindings : (Listof InitialBinding)]) : Env
  (cond
    [(empty? bindings) empty]
    [else
     (define current (first bindings))
     (define location (allocate sto 1))
     (vector-set! sto location (InitialBinding-val current))
     (cons
      (Bind (InitialBinding-name current) location)
      (populate-store sto (rest bindings)))]))

  #;(define (populate-store [sto : Store] [bindings : (Listof Bind)]) : Env
  (cond
    [(empty? bindings) empty]
    [else
     (define current (first bindings))
     (define location (allocate sto 1))
     (vector-set! sto location (PrimV (Bind-name current)))
     (cons
      (Bind (Bind-name current) location)
      (populate-store sto (rest bindings)))]))



; probably for making arrays
;(define (allocate [sto : Store] [locations : Integer]) : Location
  
(define (allocate [sto : Store] [size : Natural]) : Location
  (define start (assert (vector-ref sto 0) exact-nonnegative-integer?))
  (define next (+ start size))
  (cond
    [(> next (vector-length sto)) (error 'allocate "DRMG: Out of Memory when trying to allocate")]
    [else
     (vector-set! sto 0 next) start]))
 
(define my-store (make-initial-store 20))
(check-equal? (allocate my-store 4) 1)
(check-equal? (allocate my-store 3) 5)
(check-equal? (vector-ref my-store 0) 8)



(define initial-env : Env
  (populate-store (make-initial-store 100) top-env))


; ----------------------------------------Serialize -------------------------------------------

;takes in a value and returns a serialized string
(define (serialize [val : Value]) : String
  (match val
    [(BoolV b) (if b "true" "false")]
    [(NumV n) (~v n)]
    [(StrV s) (~v s)]
    [(CloV _ _ _) "#<procedure>"]
    [(PrimV _ ) "#<primop>"]
    [(NullV) "null"]
    [(ArrV _ _) "#<array>"]
    
    ))


; ---------------------------------Look Up for Variable in Environment-------------------------------
;takes in a symbol and an environment and it looks into the environment
;and tries to look if the given symbol is inside the environment
;and then it returns the value associated with the symbol
(define (lookup [sym : Symbol] [env : Env]) : Location
   (cond
      [(empty? env) (error 'lookup "DRMG: Binding was not found for ~v" sym)]
      [(symbol=? sym (Bind-name (first env))) (Bind-location (first env))]
      [else (lookup sym (rest env))]))


;----------------- Helper func to find duplicate variables and Invalid Variables ----------------------

;; has-duplicates? checks whether a list contains repeated symbols.
(define (has-duplicates? [symbols : (Listof Symbol)]) : Boolean
  (if (empty? symbols)
      #f
      (if (member (first symbols) (rest symbols))
          #t
          (has-duplicates? (rest symbols)))))

(check-equal? (has-duplicates? '(x y z)) #f)
(check-equal? (has-duplicates? '(x y x)) #t)
(check-equal? (has-duplicates? '()) #f)

;; Returns #t if s is a symbol that is not a special keyword
(define (valid-id? [s : Sexp]) : Boolean
  (and (symbol? s) (not (member s reserved)) #t))


;takes in a list of symbols and makes sure each symbol is valid to use
(define (all-valid-ids? [syms : (Listof Symbol)]) : Boolean
  (cond
    [(empty? syms) #t]
    [(valid-id? (first syms)) (all-valid-ids? (rest syms))]
    [else #f]))
; -----------------------------------Parser---------------------------------------------------

;-----------------------------
;Takes in a Sexp and parses it into an ExprC
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? n) (NumC n)]
    [(? string? s) (StrC s)]
    [(? symbol? sym)
     (cond
       [(equal? (valid-id? sym) #t) (IdC sym)]  
       [else (error 'parse "DRMG: Incorrect usage of DRMG syntax")])]
    [(list 'if condition ifTrue ifFalse) (If (parse condition) (parse ifTrue) (parse ifFalse))]
    [(list 'if _ ...)
     (error 'parse "DRMG: Invalid syntax for if")] 
    [(list '% (? symbol? params) ... '-> body)
     (define ps (cast params (Listof Symbol)))
     (cond
       [(not (all-valid-ids? ps))
        (error 'parse "DRMG: Reserved DRMG word used as parameter")]
       [(has-duplicates? ps)
        (error 'parse "DRMG: Duplicate function parameters")]
       [else (LamC (map IdC ps) (parse body))])]

    [(list 'local (list (? symbol? vars) '= expr) ... 'in expr2)
     (define vs (cast vars (Listof Symbol)))
     (cond
       [(not (all-valid-ids? vs))
        (error 'parse "DRMG: Reserved word used as local variable")] 
       [(has-duplicates? vs)
        (error 'parse "DRMG: Duplicate variables in local")]
       [else (AppC (LamC (map IdC vs) (parse expr2)) (map parse (cast expr (Listof Sexp))))])]
    
    ;finish mutation rule
    #;[(list var ':= val)
     (define valid (valid-id? var))
     (if (not valid)
         (error 'parse "DRMG: Reserved word used as variable")   
         (parse val))]
       
    
    [(list fun args ...) (AppC (parse fun) (map parse args))]
    [_ (error 'parse "DRMG: Invalid syntax")]))
 

;------- interp helper for multiple arguements ---------

;Takes in a list of args and the environment and it evaluates the args
;into values using interp
(define (interp-args [args : (Listof ExprC)] [env : Env] [sto : Store]) : (Listof Value)
  (match args
    ['()'()]
    [(cons first rest)
     (define interp-first (interp first env sto))
     (cons interp-first
           (interp-args rest env sto))]))

; basically reserves a spot for a param and saves the value of that param in that spot 
(define (extend-env [params : (Listof Symbol)] [vals : (Listof Value)] [env : Env] [sto : Store]) : Env
  (match* (params vals)
    [('() '()) env]
    [((cons p-first p-rest) (cons val-first val-rest))
     (define location (allocate sto 1))
     (vector-set! sto location val-first)
     (extend-env p-rest val-rest (cons (Bind p-first location) env) sto)]
    [(_ _) (error 'extend-env "DRMG: parameter/argument count mismatch")]))



;------ Making a Array (With populate helper function)
(define (fill-store! [vals :(Listof Value)] [start : Natural] [sto : Store]) : Void
  (match vals
    ['() (void)]
    [(cons val rest-vals)
     (vector-set! sto start val)
     (fill-store! rest-vals (+ start 1) sto)]))
  
;Make Array function
(define (new-array [vals : (Listof Value)] [sto : Store])
  (define len (length vals))
  (cond
    [(< len 1 ) (error 'new-array "DRMG: arrays need at least one cell")]
    [else
     (define base (allocate sto len))
     (fill-store! vals base sto)
     (ArrV base len)]))
  
;----------------- drmg 5 helpers -------------------

;takes in a list of Values and checks if it meets the arity of println
(define (println-drmg [args : (Listof Value)]) : BoolV
  (match args
       [(list (StrV s)) (println s) (BoolV #t)]
       [_ (error "DRMG: println expects one string argument")]))

;reads input from the user and returns a StrV of the user input
;if it is valid or false if the input is invalid
(define (read-input) : (U StrV #f)
  (printf "> ")
  (define input-val (read-line))
  (if (string? input-val)
      (StrV (cast input-val String))
      #f))

;takes in a list of Values and concatenates all of them into one string
(define (concat-drmg [args : (Listof Value)]) : String
  (match args
    ['() ""]
    [(cons f r) (match f
                  [(NumV num) (string-append (number->string num) (concat-drmg r))]
                  [(StrV str) (string-append str (concat-drmg r))]
                  [(BoolV boo) (string-append (serialize (BoolV boo)) (concat-drmg r))]
                  [_ (string-append (serialize f) (concat-drmg r))])]))

;------------------------ Interp for Primitive Values ----------------------------------------

;takes in a symbol and a list of values and it performs an operation based
;on the symbol that was given
(define (interp-prim [op : Symbol] [args : (Listof Value)] [sto : Store]) : Value
  (match op
    ['+
     (match args
       [(list (NumV a) (NumV b)) (NumV (+ a b))]
       [_ (error 'interp-prim "DRMG: Expected two numbers")])]
    ['-
     (match args
       [(list (NumV a) (NumV b)) (NumV (- a b))]
       [_ (error 'interp-prim "DRMG: Expected two numbers")])]
    ['*
     (match args
       [(list (NumV a) (NumV b)) (NumV (* a b))]
       [_ (error 'interp-prim "DRMG: Expected two numbers")])]
    ['/
     (match args
       [(list (NumV a) (NumV b))
        (if (zero? b)
            (error 'interp-prim "DRMG: Division by zero")
            (NumV (/ a b)))]
       [_ (error 'interp-prim "DRMG: Expected two numbers")])]
    ['<=
     (match args
       [(list (NumV a) (NumV b)) (BoolV (<= a b))]
       [_ (error 'interp-prim "DRMG: Expected two numbers")])]
    ['strlen
     (match args
       [(list (StrV s)) (NumV (string-length s))]
       [_ (error 'interp-prim "DRMG: strlen expects one string")])]
    ['substring
     (match args
       [(list (StrV s) (NumV n1) (NumV n2))
        (cond
          [(not (and (exact-nonnegative-integer? n1)
                     (exact-nonnegative-integer? n2)))
           (error 'interp-prim "DRMG: Invalid substring indices")]
          [(< n2 n1)
           (error 'interp-prim "DRMG: Second input is less than first input")] 
          [(> n2 (string-length s))
           (error 'interp-prim "DRMG: Range exceeds string length ")]
          [else (StrV (substring s n1 n2))])] 
       [_ (error 'interp-prim "DRMG: Invalid substring arguments")])] 
    ['equal?
     (match args
       [(list v1 v2)
        (BoolV
         (match* (v1 v2)
           [((NumV a) (NumV b)) (= a b)]
           [((BoolV a) (BoolV b)) (equal? a b)]
           [((StrV a) (StrV b)) (string=? a b)]
           [(_ _) #f]))]
       [_ (error 'interp-prim "DRMG: equal? expects two arguments")])]
    ['error
     (match args
       [(list any) (error 'user-error (serialize any))]
       [_ (error 'interp-prim "DRMG: error expects one argument")])]
    
    ['println (println-drmg args)]
    ['read-num
     (define input-val (read-input))
     (match input-val
       [(StrV str) (if (real? (string->number str))
                    (NumV (cast (string->number str) Real))
                    (error "DRMG: read-num expects a number value"))]
       [_ (error "DRMG: invalid input for read-num")])]
    ['read-str
     (define input-val (read-input))
     (if (equal? input-val #f)
         (error "DRMG: invalid input for read-str")
         (cast input-val StrV))]
    ['begin (last args)]
    ['++ (StrV (concat-drmg args))]
    ['random
     (match args
       [(list (NumV n))
        (if (exact-positive-integer? n)
            (NumV (random (cast n Integer))) 
            (error 'interp-prim "random expects a positive integer"))]
       [_ (error 'interp-prim "random expects one argument")])]

    [_ (error 'interp-prim "DRMG: Unknown primitive operator")])) 

    
; ------------------------------- Interp ------------------------------------------------

;takes in an exprC and an environment and it uses the
;environment to interpret the expression into a value
#;(define (interp [e : ExprC] [env : Env]) : Value
  (match e
    [(IdC s) (lookup s env)]
    [(NumC n) (NumV n)]
    [(StrC s) (StrV s)] 
    [(If condition ifTrue else)
     (define result (interp condition env))
     (match result
       [(BoolV #t) (interp ifTrue env)]
       [(BoolV #f) (interp else env)]
       [_ (error 'interp "DRMG: condition is not a boolean value")])]
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
                  [else (error 'interp "DRMG: Number of Arguments are not equal got ~v" args)])]
               [(PrimV op) (interp-prim op (interp-args args env))]
               [_ (error 'interp "DRMG: Attempted to call a non-function")])]))


; DRMG 6 version
(define (interp [e : ExprC] [env : Env] [sto : Store]) : Value
  (NumV 2)
  (match e
    [(IdC s) (cast (vector-ref sto (lookup s env)) Value)]
    [(NumC n) (NumV n)]
    [(StrC s) (StrV s)]
    [(If condition ifTrue else)
      (define result (interp condition env sto))
       (match result
       [(BoolV #t) (interp ifTrue env sto)]
       [(BoolV #f) (interp else env sto)]
       [_ (error 'interp "DRMG: condition is not a boolean value")])]
    [(LamC args body) (CloV (map IdC-sym args) body env)]
    ;; copied and pasted changed a couple things (ts is not correct feel free to change)
    [(AppC func args)
     (define fundef (interp func env sto))
     (define arg-vals (interp-args args env sto))
             (match fundef
               ; Interp returns Value so we have to match to see if its specifically CloV
               [(CloV params body closure-env) 
                (cond
                  ;if the lengths of arguements are right we interp the values,
                  ;making in bindings, and create the new env
                  [(equal? (length params) (length args))
                   ;(define arg-values (interp-args args env sto))
                   ;(define bindings (map Bind params arg-values))
                   ;(define new-env (append bindings closure-env))
                   (interp body (extend-env params arg-vals closure-env sto) sto)]
                   ;(interp body new-env)]
                  [else (error 'interp "DRMG: Number of Arguments are not equal got ~v" args)])]
              ; [(PrimV op) (interp-prim op (interp-args args env))]
               [(PrimV op) (interp-prim op arg-vals sto)]
               [_ (error 'interp "DRMG: Attempted to call a non-function")])]
    ))

;----------------------------- top interp -------------------------------

;takes in a sexp and returns a serialized string containing a value
#;(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))

; DRMG 6 version of top interp
(define (top-interp [s : Sexp] [memsize : Natural]) : String
  (serialize (interp (parse s) initial-env (make-initial-store memsize))))

