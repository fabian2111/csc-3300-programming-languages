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

;(struct Cell ([location : Integer] [val : Value]) #:transparent)

;hopefully this works fully
(define-type StoreValue (U Location Value))

(define-type Store (Vectorof StoreValue))
(struct InitialBinding ([name : Symbol] [val : Value]) #:transparent)
(struct Bind ([name : Symbol] [location : Location]) #:transparent)

;i think you will need both this and initial env, since you already know the location values initially.
;doing this helped the issues that i was having
;then again, im not too sure so feel free to change
(define top-env
  (list
   (Bind 'true 1)
   (Bind 'false 2)
   (Bind '+ 3)
   (Bind '- 4)
   (Bind '* 5)
   (Bind '/ 6)
   (Bind '<= 7)
   (Bind 'substring 8)
   (Bind 'strlen 9)
   (Bind 'equal? 10)
   (Bind 'error 11)
   (Bind 'println 12)
   (Bind 'read-num 13)
   (Bind 'read-str 14)
   (Bind 'begin 15)
   (Bind '++ 16)
   (Bind 'random 17)
   (Bind 'make-array 18)
   (Bind 'array 19)
   (Bind 'aref 20)
   (Bind 'aset! 21)

   ;i dont know if you need to make while a prim value, but i think it might be 
   ;(Bind 'while 22)
   ))

(define initial-env
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
    (InitialBinding 'aset! (PrimV 'aset!))
    ;(InitialBinding 'while (PrimV 'while))
    ))


;reserved keywords
(define reserved '(if = local % -> in := rocal))

;-----------------  store functions ------------aaert

#;(define (add-to-store [sto : Store] [value : Value]) : Location
  (define loc (allocate sto 1))
  (vector-set! sto loc value)
  loc)
  

(define (make-initial-store [memsize : Natural]) : Store
  ;(define store : Store (make-vector memsize (NullV)))
  (cond
    [(< memsize 1) (error 'make-initial-store "DRMG: Store length should be minimum 1")]
    [else 
     (define store : Store (make-vector memsize 0))
     ;(vector-set! store 0 1)
     (populate-store store initial-env)
     store ]))
;------------- 

(define (populate-store [sto : Store] [bindings : (Listof InitialBinding)]) : Null
  (cond
    [(empty? bindings) empty] 
    [else
     (define current (first bindings))
     (define location (allocate sto 1))
     (vector-set! sto location (InitialBinding-val current))
     (populate-store sto (rest bindings))]))
     ;(cons
      ;(Bind (InitialBinding-name current) location)
      ;(populate-store sto (rest bindings)))]))

 
; probably for making arrays
;(define (allocate [sto : Store] [locations : Integer]) : Location

#;(define (find-next-loc [sto : Store] [curLoc : Natural]) : Location
  (if (equal? (vector-ref sto curLoc) (NullV)
  ))) 


(define (allocate [sto : Store] [size : Natural]) : Location
  (define start (assert (vector-ref sto 0) exact-nonnegative-integer?))
  (define next (+ start size))
  (cond
    [(> next (vector-length sto)) (error 'allocate "DRMG: Out of Memory when trying to allocate")]
    [else
     ;i think adding 1 to start should fix cuz i think what you are trying to do is make the 0th value the length
     (vector-set! sto 0 next) {+ start  1}]))
 
 
(define s30 (make-initial-store 30))   ; the 21 primitives use cells 1-21
(check-equal? (allocate s30 8) 22) 
(check-equal? (vector-ref s30 0) 30) 
;(check-exn #rx"DRMG" (lambda () (allocate s30 1)))
;(check-exn #rx"DRMG" (lambda () (make-initial-store 0)))

;only for testing
;(define initial-store : Store (make-initial-store 100))


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
    [(ArrV _ _) "#<array>"]))


; ---------------------------------Look Up for Variable in Environment-------------------------------
;takes in a symbol and an environment and it looks into the environment
;and tries to look if the given symbol is inside the environment
;and then it returns the value associated with the symbol
(define (lookup [sym : Symbol] [env : Env]) : Location
   (cond
      [(empty? env) (error 'lookup "DRMG: Binding was not found for ~v" sym)]
      [(symbol=? sym (Bind-name (first env))) (Bind-location (first env))]
      [else (lookup sym (rest env))]))

;(lookup '+ top-env)
;(vector-ref initial-store 3)


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
    [(list (? symbol? var) ':= val)
     (define valid (valid-id? var))
     (if (not valid)
         (error 'parse "DRMG: Reserved word used as variable")   
         (MutC (IdC var) (parse val)))]
    
    [(list 'rocal (list (? symbol? name) '= expr) 'in body)
     (cond
       [(valid-id? name) (RocalC (IdC name) (parse expr) (parse body))]
       [else (error 'parse "DRMG: reserved word used as rocal variable ")])]

    ; i thinkkk its somewhat like this, i need to fully understand what the instructions are saying
    ; i might be wrong on this so change it however you like,  not fully sure if it needs to be in parser
    
    ;[(list 'while guard body) (RocalC (IdC 'while) (parse guard) (parse body))]
       
    
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

;; repeat-value: a list containing n copies of v
(define (repeat-value [n : Natural] [v : Value]) : (Listof Value)
  (cond
    [(zero? n) '()]
    [else (cons v (repeat-value (- n 1) v))]))

;; fill-with!: stores v in n consecutive cells starting at start
(define (fill-with! [start : Natural] [n : Natural] [v : Value] [sto : Store]) : Void
  (cond
    [(zero? n) (void)]
    [else
     (vector-set! sto start v)
     (fill-with! (+ start 1) (- n 1) v sto)]))

;; check-index: returns idx as a Natural if 0 <= idx < len, errors otherwise
(define (check-index [idx : Real] [len : Natural]) : Natural
  (cond
    [(and (exact-nonnegative-integer? idx) (< idx len)) idx]
    [else (error 'array "DRMG: index ~v out of range for array of length ~v" idx len)]))
  
;----------------- Println/read-input Helpers -------------------

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
           ; new for drmg6 
           [((ArrV l1 n1) (ArrV l2 n2)) (and (= l1 l2) (= n1 n2))]
           [((NullV) (NullV)) #t]
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

    ; Array cases
    #;['make-array
     (match args
       [(list (NumV  (? exact-positive-integer? n)) v)
        (new-array  (repeat-value n v) sto)]
       [_ (error 'interp-prim "DRMG: make-array expects a positive integer size and a value")])]

    ['make-array
     (match args
       [(list (NumV (? exact-positive-integer? n)) v)
        (define base (allocate sto n))   ; errors with DRMG before anything is built
        (fill-with! base n v sto)
        (ArrV base n)]
       [_ (error 'interp-prim "DRMG: make-array expects a positive integer size and a value")])]

    
    ['array (new-array args sto)]
    
    ['aref
     (match args
       [(list (ArrV base len) (NumV idx))
        (cast (vector-ref sto (+ base (check-index idx len))) Value)]
       [_ (error 'interp-prim "DRMG: aref expects an array and an index")])]
    #;['aref
     (match args
       [(list (ArrV base len) (NumV idx))

        ;cast this to value, maybe change vector type to avoid cast but idk
       (cast (vector-ref sto (+ base len)) Value)]
       [_ (error 'interp-prim "DRMG: aref expects an array and an index")])]
    ['aset!
     (match args
       [(list (ArrV base len) (NumV idx) v)
        (vector-set! sto (+ base (check-index idx len)) v)
        (NullV)]
       [_ (error 'interp-prim "DRMG: aset! expects an array, an index and a value")])]

    ;need to finish while
    ;['while
     ;(match args
      ; [(list guard body)
       ; (if guard
        ;    body
        

    [_ (error 'interp-prim "DRMG: Unknown primitive operator")])) 

    
; ------------------------------- Interp ------------------------------------------------

;takes in an exprC and an environment and it uses the 
;environment to interpret the expression into a value


; DRMG 6 version
(define (interp [e : ExprC] [env : Env] [sto : Store]) : Value
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

    ;rocal 
    [(RocalC name rhs body)
     (define loc (allocate sto 1))
     (vector-set! sto loc (NullV))
     (define new-env (cons (Bind (IdC-sym name) loc) env))
     (vector-set! sto loc (interp rhs new-env sto))
     (interp body new-env sto)]

    
    #;[(RocalC name rhs body)
     (define location (allocate sto 1))
     (define roc-sym (IdC-sym name))
     
     (define new-bind (Bind roc-sym location)) 
     (define new-env (cons new-bind env))
     (vector-set! sto location (interp rhs new-env sto))
     
     
     

     ;lowkey dont know if this is what you're supposed to do
     (cond
       [(equal? roc-sym 'while) (define guard-result (interp rhs new-env sto))
                                (match guard-result
                                  [(BoolV #t) (interp body new-env sto) (interp e new-env sto)]
                                  [(BoolV #f) (NullV)])]
       [else (interp body new-env sto)]
       )]

    ;mutations, i want to finally let my inner chud take control so that i can play hollow knight, and silksong (hollow knight pt 2) and gta 6 all day
    ;and pirate token, pirate token
    [(MutC var val)
     (define location (lookup (IdC-sym var) env))
     (vector-set! sto location (interp val env sto))
     (NullV)]

  
     
    ;; copied and pasted changed a couple things (ts is not correct feel free to change), nah do it again
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

;(define rocal (parse '{rocal [fact = {% n -> {if (<= n 0) 1 {* n {fact {- n 1}}}}}] in {fact 4}}))

;(parse '{rocal [x = 3] in x})
;(define test (parse '{+ 1 2}))

;ts works!!!, maybe... gotta test more 
(define val (parse '{local [x = 4] in {begin {println {++ "" x}} {x := "1738"} {println x}}}))

;should error when interping
(define cant-mutate (parse '{x := 4}))

;suprising works but needs more testing ofc, lowkey double check if im implementing the while function in the right locations (interp, interp-prim, etc)
; also found out that x = x + 1 does work as expected if we are supposed to do that
#;(define while-thing (parse '{local [x = 0] in {begin {while {<= x 4} {begin {println {++ "" x}} {x := {+ x 1}}}} {println "done"}}}))


(define func (parse '{{% a b -> {+ a b}} 4 5}))

#;(interp while-thing top-env (make-initial-store 80))

;test cases, grrr
(check-equal? (parse '{x := 4}) (MutC (IdC 'x) (NumC 4)))
(check-equal? (parse '{x := "y"}) (MutC (IdC 'x) (StrC "y")))

;(check-equal? (interp (MutC (IdC 'x) (NumC 3)) top-env (make-initial-store 30))


;----------------------------- top interp -------------------------------

;takes in a sexp and returns a serialized string containing a value
#;(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))

; DRMG 6 version of top interp
(define (top-interp [s : Sexp] [memsize : Natural]) : String
  (serialize (interp (parse s) top-env (make-initial-store memsize))))
 
 
(define while
  '{rocal [loop = {% guard body ->
                     {if {guard}
                         {begin {body} {loop guard body}}
                        false}}]
    in loop}) 
  
(check-equal?
  (top-interp `{local [while = ,while] [x = 0]
               in {begin {while {% -> {<= x 4}} {% -> {x := {+ x 1}}}}
                         x}}
             100)
 "5")


(check-equal? (top-interp '{aref {array 5 6 7} 1} 100) "6")
(check-exn #rx"DRMG" (lambda () (top-interp '{aref {array 1 2} 2} 100)))
(check-equal? (top-interp '{local [a = {array 1 2}] in {equal? a a}} 100) "true")
(check-equal? (top-interp '{equal? {array 1} {array 1}} 100) "false")
(check-equal? (top-interp '{rocal [f = {% n -> {if {<= n 0} 0 {+ n {f {- n 1}}}}}] in {f 3}} 200) "6")
(check-exn #rx"DRMG" (lambda () (top-interp '{make-array 1000000000 0} 100)))