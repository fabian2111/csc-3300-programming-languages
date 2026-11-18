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
(struct ArrC ([values : (Listof ExprC)]) #:transparent)
(struct ArrV ([loc : Location] [len : Natural]) #:transparent)
(struct NullV () #:transparent)
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


;the s-expression for the while function
(define while
  '{rocal [loop = {% guard body ->
                     {if {guard}
                         {begin {body} {loop guard body}}
                        false}}]
    in loop})

;the s-expression for the while functino
(define in-order
  '{% arr arrlen ->
      {local [curLoc = 0] [res = false] in
        {begin
          {if {<= arrlen 1}
              {res := true}
              {while {% -> {<= curLoc {- arrlen 2}}}
                     {% -> {begin {res := {<= {aref arr curLoc} {aref arr {+ curLoc 1}}}}
                                  {curLoc := {+ curLoc 1}}}}}}
          res}}})

(define top-env
  (list
   (Bind 'true 1) (Bind 'false 2)
   (Bind '+ 3) (Bind '- 4)
   (Bind '* 5) (Bind '/ 6)
   (Bind '<= 7) (Bind 'substring 8)
   (Bind 'strlen 9) (Bind 'equal? 10)
   (Bind 'error 11) (Bind 'println 12)
   (Bind 'read-num 13) (Bind 'read-str 14)
   (Bind 'begin 15) (Bind '++ 16)
   (Bind 'random 17) (Bind 'make-array 18)
   (Bind 'array 19) (Bind 'aref 20)
   (Bind 'aset! 21) (Bind 'arr-len 22)))

(define initial-env
  (list
    (InitialBinding 'true (BoolV #t)) (InitialBinding 'false (BoolV #f))
    (InitialBinding '+ (PrimV '+)) (InitialBinding '- (PrimV '-))
    (InitialBinding '* (PrimV '*)) (InitialBinding '/ (PrimV '/))
    (InitialBinding '<= (PrimV '<=)) (InitialBinding 'substring (PrimV 'substring))
    (InitialBinding 'strlen (PrimV 'strlen)) (InitialBinding 'equal? (PrimV 'equal?))
    (InitialBinding 'error (PrimV 'error)) (InitialBinding 'println (PrimV 'println))
    (InitialBinding 'read-num (PrimV 'read-num)) (InitialBinding 'read-str (PrimV 'read-str))
    (InitialBinding 'begin (PrimV 'begin)) (InitialBinding '++ (PrimV '++))
    (InitialBinding 'random (PrimV 'random)) (InitialBinding 'make-array (PrimV 'make-array))
    (InitialBinding 'array (PrimV 'array)) (InitialBinding 'aref (PrimV 'aref))
    (InitialBinding 'aset! (PrimV 'aset!)) (InitialBinding 'arr-len (PrimV 'arr-len))))


;reserved keywords
(define reserved '(if = local % -> in := rocal))

;-----------------  store functions ------------

(define (make-initial-store [memsize : Natural]) : Store
  ;(define store : Store (make-vector memsize (NullV)))
  (cond
    [(< memsize 1) (error 'make-initial-store "DRMG: Store length should be minimum 1")]
    [else
     (define store : Store (make-vector memsize 0))
     (populate-store store initial-env)
     store]))

;-------------

(define (populate-store [sto : Store] [bindings : (Listof InitialBinding)]) : Void
  (cond
    [(empty? bindings) (void)]
    [else
     (define current (first bindings))
     (define location (allocate sto 1))
     (vector-set! sto location (InitialBinding-val current))
     (populate-store sto (rest bindings))]))

; probably for making arrays
(define (allocate [sto : Store] [size : Natural]) : Location
  (define start (assert (vector-ref sto 0) exact-nonnegative-integer?))
  (define next (+ start size))
  (cond
    [(>= next (vector-length sto)) (error 'allocate "DRMG: Out of Memory when trying to allocate")]
    [else (vector-set! sto 0 next) {+ start 1}]))

;only for testing
(define initial-store : Store (make-initial-store 100))

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
       [(symbol=? sym 'while) (parse while)]
       [(symbol=? sym 'in-order) (parse in-order)]
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
    [(list (? symbol? var) ':= val)
     (define valid (valid-id? var))
     (define val-valid (valid-id? val))
     (if  (not valid) 
         (error 'parse "DRMG: Reserved word used as variable")
         (MutC (IdC var) (parse val)))]
    [(list 'rocal (list (? symbol? name) '= expr) 'in body)
     (cond
       [(valid-id? name) (RocalC (IdC name) (parse expr) (parse body))]
       [else (error 'parse "DRMG: reserved word used as rocal variable ")])]
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
    ['make-array
     (match args
       [(list (NumV (? exact-positive-integer? n)) v)
        (new-array (repeat-value n v) sto)]
       [_ (error 'interp-prim "DRMG: make-array expects a positive integer size and a value")])]
    ['array (new-array args sto)]
    ['aref
     (match args
       [(list (ArrV base len) (NumV idx))
        (cast (vector-ref sto (+ base (check-index idx len))) Value)]
       [_ (error 'interp-prim "DRMG: aref expects an array and an index")])]
    ['aset!
     (match args
       [(list (ArrV base len) (NumV idx) v)
        (vector-set! sto (+ base (check-index idx len)) v)
        (NullV)]
       [_ (error 'interp-prim "DRMG: aset! expects an array, an index and a value")])]
    ['arr-len
     (match args
       [(list (ArrV base len)) (NumV (ArrV-len (first args)))]
       [_ (error 'interp-prim "DRMG: arr-len expects an array")])]

    [_ (error 'interp-prim "DRMG: Unknown primitive operator")]))

; ------------------------------- Interp ------------------------------------------------

;takes in an exprC and an environment and it uses the
;environment to interpret the expression into a value
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
    [(RocalC name rhs body)
     (define loc (allocate sto 1))
     (vector-set! sto loc (NullV))
     (define new-env (cons (Bind (IdC-sym name) loc) env))
     (vector-set! sto loc (interp rhs new-env sto))
     (interp body new-env sto)]
    [(MutC var val)
     (define location (lookup (IdC-sym var) env))
     (vector-set! sto location (interp val env sto))
     (NullV)]
    [(AppC func args)
     (define fundef (interp func env sto))
     (define arg-vals (interp-args args env sto))
             (match fundef
               [(CloV params body closure-env)
                (cond
                  ;if the lengths of arguements are right we interp the values,
                  ;making in bindings, and create the new env
                  [(equal? (length params) (length args))
                   (interp body (extend-env params arg-vals closure-env sto) sto)]
                  [else (error 'interp "DRMG: Number of Arguments are not equal got ~v" args)])]
               [(PrimV op) (interp-prim op arg-vals sto)]
               [_ (error 'interp "DRMG: Attempted to call a non-function")])]))

;----------------------------- top interp -------------------------------

;takes in a sexp and a memory size and returns a serialized string containing a value
(define (top-interp [s : Sexp] [memsize : Natural]) : String
  (serialize (interp (parse s) top-env (make-initial-store memsize))))



(check-equal? (top-interp `{local [x = 0]
               in {begin {while {% -> {<= x 4}} {% -> {x := {+ x 1}}}}
                         x}} 100) "5")



;---------------------------------------- Tests -------------------------------------
(check-exn (regexp "DRMG:") (lambda () (make-initial-store 0)))
(check-equal?(serialize (NullV)) "null")
 
(check-equal?(serialize (ArrV 10 1)) "#<array>") 


;------------------ Println ------------------------------
(check-equal? (println-drmg (list (StrV "Hello"))) (BoolV #t))

(check-exn (regexp (regexp-quote "DRMG: println expects one string argument"))
           (lambda () (println-drmg (list (NumV 5)))))
(check-exn (regexp (regexp-quote "DRMG: println expects one string argument"))
           (lambda () (println-drmg '())))
(check-exn (regexp (regexp-quote "DRMG: println expects one string argument"))
           (lambda () (println-drmg (list (StrV "Hello") (StrV "World")))))
(check-exn (regexp "println expects one string argument") (lambda () (println-drmg (list (NumV 5) (NumV 10)))))

;------------------ Concat (++) ------------------

(check-equal? (concat-drmg (list (PrimV '+))) "#<primop>")
(check-equal? (concat-drmg '())"")
(check-equal? (concat-drmg (list (StrV "Hello"))) "Hello")
(check-equal? (concat-drmg (list (NumV 42)))"42")
(check-equal? (concat-drmg (list (BoolV #t)))"true")
(check-equal? (concat-drmg (list (BoolV #f)))"false")
(check-equal? (concat-drmg (list (StrV "Hello ") (StrV "World"))) "Hello World")
(check-equal? (concat-drmg (list (StrV "A") (StrV "B") (StrV "C"))) "ABC")
(check-equal? (concat-drmg (list (StrV "Age: ") (NumV 20))) "Age: 20")
(check-equal? (concat-drmg (list (NumV 5) (StrV " apples"))) "5 apples")
(check-equal? (concat-drmg (list (StrV "Result: ") (BoolV #t))) "Result: true")
(check-equal? (concat-drmg (list (NumV 10) (StrV " + ") (NumV 5))) "10 + 5")
(check-equal?
 (concat-drmg
  (list (StrV "Player: ")
        (StrV "Rock")
        (StrV ", Score: ")
        (NumV 10)
        (StrV ", Winner: ")
        (BoolV #t)))
 "Player: Rock, Score: 10, Winner: true")
;begin
(check-equal? (interp-prim 'begin (list {NumV 3} {StrV "test"})(make-initial-store 50)) {StrV "test"} )
(check-equal? (interp-prim 'begin (list {NumV 6}) (make-initial-store 50) ) {NumV 6})

;println
(check-equal? (interp-prim 'println (list {StrV "test"}) (make-initial-store 50)) {BoolV #t})

;interp-prim - ++
(check-equal? (interp-prim '++ (list {StrV "Num: "} {NumV 5}) (make-initial-store 50)) {StrV "Num: 5"})

;------------------------ Look Up Function -------------------

(check-equal? (lookup 'b (list (Bind 'a 5) (Bind 'b 12))) 12)

;; when a name is bound twice, the innermost (first) binding wins
(check-equal? (lookup 'x (list (Bind 'x 7) (Bind 'x 9))) 7)
;; top-level primitives map to their store locations
(check-equal? (lookup '+ top-env) 3)
(check-equal? (lookup 'true top-env) 1)
(check-equal? (lookup 'aset! top-env) 21)
;; the location holds the value, so check the store too
(check-equal? (vector-ref (make-initial-store 50) (lookup '+ top-env)) (PrimV '+))

;; errors
(check-exn (regexp (regexp-quote "DRMG: Binding was not found"))
           (lambda () (lookup 'c (list (Bind 'a 5) (Bind 'b 12)))))
(check-exn (regexp (regexp-quote "DRMG: Binding was not found"))
           (lambda () (lookup 'c (list (Bind 'a 10) (Bind 'b 10)))))
(check-exn (regexp (regexp-quote "DRMG: Binding was not found"))
           (lambda () (lookup 'x '())))

; Top-interp tests
;(define store1 (make-initial-store 100))
(check-equal? (top-interp "hi" 50) "\"hi\"")
(check-equal? (top-interp 'true 50) "true")
(check-equal? (top-interp 'false 50) "false")
(check-equal? (top-interp '{if {<= {* 5 2} 11} 10 11} 50) "10")
(check-equal? (top-interp '{local [x = 4] in {+ x 4}} 50) "8")
(check-equal? (top-interp '{local [z = {+ 9 14}] [y = 98] in {+ z y}} 50) "121")
(check-equal? (top-interp '{local [func = {% x -> {+ x 5}}] in {func 5}} 50) "10")
(check-equal? (top-interp '{% x -> {+ x 1}} 50) "#<procedure>")
(check-equal? (top-interp '{{% x -> {+ x 1}} 4} 50) "5")
(check-equal? (top-interp '{{% -> {+ 4 10}}} 50) "14")
(check-equal? (top-interp '({% x -> {+ x 1}} {* 2 10}) 50) "21")

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
 (serialize (CloV empty (NumC 5) empty))"#<procedure>")

; Test serializing primitive operators
(check-equal? (serialize (PrimV '+)) "#<primop>")
;---------------------- Parser Test ---------------------------
;Basic Test
(check-equal? (parse 4) {NumC 4})
(check-equal? (parse "test") {StrC "test"})
(check-equal? (parse 'Hello) {IdC 'Hello})
(check-equal? (parse '{if true 10 20}) (If (IdC 'true) (NumC 10) (NumC 20)))
(check-equal? (parse '{% x y -> x}) (LamC (list (IdC 'x) (IdC 'y)) (IdC 'x)))
(check-equal? (parse '{+ 2 3}) (AppC (IdC '+) (list (NumC 2) (NumC 3))))
(check-equal? (parse '{% x y -> x}) (LamC (list (IdC 'x) (IdC 'y)) (IdC 'x)))

;duplicate parameters test
(check-equal? (has-duplicates? '(x y z)) #f)
(check-equal? (has-duplicates? '(x y x)) #t)
(check-equal? (has-duplicates? '()) #f)
; Invalid Syntax
(check-exn (regexp (regexp-quote "DRMG: Invalid syntax")) (lambda () (parse '{})))
(check-exn (regexp (regexp-quote "DRMG: Invalid syntax for if")) (lambda () (parse '{if true 4})))
;Error
(check-exn (regexp "DRMG: Duplicate function parameters") (lambda () (parse '{% x x -> x})))
(check-exn (regexp "DRMG: Duplicate variables in local") (lambda () (parse '{local [z = {% -> 3}] [z = 9] in z})))

(check-exn (regexp "DRMG: Incorrect usage of DRMG syntax") (lambda () (parse '{+ if 4})))
(check-exn (regexp "DRMG: Incorrect usage of DRMG syntax") (lambda () (parse '{- local 2})))
(check-exn (regexp "DRMG: Incorrect usage of DRMG syntax") (lambda () (parse '{* % 2})))
(check-exn (regexp "DRMG: Incorrect usage of DRMG syntax") (lambda () (parse '{* % 2})))
(check-exn (regexp "DRMG: Duplicate variables in local") (lambda () (parse '(local (z = (% -> 3)) (z = 9) in (z)))))
(check-exn (regexp "DRMG: Incorrect usage of DRMG syntax") (lambda () (parse '(if = 0 1))))
(check-exn (regexp "DRMG: Reserved word used as local variable") (lambda () (parse '(local (local = "") in "World"))))
(check-exn (regexp "DRMG: Reserved DRMG word used as parameter") (lambda () (parse '(% if -> 3))))
;------------------------------- Interp Test Cases -----------------------

;; test setup: a store where x lives at a real location and holds (NumV 4)
(define sto-x : Store (make-initial-store 100))
(define x-loc : Location (allocate sto-x 1))
(vector-set! sto-x x-loc (NumV 4))
(define env-x : Env (cons (Bind 'x x-loc) top-env))

;Normal
(check-equal? (interp (NumC 5) top-env (make-initial-store 100)) (NumV 5))
(check-equal? (interp (IdC 'x) env-x sto-x) (NumV 4))
(check-equal? (interp (IdC '+) top-env (make-initial-store 100)) (PrimV '+))

;String
(check-equal? (interp (StrC "hello") top-env (make-initial-store 100)) (StrV "hello"))

;AppC
;; the parameter x gets a fresh location and shadows the outer x
(check-equal? (interp (AppC (LamC (list (IdC 'x)) (IdC 'x)) (list (NumC 10)))
                      env-x sto-x)
              (NumV 10))
;; the body sees the outer x, since the parameter is y
(check-equal? (interp (AppC (LamC (list (IdC 'y)) (IdC 'x)) (list (NumC 10)))
                      env-x sto-x)
              (NumV 4))
;; the outer x is unchanged after the call
(check-equal? (vector-ref sto-x x-loc) (NumV 4))
;; wrong arg count
(check-exn #rx"DRMG"
           (lambda () (interp (AppC (LamC (list (IdC 'x)) (IdC 'x)) (list (NumC 1) (NumC 2)))
                              top-env (make-initial-store 100))))
;; calling a non-function
(check-exn #rx"DRMG"
           (lambda () (interp (AppC (NumC 3) (list (NumC 1))) top-env (make-initial-store 100))))

;IF
(check-equal? (interp (If (AppC (IdC '<=) (list (NumC 2) (NumC 3))) (NumC 4) (NumC 21))
                      top-env (make-initial-store 100))
              (NumV 4))
(check-equal? (interp (If (AppC (IdC '<=) (list (NumC 10) (NumC 0))) (NumC 2) (NumC 100))
                      top-env (make-initial-store 100))
              (NumV 100))
(check-equal? (interp (If (IdC 'true) (NumC 1) (NumC 2)) top-env (make-initial-store 100)) (NumV 1))
(check-equal? (interp (If (IdC 'false) (NumC 10) (NumC 20)) top-env (make-initial-store 100)) (NumV 20))


(check-equal? (interp (If (IdC 'true) (NumC 10) (AppC (IdC 'error) (list (StrC "custom error"))))
                      top-env (make-initial-store 100))
              (NumV 10))
;; non-boolean condition
(check-exn #rx"DRMG"
           (lambda () (interp (If (NumC 1) (NumC 2) (NumC 3)) top-env (make-initial-store 100))))

;MutC (:=)
;; returns null and changes the store cell
(check-equal? (interp (MutC (IdC 'x) (NumC 9)) env-x sto-x) (NullV))
(check-equal? (vector-ref sto-x x-loc) (NumV 9))
;; unbound variable
(check-exn #rx"DRMG"
           (lambda () (interp (MutC (IdC 'nope) (NumC 1)) top-env (make-initial-store 100))))

;RocalC
(check-equal? (interp (RocalC (IdC 'x) (NumC 3) (IdC 'x)) top-env (make-initial-store 100))
              (NumV 3))
(check-equal? (top-interp '{rocal [f = {% n -> {if {<= n 0} 0 {+ n {f {- n 1}}}}}] in {f 3}} 200)
              "6")

(check-equal? (parse '{x := 4}) (MutC (IdC 'x) (NumC 4)))
(check-equal? (parse '{x := "y"}) (MutC (IdC 'x) (StrC "y")))

;evaluation order (
(check-equal? (top-interp '{local [x = 1]
                            in {- {begin {x := {+ x 1}} x}
                                  {begin {x := {* x 10}} x}}} 200)
              "-18")

;---------------- Interp Prims Test Cases -------------------------
;creates a store for each test case
(define (prim [op : Symbol] [args : (Listof Value)]) : Value
  (interp-prim op args (make-initial-store 100)))

;; arithmetic and comparison
(check-equal? (prim '+ (list (NumV 2) (NumV 3))) (NumV 5))
(check-equal? (prim '- (list (NumV 10) (NumV 3))) (NumV 7))
(check-equal? (prim '- (list (NumV 3) (NumV 10))) (NumV -7))
(check-equal? (prim '* (list (NumV 4) (NumV 3))) (NumV 12))
(check-equal? (prim '* (list (NumV 5) (NumV 0))) (NumV 0))
(check-equal? (prim '/ (list (NumV 12) (NumV 3))) (NumV 4))
(check-equal? (prim '/ (list (NumV 5) (NumV 2))) (NumV 5/2))
(check-equal? (prim '<= (list (NumV 3) (NumV 5))) (BoolV #t))
(check-equal? (prim '<= (list (NumV 5) (NumV 3))) (BoolV #f))
(check-equal? (prim '<= (list (NumV 4) (NumV 4))) (BoolV #t))

;; equal?
(check-equal? (prim 'equal? (list (NumV 5) (NumV 5))) (BoolV #t))
(check-equal? (prim 'equal? (list (NumV 5) (StrV "5"))) (BoolV #f))
(check-equal? (prim 'equal? (list (StrV "hello") (StrV "hello"))) (BoolV #t))
(check-equal? (prim 'equal? (list (BoolV #t) (BoolV #f))) (BoolV #f))
;; arrays are equal only if they are the same array
(check-equal? (prim 'equal? (list (ArrV 22 3) (ArrV 22 3))) (BoolV #t))
(check-equal? (prim 'equal? (list (ArrV 22 3) (ArrV 25 3))) (BoolV #f))
;; null equals itself and nothing else
(check-equal? (prim 'equal? (list (NullV) (NullV))) (BoolV #t))
(check-equal? (prim 'equal? (list (NullV) (NumV 0))) (BoolV #f))

;; strings
(check-equal? (prim 'strlen (list (StrV "abc"))) (NumV 3))
(check-equal? (prim 'substring (list (StrV "hello") (NumV 1) (NumV 3))) (StrV "el"))
(check-equal? (prim '++ (list (StrV "a") (NumV 1) (BoolV #t))) (StrV "a1true"))

;; arrays
(check-equal? (prim 'make-array (list (NumV 3) (NumV 0))) (ArrV 23 3))
(check-equal? (prim 'array (list (NumV 1) (NumV 2))) (ArrV 23 2))
(define sto-arr : Store (make-initial-store 100))
(define arr1 (interp-prim 'array (list (NumV 5) (NumV 6) (NumV 7)) sto-arr))
(check-equal? (interp-prim 'aref (list arr1 (NumV 2)) sto-arr) (NumV 7))
(check-equal? (interp-prim 'aset! (list arr1 (NumV 0) (StrV "x")) sto-arr) (NullV))
(check-equal? (interp-prim 'aref (list arr1 (NumV 0)) sto-arr) (StrV "x"))
;; a second array is allocated right after the first
(check-equal? (interp-prim 'array (list (NumV 1)) sto-arr) (ArrV 26 1))
 
;; primitive errors
(check-exn (regexp (regexp-quote "DRMG: Division by zero"))
           (lambda () (prim '/ (list (NumV 5) (NumV 0)))))
(check-exn #rx"Expected two numbers" (lambda () (prim '+ (list (NumV 2) (StrV "hello")))))
(check-exn #rx"Expected two numbers" (lambda () (prim '- (list (NumV 2) (StrV "hello")))))
(check-exn #rx"Expected two numbers" (lambda () (prim '* (list (NumV 2)))))
(check-exn #rx"DRMG" (lambda () (prim 'strlen (list (NumV 1)))))
(check-exn #rx"DRMG" (lambda () (prim 'substring (list (StrV "hi") (NumV 1) (NumV 5)))))
(check-exn #rx"DRMG" (lambda () (prim 'substring (list (StrV "hi") (NumV 2) (NumV 1)))))
(check-exn #rx"DRMG" (lambda () (prim 'make-array (list (NumV 0) (NumV 1)))))
(check-exn #rx"DRMG" (lambda () (prim 'array '())))
(check-exn #rx"DRMG" (lambda () (prim 'make-array (list (NumV 1000) (NumV 0)))))  ; out of memory
(check-exn #rx"DRMG" (lambda () (interp-prim 'aref (list arr1 (NumV 3)) sto-arr)))
(check-exn #rx"DRMG" (lambda () (interp-prim 'aref (list arr1 (NumV -1)) sto-arr)))
(check-exn #rx"DRMG" (lambda () (interp-prim 'aset! (list arr1 (NumV 3) (NumV 0)) sto-arr)))
(check-exn #rx"DRMG" (lambda () (prim 'aref (list (NumV 1) (NumV 0)))))
;; more arithmetic errors
(check-exn #rx"DRMG: Expected two numbers" (lambda () (prim '+ (list (NumV 2))))) 
(check-exn #rx"DRMG: Expected two numbers" (lambda () (prim '* (list (NumV 2) (NumV 3) (NumV 4)))))

;; division by zero
(check-exn #rx"DRMG: Division by zero" (lambda () (prim '/ (list (NumV 5) (NumV 0)))))

;; unknown primitives
(check-exn #rx"DRMG: Unknown primitive operator" (lambda () (prim '? (list (NumV 2) (NumV 10)))))
(check-exn #rx"DRMG: Unknown primitive operator" (lambda () (prim 'unknown (list (StrV "hello")))))
(check-exn #rx"DRMG: Unknown primitive operator"
           (lambda () (prim 'function (list (StrV "hello") (NumV 1) (NumV 3)))))

;; custom errors
(check-exn #rx"user-error: .my custom error." (lambda () (prim 'error (list (StrV "my custom error")))))
(check-exn #rx"user-error: 3" (lambda () (prim 'error (list (NumV 3)))))
(check-exn #rx"DRMG: error expects one argument" (lambda () (prim 'error (list (NumV 3) (StrV "hi")))))
(check-exn #rx"DRMG: error expects one argument" (lambda () (prim 'error '())))

;; substring errors
(check-exn #rx"DRMG: Invalid substring indices"
           (lambda () (prim 'substring (list (StrV "hello") (NumV -1) (NumV 2)))))
(check-exn #rx"DRMG: Invalid substring indices"
           (lambda () (prim 'substring (list (StrV "hello") (NumV 1) (NumV -1)))))
(check-exn #rx"DRMG: Range exceeds string length"
           (lambda () (prim 'substring (list (StrV "hello") (NumV 1) (NumV 6)))))
(check-exn #rx"DRMG: Second input is less than first input"
           (lambda () (prim 'substring (list (StrV "hello") (NumV 4) (NumV 2)))))

;; equal? with the wrong number of arguments
(check-exn #rx"DRMG: equal\\? expects two arguments" (lambda () (prim 'equal? (list (NumV 5)))))


(check-equal? (top-interp '{aref {array 5 6 7} 1} 100) "6")
(check-exn #rx"DRMG" (lambda () (top-interp '{aref {array 1 2} 2} 100)))
(check-equal? (top-interp '{local [a = {array 1 2}] in {equal? a a}} 100) "true")
(check-equal? (top-interp '{equal? {array 1} {array 1}} 100) "false")
(check-equal? (top-interp '{rocal [f = {% n -> {if {<= n 0} 0 {+ n {f {- n 1}}}}}] in {f 3}} 200) "6")
(check-exn #rx"DRMG" (lambda () (top-interp '{make-array 1000 0} 100)))
