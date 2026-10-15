#lang typed/racket
(require typed/rackunit)

(define-type ExprC (U NumV IdC StrV If % AppC))
(struct NumV ([num : Real]) #:transparent)
(struct IdC ([sym : Symbol]) #:transparent)
(struct StrV ([str : String]) #:transparent)
(struct If ([expr : ExprC] [ifTrue : ExprC] [else : ExprC]) #:transparent)
;(struct local
(struct % ([ids : (Listof IdC)] [body : ExprC]) #:transparent)
;(struct AppC ([a : Real]) #:transparent)
(struct AppC ([fun : ExprC] [args : (Listof ExprC)])#:transparent)

(struct PrimV ([op : Symbol]) #:transparent)
;(define-type Value (U NumV BoolV StrV CloV PrimV))
(define-type Value(U NumV BoolV StrV CloV PrimV))
(struct BoolV ([boo : Boolean]) #:transparent)
;(struct CloV ([params : (Listof IdC)] [body : ExprC] [env : Env]) #:transparent)
(struct CloV ([params : (Listof IdC)][body : ExprC][env : Env]) #:transparent)



(define-type Binding Bind)
;(struct Bind ([name : Symbol] [val : Real]) #:transparent)
(struct Bind ([name : Symbol] [val : Value]) #:transparent)

;(define-type Env (Listof Binding))
(define-type Env (Listof Bind))

(define top-env empty)
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

; -----------------------------------Parser---------------------------------------------------
;Takes in a Sexp and parses it into an ExprC
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? n) (NumV n)]
    [(? string? s) (StrV s)]
    [(? symbol? sym) (IdC sym)]
    [(list 'if condition ifTrue ifFalse) (If (parse condition) (parse ifTrue) (parse ifFalse))]
    [(list '% (list (? symbol? params)...) '->  body ) (% (map IdC (cast params (Listof Symbol))) (parse body))]
    [(list fun args ...) (AppC (parse fun) (map parse args))]
  ))

(check-equal? (parse 4) {NumV 4})
(check-equal? (parse "test") {StrV "test"})
(check-equal? (parse 'Hello) {IdC 'Hello})
(check-equal? (parse '{if true 10 20})(If (IdC 'true)(NumV 10) (NumV 20)))
(check-equal? (parse '{% (x y) -> x}) (% (list (IdC 'x) (IdC 'y)) (IdC 'x)))
(check-equal?(parse '{+ 2 3})(AppC (IdC '+)(list (NumV 2) (NumV 3))))


; -------------------------------------------------------------------------------------------------
#;(define (interp [e : ExprC] [env : Env]) : Value
  (match e
    [(IdC s) (lookup s env)]
    [(NumV n) (NumV n)]
    [(StrV s) (StrV s)]
   ; [(AppC f a) (interp (

  ))

#;(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))




