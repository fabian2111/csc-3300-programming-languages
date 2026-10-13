#lang typed/racket
(require typed/rackunit)

(define-type ExprC (U NumV IdC StrV If % AppC))
(struct NumV ([num : Real]) #:transparent)
(struct IdC ([sym : Symbol]) #:transparent)
(struct StrV ([str : String]) #:transparent)
(struct If ([expr : ExprC] [ifTrue : ExprC] [else : ExprC]) #:transparent)
;(struct local
(struct % ([ids : (Listof IdC)] [body : ExprC]) #:transparent)
(struct AppC ([a : Real]) #:transparent)
;(struct

(define-type Value (U NumV BoolV StrV CloV))
(struct BoolV ([boo : Boolean]) #:transparent)
(struct CloV ([params : (Listof IdC)] [body : ExprC] [env : Env]) #:transparent)

(define-type Binding Bind)
(struct Bind ([name : Symbol] [val : Real]) #:transparent)

(define-type Env (Listof Binding))

(define top-env empty)
(define extend-env cons)

(define (serialize [val : Value]) : String
  (match val
    [(BoolV b) (if b "true" "false")]
    [(NumV n) (~v n)]
    [(StrV s) (~v s)]
    [(CloV _ _ _) "#<procedure>"]
    ;add primitive ops
  ))

;???
#;(define (lookup [sym : Symbol] [env : Env]) : Value
    (cond
      [(empty? env) (error 'lookup "name not found")]
      [else (cond
              [(symbol=? for (Bind-name (first env)))
               (bind-val (first env))]
              [else (lookup for (rest env))])]))



;Takes in a Sexp and parses it into an ExprC
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? n) (NumV n)]
    [(? string? s) (StrV s)]
  ))

(check-equal? (parse 4) {NumV 4})
(check-equal? (parse "test") {StrV "test"})


#;(define (interp [e : ExprC] [env : Env]) : Value
  (match e
    [(IdC s) (lookup s env)]
    [(NumV n) (NumV n)]
    [(StrV s) (StrV s)]
    [(AppC f a) (interp (

  ))

#;(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))




