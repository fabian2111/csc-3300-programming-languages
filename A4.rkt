#lang typed/racket
(require typed/rackunit)

(define-type ExprC (U NumV IdC StrV if % AppC))
(struct NumV ([num : Real]) #:transparent)
(struct IdC ([sym : Symbol]) #:transparent)
(struct StrV ([str : String]) #:transparent)
(struct if ([expr : ExprC] [ifTrue : ExprC] [else : ExprC]) #:transparent)
;(struct local
(struct % ([ids : (Listof IdC)] [body : ExprC]) #:transparent)
(struct AppC ([a : Real]) #:transparent)
;(struct

(define-type Value (U NumV BoolV StrV CloV))
(struct BoolV ([boo : Boolean]) #:transparent)
(struct CloV ([params : (Listof IdC)] [body : ExprC] [env : Env]) #:transparent)

(define-type Binding Bind)
(struct Bind ([name : Symbol] [val : Real]) #:transparent)

(define top-env 3)

(define (serialize [val : Value]) : String
  (match val
    [(BoolV b) (if b "true" "false")]
    [(NumC n) (~v n)]
    [(StrV s) (~v s)]
    [(CloV _ _ _) "#<procedure>"]
    ;add primitive ops
  ))



;Takes in a Sexp and parses it into an ExprC
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? n) (NumV n)]
    [(? string? s) (StrV s)]
  ))


#;(define (interp [e : ExprC] [env : Env]) : Value)

#;(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))




