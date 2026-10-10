#lang typed/racket
(require typed/rackunit)

(define-type ExprC (U NumV IdC StrV if % AppC))
(struct NumV ([num : Real]) #:transparent)
(struct IdC ([sym : Symbol]) #:transparent)
(struct StrV ([string : String]) #:transparent)
(struct if ([expr : ExprC] [ifTrue : ExprC] [else : ExprC]) #:transparent)
;(struct local
(struct % ([ids : (Listof IdC)] [body : ExprC]) #:transparent)
(struct AppC ([a : Real]) #:transparent)
;(struct

(define-type Value (U NumV BoolV StrV CloV))
(struct BoolV ([boo : Boolean]) #:transparent)
(struct CloV ([changeThis : Symbol]) #:transparent)



#;(define (parse [s : Sexp]) : ExprC)

#;(define (interp [e : ExprC] [env : Environment]) : Value)

#;(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))




