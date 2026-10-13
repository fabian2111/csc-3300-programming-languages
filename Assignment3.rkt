#lang typed/racket
(require typed/rackunit)

;Full project implemented except for checking for duplicate functions

(define operators
  (hash
   '+ +
   '* *
   '/ /))

(struct ifleq0? ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct NumC ([n : Real]) #:transparent)

(define-type Binop (U NumC ExprC ifleq0?))
(struct BinopC ([op : Symbol] [left : ExprC] [right : ExprC]) #:transparent)


(struct IdC ([s : Symbol]) #:transparent)
(struct AppC ([fun : Symbol] [args : (Listof ExprC)]) #:transparent)

(define-type ExprC (U NumC BinopC ifleq0? IdC AppC))

(struct FundefC ([name : Symbol] [args : (Listof Symbol)] [body : ExprC]) #:transparent)


;-----------------------------parser--------------------

;Checks if the given Sexp is a operator
(define (operator? [s : Sexp]) : Boolean
  (hash-has-key? operators s))

;Takes in a Sexp and parses it into a ExprC
(define (parse [input : Sexp]) : ExprC
   (match input
     [(? real? n) (NumC n)]
     [(? symbol? s) (IdC s)]
     [(list 'ifleq0? test left right) (ifleq0? (parse test) (parse left) (parse right))]
     [(list (? operator? op) left right) (BinopC (cast op Symbol) (parse left) (parse right))]
     [(list (? symbol? fun) arg ...) (AppC fun (map parse (cast arg (Listof Sexp))))])) 


;--------------------'{double x = {+ x x}}  ( Parse-fundef ) -----------------------------

;Takes in a Sexp and parses it into a list of symbols that represent the arguments
;for a function while checking for duplicate symbols
(define (parse-args [args : (Listof Sexp)]) : (Listof Symbol)
  (match args
    ['()'()]
    [(cons first rest)
     (cond
       [(not (symbol? first)) (error 'parse-args "Expected a symbol: ~e" first)]
       [(member first rest) (error 'parse-args "duplicate argument: ~e" first)]
       [else (cons first (parse-args rest))])]))

;Takes in a Sexp and parses it into a FundefC
(define (parse-fundef [input : Sexp]) : FundefC
   (match input
    [(list (? symbol? name) (list (? symbol? args) ...) '= body)
     (FundefC name (parse-args (cast args (Listof Symbol))) (parse body))]
     [_ (error 'parse-fundef "invalid function definition: ~e" input)])) 



; -------------- parse - prog ---------------------------------------------------------

;Takes in a Sexp and parses it into a list of FundefC by calling parse-fundef
(define (parse-prog [input : Sexp]) : (Listof FundefC)
  (match input
    ['() '()]
    ;[(list fundefs ...) (map parse-fundef input)]
    [(cons (list (? symbol? f-name) '= expr) r) (cons (parse-fundef (list f-name '() '= expr)) (parse-prog r))]
    [(cons (list (? symbol? f-name) arg ... '= expr) r)
     (cons (parse-fundef (list f-name (cast arg (Listof Symbol)) '= expr)) (parse-prog r))]))  
 
;------------------------------------Interpreter---------------------------------------

; ------ find-fundefC ------------

;Takes in a function name and a list of FundefC and checks if the function is in the list of functions
(define (find-fundefC [name : Symbol] [funs : (Listof FundefC)]) : FundefC
  (match funs
    ['() (error 'find-fundefC "DRMG3 Function name lookup Error, got ~e" name)  ]
    [(cons first rest)
     (cond
       [(eq? (FundefC-name first) name) first ]
       [else
        (find-fundefC name rest)])]))

;----- substitute -----

;Takes in a symbol to replace, a value to replace it with, and a expression to perform it on and
;returns the updated expression
(define (subst [what : Symbol] [for : Real] [in : ExprC]) : ExprC
  (match in
    [ (NumC n) (NumC n )]
    [(IdC x)
        (cond
          [(equal? x what) (NumC for)]
          [else (IdC x)])]
    [(BinopC op left right) (BinopC op (subst what for left) (subst what for right))] 
    [(ifleq0? test then else) (ifleq0? (subst what for test) (subst what for then) (subst what for else))]))  


; --- subst args -------

;Takes in a list of arg names, list of values, and a expression and replaces the args
;in the function with the values provided
(define (subst-args [names : (Listof Symbol)] [values : (Listof Real)] [body : ExprC]) : ExprC
  (match names
    ['() body]
    [(cons first-name rest-names)
     (match values
       [(cons first-value rest-values)
        (subst-args rest-names rest-values
         (subst first-name first-value body))])]))

;------- interp helper ---------

;Takes in a list of args and a list of functions and it evaluates the args
;into values using interp
(define (interp-args [args : (Listof ExprC)] [funs : (Listof FundefC)]) : (Listof Real)
  (match args
    ['()'()]
    [(cons first rest)
     (cons (interp first funs)
           (interp-args rest funs))]))


; ---------- interp main function ----- 

;Takes in an expression and a list of functions and translates the
;expression into a value
(define (interp [expr : ExprC] [funs : (Listof FundefC)] ) : Real
  (match expr
    [(NumC n) n]
    [(ifleq0? test then else )
     (cond
       [(<= (interp test funs) 0) (interp then funs)]
       [else (interp else funs)])]
    [(BinopC s l r) ((hash-ref operators s) (interp l funs) (interp r funs))]
    [(AppC func args) (define fundef (find-fundefC func funs))
           (cond
             [(equal? (length (FundefC-args fundef)) (length args)) (define values (interp-args args funs))
               (interp (subst-args (FundefC-args fundef) values (FundefC-body fundef)) funs)]
                [else (error 'interp "DRMG3: Number of Arguments are not equal, got ~e" args)])]))


;; -------interp-fns --- 

;Takes in a list of functions and calls main 
(define (interp-fns [funs : (Listof FundefC)]) : Real
  (define main (find-fundefC 'main funs))
  (interp (FundefC-body main) funs))

;----------------------Both interpretor & parser ----------------------------

;Takes in input as a Sexp and evaluates the entire input into a value
(define (top-interp [input : Sexp]) : Real
  (interp-fns (parse-prog input)))

; --------------------------------



;Incorrect number of Parameters
(check-exn (regexp (regexp-quote "DRMG3: Number of Arguments are not equal, got (list (NumC 50))"))
           (lambda () (top-interp '{{double x y = {* 2 x}} {main = {double 50}}})))
(check-exn (regexp (regexp-quote "DRMG3: Number of Arguments are not equal, got (list (NumC 50) (NumC 50)"))
           (lambda () (top-interp '{{double x = {* 2 x}} {main = {double 50 50}}}))) 


;parse tests
(check-equal? (parse '{five}) (AppC 'five '()))
(check-equal? (parse '{area 4 4}) (AppC 'area (list (NumC 4) (NumC 4))))

;parse-args tests
(check-equal? (parse-args '{x y z}) '{x y z})
(check-equal? (parse-args '{}) '{})
(check-exn (regexp (regexp-quote "parse-args: duplicate argument: 'x")) (lambda () (parse-args '{x x})))
(check-exn (regexp (regexp-quote "parse-args: Expected a symbol: 4")) (lambda () (parse-args '{4})))

;parse-fundef tests
(check-equal? (parse-fundef '{double (x) = {+ 5 10}}) (FundefC 'double '(x) (BinopC '+ (NumC 5) (NumC 10))))
(check-equal? (parse-fundef '{double (x) = {+ x x}}) (FundefC 'double '(x) (BinopC '+ (IdC 'x) (IdC 'x))))
(check-equal? (parse-fundef '{double (x) = {+ 5 x}}) (FundefC 'double '(x) (BinopC '+ (NumC 5) (IdC 'x))))
(check-exn (regexp (regexp-quote "parse-fundef: invalid function definition: '(func = 4 + +)"))
           (lambda () (parse-fundef '{func = 4 + +})))

              
;parse-prog tests
(check-equal? (parse-prog '{{main = 20}}) {list {FundefC 'main '() {NumC 20}}})
(check-equal? (parse-prog '{{func x = {+ 20 x}} {main = {f 20}}})
              {list (FundefC 'func '(x) {BinopC '+ {NumC 20} {IdC 'x}}) (FundefC 'main '() {AppC 'f (list {NumC 20})})})

;find-fundefC tests
(check-equal? (find-fundefC 'fun
                            {list {FundefC 'fun '{x} {BinopC '+ {NumC 4} {IdC 'x}}}
                                  {FundefC 'main '() {NumC 4}}})
              {FundefC 'fun '{x} {BinopC '+ {NumC 4} {IdC 'x}}})
(check-equal? (find-fundefC 'main
                            {list {FundefC 'fun '{x} {BinopC '+ {NumC 4} {IdC 'x}}} {FundefC 'main '() {NumC 4}}})
              {FundefC 'main '{} {NumC 4}})

(check-exn (regexp (regexp-quote "find-fundefC: DRMG3 Function name lookup Error, got 'nofunc"))
           (lambda () (find-fundefC 'nofunc
                                    {list {FundefC 'fun '{x} {BinopC '+ {NumC 4} {IdC 'x}}}
                                          {FundefC 'main '() {NumC 4}}})))

;substitute tests
(check-equal? (subst 'x 5 (IdC 'x)) (NumC 5))
(check-equal? (subst 'x 5 (BinopC '+ (IdC 'x) (IdC 'x))) (BinopC '+ (NumC 5) (NumC 5)))
(check-equal? (subst 'x 5 (ifleq0? (IdC 'x) (NumC 10) (NumC 5))) (ifleq0? (NumC 5) (NumC 10) (NumC 5)))

;subst-args tests
(check-equal? (subst-args '(x y) '(5 10) (BinopC '+ (IdC 'x) (IdC 'y))) (BinopC '+ (NumC 5) (NumC 10)))
(check-equal? (subst-args '() '() (BinopC '+ (NumC 3) (NumC 3))) (BinopC '+ (NumC 3) (NumC 3))) 

;interp-args tests
(check-equal? (interp-args {list {BinopC '+ {NumC 10} {NumC 20}}} '()) {list 30})
(check-equal? (interp-args {list {BinopC '+ {NumC 2} {NumC 4}} {BinopC '* {NumC 3} {NumC 1}}} '()) {list 6 3})

;interp tests
(check-equal? (interp {NumC 3} '()) 3)
(check-equal? (interp {ifleq0? {NumC -3} {NumC 4} {NumC 0}} '()) 4)
(check-equal? (interp {ifleq0? {NumC 5} {NumC 4} {NumC 0}} '()) 0)
(check-equal? (interp {BinopC '+ {NumC 10} {NumC 20}} '()) 30)
(check-equal? (interp {AppC 'func {list {NumC 3}}} {list {FundefC 'func '(x) {BinopC '+ (IdC 'x) (NumC 10)}}}) 13)
(check-exn (regexp (regexp-quote "interp: DRMG3: Number of Arguments are not equal, got '()"))
           (lambda () (interp {AppC 'func '()} {list {FundefC 'func '(x) {BinopC '+ (IdC 'x) (NumC 10)}}}))) 


;interp-fns tests
(check-equal? (interp-fns {list {FundefC 'f '() {NumC 5}} {FundefC 'main '() {NumC 5}}}) 5)
(check-equal? (interp-fns {list {FundefC 'f '() {NumC 5}} {FundefC 'main '() {AppC 'f '()}}}) 5)


;top-interp tests
(check-equal? (top-interp '({five = 10} {main = {five}})) 10)
(check-equal? (top-interp '({main = {ifleq0? -5 10 20}})) 10)


; main with no function call  
(check-equal? (top-interp '({main = 5})) 5) 

;Double function 
(check-equal? (top-interp '{{double x = {+ x x}} {main = {double 5}}}) 10)

(check-equal? (top-interp '{{double x = {+ x x}} {main = {double 5}}}) 10)

;Multiple arguments
(check-equal? (top-interp '{{area w h = {* w h}} {main = {area 4 5}}}) 20)

;Zero Arguements
(check-equal? (top-interp '{{five = 5} {main = {five}}})5)

; Eager Test
(check-equal? (top-interp '{{double x = {+ x x}} {main = {double {+ 2 3}}}}) 10)

;Multiple Arguements with Expression
(check-equal? (top-interp '{{area w h = {* w h}} {main = {area {+ 1 3} {* 2 3}}}})24)




