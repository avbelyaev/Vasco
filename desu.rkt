#lang racket

(require racket/include)
(include "utils.rkt")


; const? : exp -> boolean
(define (const? exp)
  (or (integer? exp)
      (boolean? exp)))

; ref? : exp -> boolean
(define (ref? exp)
  (symbol? exp))

; let? : exp -> boolean
(define (let? exp)
  (tagged-list? 'let exp))

; let->bindings : let-exp -> alist[symbol,exp]
(define (let->bindings exp)
  (cadr exp))

; let->exp : let-exp -> exp
(define (let->exp exp)
  (caddr exp))

; let->bound-vars : let-exp -> list[symbol]
(define (let->bound-vars exp)
  (map car (cadr exp)))

; let->args : let-exp -> list[exp]
(define (let->args exp)
  (map cadr (cadr exp)))


; lambda? : exp -> boolean
(define (lambda? exp)
  (tagged-list? 'lambda exp))

; lambda->formals : lambda-exp -> list[symbol]
(define (lambda->formals exp)
  (cadr exp))

; lambda->exp : lambda-exp -> exp
(define (lambda->exp exp)
  (caddr exp))

; if? : exp -> boolean
(define (if? exp)
  (tagged-list? 'if exp))

; if->condition : if-exp -> exp
(define (if->condition exp)
  (cadr exp))

; if->then : if-exp -> exp
(define (if->then exp)
  (caddr exp))

; if->else : if-exp -> exp
(define (if->else exp)
  (cadddr exp))

; app? : exp -> boolean
(define (app? exp)
  (pair? exp))

; prim? : exp -> boolean
(define (prim? exp)
  (or (eq? exp '+)
      (eq? exp '-)
      (eq? exp '*)
      (eq? exp '=)
      (eq? exp 'display)))

; begin? : exp -> boolean
(define (begin? exp)
  (tagged-list? 'begin exp))

; begin->exps : begin-exp -> list[exp]
(define (begin->exps exp)
  (cdr exp))


;; Syntax manipulation.

; substitute-var : alist[var,exp] ref-exp -> exp
(define (substitute-var env var)
  (let ((sub (assq var env)))
    (if sub
        (cadr sub)
        var)))

; substitute : alist[var,exp] exp -> exp
(define (substitute env exp)

  (define (substitute-with env)
    (lambda (exp)
      (substitute env exp)))

  (cond
    ; Core forms:
    ((null? env)        exp)
    ((const? exp)       exp)
    ((prim? exp)        exp)
    ((ref? exp)         (substitute-var env exp))
    ((lambda? exp)      `(lambda ,(lambda->formals exp)
                           ,(substitute (assq-remove-keys env (lambda->formals exp))
                                        (lambda->exp exp))))

    ((if? exp)          `(if ,(substitute env (if->condition exp))
                             ,(substitute env (if->then exp))
                             ,(substitute env (if->else exp))))

    ; Sugar:
    ((let? exp)         `(let ,(azip (let->bound-vars exp)
                                     (map (substitute-with env) (let->args exp)))
                           ,(substitute (assq-remove-keys env (let->bound-vars exp))
                                        (let->exp exp))))

    ((begin? exp)       (cons 'begin (map (substitute-with env) (begin->exps exp))))

    ; Application:
    ((app? exp)         (map (substitute-with env) exp))
    (else               (error "unhandled expression type in substitution: " exp))))




;; Desugaring.

; let=>lambda : let-exp -> app-exp
(define (let=>lambda exp)
  (if (let? exp)
      (let ((vars (map car (let->bindings exp)))
            (args (map cadr (let->bindings exp))))
        `((lambda (,@vars) ,(let->exp exp)) ,@args))
      exp))

; begin=>let : begin-exp -> let-exp
(define (begin=>let exp)
  (define (singlet? l)
    (and (list? l)
         (= (length l) 1)))

  (define (dummy-bind exps)
    (cond
      ((singlet? exps)  (car exps))

      ((pair? exps)     `(let (($_ ,(car exps)))
                          ,(dummy-bind (cdr exps))))))
  (dummy-bind (begin->exps exp)))

; desugar : exp -> exp
(define (desugar exp)
  (cond
    ; Core forms:
    ((const? exp)      exp)
    ((prim? exp)       exp)
    ((ref? exp)        exp)
    ((lambda? exp)     `(lambda ,(lambda->formals exp)
                          ,(desugar (lambda->exp exp))))
    ((if? exp)         `(if ,(if->condition exp)
                            ,(if->then exp)
                            ,(if->else exp)))
    ; Sugar:
    ((begin? exp)      (desugar (begin=>let exp)))
    ((let? exp)        (desugar (let=>lambda exp)))

    ; Applications:
    ((app? exp)        (map desugar exp))
    (else              (error "unknown exp: " exp))))


; program being desugared
; TODO: read from stdin
; NOTE: remove first and last parentheses from expression when pasting here
(define test
	'(
	  define (summator x)
        (let ((y 123)
              (z 456))
          (+ x y z))
	))



 (define (main input)
 	(pretty-write input)

 	(displayln "      |")
 	(displayln "   desugar")
 	(displayln "      |")
 	(displayln "      V")

 	(define desugared (desugar test))
	(pretty-write desugared))



(main test)
