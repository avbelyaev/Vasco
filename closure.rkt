#lang racket

(require racket/include)
(include "utils.rkt")


; program being closure-converted
; TODO: read from stdin
; (define test
;     '(
;        lambda (f)
;         (lambda (x)
;          (f x a))
;      ))



(define (substitute sub exp)
  (match exp
    [`(lambda ,params ,body)
      (define params* (apply set params))
      (define sub* 
        (for/hash ([(k v) sub] #:when (not (set-member? params* k)))
          (values k v)))
      `(lambda ,params ,(substitute sub* body))]
    
    [`(lambda* ,params ,body)
     ; should not have free variables
      (define params* (apply set params))
      (define sub* 
        (for/hash ([(k v) sub] #:when (not (set-member? params* k)))
          (values k v)))
      `(lambda* ,params ,(substitute sub* body))]
    
    [(? symbol?)
     (if (hash-has-key? sub exp)
         (hash-ref sub exp)
         exp)]
    
    [`(closure* ,lam ,env)
     `(closure* ,(substitute sub lam) ,(substitute sub env))]
    
    [`(env* (,vs ,es) ...)
     `(env* ,@(map list vs (map (substitute-with sub) es)))]
    
    [`(envGet ,env ,v)
     `(envGet ,(substitute sub env) ,v)]
    
    [`(applyclosure ,f ,args ...)
     `(applyclosure ,@(map (substitute-with sub) `(,f . ,args)))]
    
    [`(,f ,args ...)
     (map (substitute-with sub) `(,f . ,args))]))


(define (substitute-with sub)
  (lambda (exp)
    (substitute sub exp)))


(define (findFreeVars exp)
  ; (printf "findFreeVars: ~s\n" exp)
  (match exp
    [(? symbol?)                  (list exp)]
    ; {free vars} = {vars in body}.removeAll({vars in params})
    [`(lambda ,params ,body)      (removeAll (findFreeVars body) params)]
    [`(,func ,args ...)           ;(printf "func: ~s args: ~s\n" func args)
                                  (define fv (map findFreeVars `(,func . ,args)))
                                  ; (printf "fv: ") (displayln fv)
                                  (apply set-union fv)]
    ; ---
    [else (error (format "error on findFreeVars: ~s\n" exp))]))


(define (newClosure exp)
  (define envID (gensym "CLOSURE"))
  (printf "_ ~s _\n" envID)

  (define originalParams  (cadr exp))
  (define modifiedParams  (cons envID originalParams))
  (define originalBody    (convertClosure (caddr exp)))
  (define freeVars        (findFreeVars exp))
  (define env             (map (lambda (v) `(,v ,v)) freeVars))
  ; create hash table where "k" is a var in body to be replaced with env-reference "v"
  (define replacements    (for/hash ((v freeVars)) (values v `(envGet ,envID ,v))))
  (define modifiedBody    (substitute replacements originalBody))

  (printf "params before| ~s\n" originalParams )
  (printf "params after | ~s\n" modifiedParams)
  (printf "body before  | ~s\n" originalBody)
  (printf "free vars    | ~s\n" freeVars)
  (printf "env          | ~s\n" env)
  (printf "replacements | ~s\n" replacements)
  (printf "body after   | ~s\n" modifiedBody)
  (printf "_____________|\n")
  
  `(closure* 
      (lambda* ,modifiedParams ,modifiedBody) 
      (env* ,@env)))


(define (convertClosure exp)
  (match exp
    ; for example converting (lambda (x) (f x a)):
    ; to convert `f`
    [(? symbol?)                exp]
    ; to convert `(lambda (x) (f x a))`
    [`(lambda ,params ,body)    (newClosure exp)]
    ; to convert `(f x a)`
    [(? pair?)                  (map convertClosure exp)]
    ; ---
    [else (error (format "error on convertClosure: ~s\n" exp))]))


(define (main input)
  (displayln "converting closure...")
  (pretty-write input)

  (define converted (convertClosure input))

  (displayln "-------------------")
  (pretty-write converted))



(main test)
