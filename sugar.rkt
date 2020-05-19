#lang racket

(require racket/include)
(include "utils.rkt")


(define (simple? exp)
  (match exp
    [`(λ . ,_)     #f]
    [(? number?)   #t]
    [(? string?)   #t]
    [(? boolean?)  #t]
    [`(quote . ,_) #t]
    ['(void)       #t]
    [else          #f]))

(define (simpleDefine? def)
  (match def
    [`(define ,v ,exp)  (simple? exp)]
    [else               #f]))



;
; ---------------- Program -----------------
;
(define (deusgarTopLevel exprs)
  (define (desugarer exp)
    (match exp
      ; ... -> matches any number of values of preceding pattern;
      ; .   -> shorthand for cons (aka "make a list")
      [`(define (,f ,params ...) . ,body)           `(define ,f (λ ,params . ,body))]
      ; regular defines do not bother us - leave them as is
      [`(define ,v ,any)                            `(define ,v ,any)]
      ; for any other expr create a define
      [any                                          `(define ,(gensym '_) ,any)]))

  (map desugarer exprs))



(define (desugarBody body)
  (match body
    [`(,exp)                                        (desugarExp exp)]
    [`(,tops ... ,exp)
      (define defs (deusgarTopLevel tops))
      (desugarExp (match defs
                    [`((define ,vs ,es) ...)        `(letrec ,(map list vs es) ,exp)]))]))



(define (desugarExp exp)
  (match exp
    ; primitive cases stay same
    [(? symbol?)                                    exp]
    [(? number?)                                    exp]
    [(? string?)                                    exp]
    [(? boolean?)                                   exp]
    [(? void?)                                      exp]

    ; ,@ == unquote-splicing
    ; let -> lambda
    [`(let ((,vs ,es) ...) . ,body)
     `((lambda ,vs ,(desugarBody body))
       ,@(map desugarExp es))]

    [`(lambda ,params . ,body)                      `(lambda ,params ,(desugarBody body))]

    ; cond -> nested IFs
    [`(cond)                                        '(void)]
    [`(cond (else ,exp))                            (desugarExp exp)]
    [`(cond (,condition ,then))                     `(if ,(desugarExp condition)
                                                         ,(desugarExp then)
                                                          (void))]
    [`(cond (,condition ,then) ,rest ...)           `(if ,(desugarExp condition)
                                                         ,(desugarExp then)
                                                         ,(desugarExp `(cond . ,rest)))]

    ; unwrap IFs
    [`(if ,condition ,then)                         `(if ,(desugarExp condition) 
                                                         ,(desugarExp then) 
                                                          (void))]

    [`(if ,condition ,then ,else)                   `(if ,(desugarExp condition)
                                                         ,(desugarExp then)
                                                         ,(desugarExp else))]

    [`(set! ,v ,exp)                                `(set! ,v ,(desugarExp exp))]

    ; func
    [`(,f . ,args)                                  `(,(desugarExp f) ,@(map desugarExp args))]

    ; ---
    [else                                           (error (format "error on desugarExp: ~s\n" exp))]))



; desugar-define : define-term -> exp
(define (desugarDefine exp)
  (match exp
    [`(define ,v ,any)                              `(define ,v ,(desugarExp any))]
    [else                                           (error "error on desugarDefine")]))



(define (desugarProgram prog)
  (set! prog (deusgarTopLevel prog))
  (set! prog (map desugarDefine prog))
  (set! prog
    (partition
     simpleDefine?
     prog
     (λ (variables complex)
       (define funcDeclarations
         (for/list ([c complex])
           (match c
             [`(define ,v ,complex)                 `(,v (void))]
             [else                                  (error "error on funcDeclarations")])))

       (define (functionDeclarationMapper exp)
          (match exp
            [`(,name ,voidBody)                     `(define ,name ,voidBody)]
            [else                                   (error (format "mapper error: ~s\n" exp))]))

       (define functionDefinitions
         (for/list ([c complex])
           (match c
             [`(define ,v ,complex)                 `(set! ,v ,complex)]
             [else                                  (error "error on functionDefinitions")])))

       (displayln "\n--------------- Simple variables ---------------")
       (displayln (pretty-format variables 30))

       (displayln "\n------------- Function declaration -------------")
       (displayln (pretty-format funcDeclarations 30))

       (displayln "\n------------- Function definitions -------------")
       (displayln (pretty-format functionDefinitions 40))

       (displayln "\n------------------- Result ---------------------")
       (append
            variables
            (map functionDeclarationMapper funcDeclarations)
            functionDefinitions)
       )))
  prog)




; program being desugared
; NOTE: remove first and last parentheses from expression when pasting here
(define stdInStub
  '(
    (define (fact n)
      			(if (= n 0)
          				1
          				(* n (fact (- n 1)))))

    (display (fact 5))
    ))



(define (main prog)
	(displayln (pretty-format prog 40))
	(newline)

 	(displayln "         |")
 	(displayln "      desugar")
 	(displayln "         |")
 	(displayln "         V")

 	(set! prog (desugarProgram prog))
	(displayln (pretty-format prog 40)))


; TODO read from stdin
;(main (read))
(main stdInStub)
