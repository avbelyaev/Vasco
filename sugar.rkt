#lang racket




; define? : term -> boolean
(define (define? sx)
  (match sx
    [`(define . ,_) #t]
    [else           #f]))

; not-define? : term -> boolean
(define (not-define? sx)
  (not (define? sx)))

; atomic? : term -> boolean
(define (atomic? exp)
  (match exp
    [`(λ . ,_)     #f]
    [(? number?)   #t]
    [(? string?)   #t]
    [(? boolean?)  #t]
    [`(quote . ,_) #t]
    ['(void)       #t]
    [else          #f]))

; atomic-define? : term -> boolean
(define (atomic-define? def)
  (match def
    [`(define ,v ,exp)  (atomic? exp)]
    [else               #f]))



; tops-to-defs : top list -> def list
(define (tops-to-defs tops)
  (define (top-to-def top)
    (match top
      [`(define (,f ,params ...) . ,body)   `(define ,f (λ ,params . ,body))]
      [`(define ,v ,exp)                    `(define ,v ,exp)]
      [exp                                  `(define ,(gensym '_) ,exp)]))
  (map top-to-def tops))




(define (desugar-body body)
  (match body
    [`(,exp)
     (desugar-exp exp)]

    [`(,(and (? not-define?) exps) ...)
     `(begin ,@(map desugar-exp exps))]

    [`(,tops ... ,exp)
     (define defs (tops-to-defs tops))
     (desugar-exp (match defs
                    [`((define ,vs ,es) ...)
                     `(letrec ,(map list vs es) ,exp)]))]))




; desugar-exp : exp -> exp
(define (desugar-exp exp)
  (match exp
    [(? symbol?)      exp]

    [`(let ((,vs ,es) ...) . ,body)
     `((λ ,vs ,(desugar-body body))
       ,@(map desugar-exp es))]

    [`(letrec ((,vs ,es) ...) . ,body)
     (desugar-exp
      `(let ,(for/list ([v vs])
               (list v '(void)))
         ,@(map (λ (v e)
                  `(set! ,v ,e))
                vs es)
         ,@body))]

    [`(λ ,params . ,body)
     `(λ ,params ,(desugar-body body))]

    [`(cond)
     '(void)]

    [`(cond (else ,exp))
     (desugar-exp exp)]

    [`(cond (,test ,exp))
     `(if ,(desugar-exp test)
          ,(desugar-exp exp)
          (void))]

    [`(cond (,test ,exp) ,rest ...)
     `(if ,(desugar-exp test)
          ,(desugar-exp exp)
          ,(desugar-exp `(cond . ,rest)))]

    [`(and)   #t]
    [`(or)    #f]

    [`(or ,exp)
     (desugar-exp exp)]

    [`(and ,exp)
     (desugar-exp exp)]

    [`(or ,exp . ,rest)
     (define $t (gensym 't))
     (desugar-exp
      `(let ((,$t ,exp))
         (if ,$t ,$t (or . ,rest))))]

    [`(and ,exp . ,rest)
     `(if ,(desugar-exp exp)
          ,(desugar-exp `(and . ,rest))
          #f)]

    [`(if ,test ,exp)
     `(if ,(desugar-exp test) ,(desugar-exp exp) (void))]

    [`(if ,test ,exp1 ,exp2)
     `(if ,(desugar-exp test)
          ,(desugar-exp exp1)
          ,(desugar-exp exp2))]

    [`(set! ,v ,exp)
     `(set! ,v ,(desugar-exp exp))]


    ;[`(begin . ,body)
    ; (desugar-body body)]

    [(? atomic?)      exp]

    [`(,f . ,args)
     `(,(desugar-exp f) ,@(map desugar-exp args))]

    [else
     (printf "desugar fail: ~s~n" exp)
     exp]))



; desugar-define : define-term -> exp
(define (desugar-define def)
  (match def
    [`(define ,v ,exp)   `(define ,v ,(desugar-exp exp))]
    [else                 (error (format "cannot desugar: ~s~n" def))]))



(define (desugar-program prog)
  (set! prog (tops-to-defs prog))
  (set! prog (map desugar-define prog))
  (set! prog
    (partition
     atomic-define?
     prog
     (λ (variables complex)
       (define func-declarations
         (for/list ([c complex])
           (match c
             [`(define ,v ,complex)         `(,v (void))])))

       (define function-definitions
         (for/list ([c complex])
           (match c
             [`(define ,v ,complex)         `(set! ,v ,complex)])))

       (displayln "\n--------------- Simple variables ---------------")
       (displayln (pretty-format variables 40))

       (displayln "\n------------- Function declaration -------------")
       (set! func-declarations (map function-declaration-mapper func-declarations))
       (displayln (pretty-format func-declarations 40))

       (displayln "\n------------- Function definitions -------------")
       (displayln (pretty-format function-definitions 40))

       (displayln "\n------------------- Result ---------------------")
       (append
            variables
            func-declarations
            function-definitions)
       )))
  prog)



(define (function-declaration-mapper exp)
    (match exp
        [`(,name ,voidBody)     `(define ,name ,voidBody)]
        [else                   (error (format "mapper error: ~s\n" exp))]))


(define (partition pred lst k)
  (if (not (pair? lst))
      (k '() '())
      (partition pred (cdr lst) (λ (in out)
        (if (pred (car lst))
            (k (cons (car lst) in) out)
            (k in (cons (car lst) out)))))))



(define test
  '(
    (define (area x) (* pi 2 x x))
    (define pi 3.14)
    (display (area 10))
    (define (foo x)
        (cond
            [(= x 0)  1]
            [(> x 0)  (+ x (foo (- x 1)))]
            [else     'universe-broke]))

    (define bar 20)

    (display (foo bar))
    ))

(display (pretty-format (desugar-program test) 40))
