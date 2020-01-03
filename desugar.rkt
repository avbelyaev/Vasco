#lang racket


; program being desugared
; TODO: read from stdin
(define test 
	'(
	  (define (area x) (* pi 2 x x))
(define pi 3.14)
(display (area 10))

	))



; ---------------- Program -----------------
; Done
(define (desugarTopLevel exp)
	(match exp
		; ... -> matches any number of values of preceding pattern; . -> cons aka "make a list"
		[`(define (,f ,params ...) . ,body) 	`(define ,f (lambda ,params . ,body))]
		; regular defines do not bother us - leave them as is
		[`(define ,var ,anyExp) 				`(define ,var ,anyExp)]
		; for any other expr create a define
		[anyExp 								`(define ,(gensym) ,anyExp)]))

(define (desugarProgram exprs)
	(map desugarTopLevel exprs))



(define (atomic? exp)
  	(match exp
    	[`(lambda . ,_)     #t]
	    [(? number?)   		#t]
	    [(? string?)   		#t]
	    [(? boolean?)  		#t]
	    [`(quote . ,_) 		#t]
	    ['(void)       		#t]
	    [else          		#f]))

(define (atomic-define? def)
  (match def
    [`(define ,v ,exp)  (atomic? exp)]
    [else               #f]))



(define (desugarQuote exp)
  (cond
    [(pair? exp)     `(cons ,(desugarQuote (car exp)) ,(desugarQuote (cdr exp)))]
    [(null? exp)     ''()]
    [(number? exp)   exp]
    [(string? exp)   exp]
    [(boolean? exp)  exp]
    [(symbol? exp)   `(quote ,exp)]
    ; ---
	[else (error (format "error on desugarBody: ~s\n" exp))]))


(define (desugarQuasiQuote n qq-exp)
  (match qq-exp
    [(list 'unquote exp)
     (if (= n 1)
         (desugarExp exp)
         (list 'list ''unquote 
               (desugarQuasiQuote (- n 1) exp)))]
    
    [`(quasiquote ,qq-exp)
     `(list 'quasiquote ,(desugarQuasiQuote (+ n 1) qq-exp))]
    
    [(cons (list 'unquote-splicing exp) rest)
     (if (= n 1)
         `(append ,exp ,(desugarQuasiQuote n rest))
         (cons (list 'unquote-splicing (desugarQuasiQuote (- n 1) exp))
               (desugarQuasiQuote n rest)))]
    
    [`(,qq-exp1 . ,rest)
     `(cons ,(desugarQuasiQuote n qq-exp1)
            ,(desugarQuasiQuote n rest))]
       
    [else 
     (desugarQuote qq-exp)]))



; ---------------- Body ----------------
(define (desugarBody body)
	(match body
		[`(,exp)		(desugarExp exp)]
		; ---
		[else (error (format "error on desugarBody: ~s\n" exp))]))


; ---------------- Exp -----------------
;
; http://matt.might.net/articles/desugaring-scheme/
;
(define (desugarExp exp)
	; (display (printf "desugarExp: ~s\n" exp))
	(match exp
		; primitives cases
		[(? symbol?) 						exp]
		[(? number?)						exp]
		[(? string?)						exp]
		[(? boolean?)						exp]
		; quotes: if (null? xs) '() (car xs)
		[`(quote ,exp)  					(desugarQuote exp)]
		; ,@ -> unquote-splicing whatever it means
		; let -> lambda
		[`(let ((,var ,expr) ...) . ,body) 	`((lambda ,var 
												 	,(desugarBody body)) 
											   ,@(map desugarExp expr))]
		; cond -> nested IFs
		[`(cond)							'(void)]
    	[`(cond (else ,exp))				(desugarExp exp)]
    	[`(cond (,test ,exp))				`(if ,(desugarExp test) 
          								 	 	 ,(desugarExp exp) 
          								  	 	 (void))]
    	[`(cond (,test ,exp) ,rest ...)		`(if ,(desugarExp test)
          									 	 ,(desugarExp exp)
          									 	 ,(desugarExp `(cond . ,rest)))]
		; quotes
		[`(quasiquote ,qq-exp)				(desugarQuasiQuote 1 qq-exp)]
		; func
		[`(,f . ,args) 						`(,(desugarExp f) ,@(map desugarExp args))]
		; ---
		[else (error (format "error on desugarExp: ~s\n" exp))]))



; ---------------- Define -----------------
; Done
(define (desugarDefine exp)
	(match exp
		[`(define ,var ,anyExp) 			`(define ,var ,(desugarExp anyExp))]
		[else (error "error on desugarDefine")]))

(define (desugarDefines exprs)
	(map desugarDefine exprs))






(define (partition-k pred lst k)
  (if (not (pair? lst))
      (k '() '())
      (partition-k pred (cdr lst) (λ (in out)
        (if (pred (car lst))
            (k (cons (car lst) in) out)
            (k in (cons (car lst) out)))))))


(define (lift-complex-expressions prog)
	(partition-k 
     	atomic-define?
    	prog
     	(lambda (atomic complex)
       		(define bindings
         		(for/list ([c complex])
           			(match c
             			[`(define ,v ,complex)			`(,v (void))])))
       
       (define sets
         	(for/list ([c complex])
           		(match c
             		[`(define ,v ,complex)			`(set! ,v ,complex)])))
       
       (append atomic (list `(let ,bindings ,sets))))))


; main desugaring func
(define (desugar prog)
	(displayln "------- 1. desugar program -------")
	(set! prog (desugarProgram prog))
	
	(displayln (pretty-format prog))
	(newline)

	(displayln "------- 2. desugar defines -------")
	(set! prog (desugarDefines prog))
	
	(displayln (pretty-format prog))
	(newline)	

	; TODO enable lifting
	; (displayln "------- 3. lift complex exprs -------")
	; (set! prog (lift-complex-expressions prog))

	; (displayln (pretty-format prog))
	; (newline)

	prog)

	


(define (main)
	(displayln "desugaring...")
	(define desugared (desugar test))

	(displayln "-------------------")
	(pretty-write desugared))



(main)

