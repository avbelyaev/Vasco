#lang racket


; program being desugared
; TODO: read from stdin
(define test 
	'(;(define (my-abs1 x) (if (< x 0) (-x) (x)))
	  ;(define sq (lambda (x) (* x x)))
	  (cond
		((< x 0) -1)
		((= x 0) 0)
		(else 1))
	  (display "my-abs")
	  ; (my-abs 3)
	  ; (define (leSummator)
	  	; (let ((x 123)
	  		  ; (y 456))
	  		 ; (+ x y)))
	  ; (define (sign x)
  		; (cond
    		; ((< x 0) -1)
    		; ((= x 0) 0)
    		; (else 1)))
    		))



; ---------------- Program -----------------
; Done
(define (desugarTopLevel exp)
	(match exp
		; ... -> matches any; . -> cons aka "make a list"
		[`(define (,f ,params ...) . ,body) `(define ,f (lambda ,params . ,body))]
		; regular defines do not bother us - leave them as is
		[`(define ,var ,anyExp) 			`(define ,var ,anyExp)]
		; for any other expr create a define
		[anyExp 							`(define ,(gensym) ,anyExp)]))

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



(define (desugar-quote s-exp)
  (cond
    [(pair? s-exp)     `(cons ,(desugar-quote (car s-exp))
                              ,(desugar-quote (cdr s-exp)))]
    [(null? s-exp)     ''()]
    [(number? s-exp)   s-exp]
    [(string? s-exp)   s-exp]
    [(boolean? s-exp)  s-exp]
    [(symbol? s-exp)   `(quote ,s-exp)]
    [else 
     (error (format "strange value in quote: ~s~n" s-exp))]))


(define (desugar-qq n qq-exp)
  (match qq-exp
    [(list 'unquote exp)
     (if (= n 1)
         (desugarExp exp)
         (list 'list ''unquote 
               (desugar-qq (- n 1) exp)))]
    
    [`(quasiquote ,qq-exp)
     `(list 'quasiquote ,(desugar-qq (+ n 1) qq-exp))]
    
    [(cons (list 'unquote-splicing exp) rest)
     (if (= n 1)
         `(append ,exp ,(desugar-qq n rest))
         (cons (list 'unquote-splicing (desugar-qq (- n 1) exp))
               (desugar-qq n rest)))]
    
    [`(,qq-exp1 . ,rest)
     `(cons ,(desugar-qq n qq-exp1)
            ,(desugar-qq n rest))]
       
    [else 
     (desugar-quote qq-exp)]))


; ---------------- Exp -----------------
(define (desugarExp exp)
	(displayln (printf "desugarExp: ~s\n" exp))
	(match exp
		; primitives cases
		[(? symbol?) 					exp]
		[(? number?)					exp]
		[(? string?)					exp]
		[(? boolean?)					exp]
		; ,@ -> unquote-splicing whatever it means
		; let -> lambda
		[`(let ((,var ,expr) ...) . ,body) 
		 `((lambda ,var ,body) ,@(map desugarExp expr))]
		; cond -> nested IFs
		[`(cond)						'(void)]
    	[`(cond (else ,exp))			(desugarExp exp)]
    	[`(cond (,test ,exp))			`(if ,(desugarExp test) 
          								 	 ,(desugarExp exp) 
          								  	 (void))]
    	[`(cond (,test ,exp) ,rest ...)	`(if ,(desugarExp test)
          									 ,(desugarExp exp)
          									 ,(desugarExp `(cond . ,rest)))]
		; quotes
		[`(quasiquote ,qq-exp)			(desugar-qq 1 qq-exp)]
		; strings, numbers, etc atomic
		[`(? atomic?) 					exp]
		; func
		[`(,f . ,args) 					`(,(desugarExp f) ,@(map desugarExp args))]
		;
		[else (error (format "error on desugarExp: ~s\n" exp))]))



; ---------------- Define -----------------
; Done
(define (desugarDefine exp)
	(match exp
		[`(define ,var ,anyExp) `(define ,var ,(desugarExp anyExp))]
		[else (error "error on desugarDefine")]))

(define (desugarDefines exprs)
	(map desugarDefine exprs))





; ; atomic-define? : term -> boolean
; (define (atomic-define? def)
;   (match def
;     [`(define ,v ,exp)  (atomic? exp)]
;     [else               #f]))



; main desugaring func
(define (desugar prog)
	(set! prog (desugarProgram prog))
	(displayln "1. desugar program")
	(displayln (pretty-format prog))
	(newline)

	(set! prog (desugarDefines prog))
	(displayln "2. desugar defines")
	(displayln (pretty-format prog))
	(newline)	
	; (set! prog
 ;    	(partition-k 
 ;     	atomic-define?
 ;    	prog
 ;     	(lambda (atomic complex)
 ;       		(define bindings
 ;         		(for/list ([c complex])
 ;           (match c
 ;             [`(define ,v ,complex)
 ;              `(,v (void))])))
       
 ;       (define sets
 ;         (for/list ([c complex])
 ;           (match c
 ;             [`(define ,v ,complex)
 ;              `(set! ,v ,complex)])))
       
 ;       (append atomic (list `(let ,bindings ,sets))))))
	prog)

	


(define (main)
	(displayln "desugaring...")
	(define desugared (desugar test))

	(displayln "-------------------")
	(displayln (pretty-format desugared)))



(main)


