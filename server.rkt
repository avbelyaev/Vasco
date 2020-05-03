#lang racket

(require 
  racket/include
  racket/list
  web-server/http
  web-server/servlet-env
  web-server/dispatch
  json)

(include "desugar.rkt")


(define (response
   #:code    [code/kw 200]
   #:message [message/kw "OK"]
   #:seconds [seconds/kw (current-seconds)]
   #:mime    [mime/kw #f]
   #:headers [headers/kw empty]
   #:body    [body/kw empty])
  (define mime
    (cond ((string? mime/kw)
     (string->bytes/utf-8 mime/kw))
          ((bytes? mime/kw)
           mime/kw)
    (else
     #f)))
  (define message
    (cond ((bytes? message/kw)
     message/kw)
    ((string? message/kw)
     (string->bytes/utf-8 message/kw))
          (else
           #f)))
  (define body
    (cond 
      ((string? body/kw)   (list (string->bytes/utf-8 body/kw)))
      ((bytes? body/kw)    (list body/kw))
      ((list? body/kw)     body/kw)
      (#t                  body/kw)))
  (response/full
   code/kw
   message
   seconds/kw
   mime
   headers/kw
   body))


(struct model (input output))
(define (model->jsexpr r)
  (hasheq 
      'input (model-input r)
      'output (model-output r)))


(define (do-desugar req)
  (let* 
    ((data/bytes (request-post-data/raw req))
     (data/string (bytes->string/utf-8 data/bytes))
     (data/json (string->jsexpr data/string)))
   (define inProgram (hash-ref data/json 'program))
   (define outProgram (desugar (read (open-input-string inProgram))))
   (define payload (model->jsexpr (model inProgram (format "~a" outProgram))))
   (response
      #:body (jsexpr->string payload)
      #:mime "application/json")))


(define (not-found req)
  (response 
      #:code 404
      #:message "Not Found"))

(define (not-allowed req)
  (response 
      #:code 405
      #:message "Method Not Allowed"))


(define-values (app _)
  (dispatch-rules
    [("desugar")            #:method "post" do-desugar]
    [("desugar")            #:method (regexp ".*") not-allowed]
    [else not-found]))


(module+ main
  (displayln "serving at :8080")
    (serve/servlet
      app
      #:port 8080
      #:listen-ip #f
      #:command-line? #t
      #:servlet-regexp #rx""))
