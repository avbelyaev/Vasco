; since this file is included, it should NOT be a module -> no "#lang racket"

; xs.remove(elem)
(define (remove xs elem)
  (if (null? xs)
      '()
      (if (equal? (car xs) elem)
          (remove (cdr xs) elem)
          (cons (car xs) (remove (cdr xs) elem)))))

; xs.removeAll(ys)
(define (removeAll xs ys)
  ; (printf "~s removeAll ~s\n" xs ys)
  (if (null? ys)
      xs
      (removeAll (remove xs (car ys)) (cdr ys))))


(define (member sym S)
  (if (not (pair? S))
      #f
      (if (eq? sym (car S))
          #t
          (member sym (cdr S)))))

(define (difference set1 set2)
  (if (not (pair? set2))
      set1
      (difference (remove (car set2) set1) (cdr set2))))

; tagged-list? : symbol value -> boolean
(define (tagged-list? tag l)
  (and (pair? l)
       (eq? tag (car l))))


; azip : list[A] list[B] -> alist[A,B]
(define (azip list1 list2)
  (if (and (pair? list1) (pair? list2))
      (cons (list (car list1) (car list2))
            (azip (cdr list1) (cdr list2)))
      '()))

; assq-remove-key : alist[A,B] A -> alist[A,B]
(define (assq-remove-key env key)
  (if (not (pair? env))
      '()
      (if (eq? (car (car env)) key)
          (assq-remove-key (cdr env) key)
          (cons (car env) (assq-remove-key (cdr env) key)))))

; assq-remove-keys : alist[A,B] list[A] -> alist[A,B]
(define (assq-remove-keys env keys)
  (if (not (pair? keys))
      env
      (assq-remove-keys (assq-remove-key env (car keys)) (cdr keys))))
