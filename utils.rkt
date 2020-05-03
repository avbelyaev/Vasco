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

