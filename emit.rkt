#lang racket


(define (emit exp)
  (let* ((header ""))
    (string-append 
     headrer
     "int main (int argc, char* argv[]) {\n"
     "  __sum         = MakePrimitive() ;\n" 
     "  __product     = MakePrimitive() ;\n" 
     "  __difference  = MakePrimitive() ;\n" 
     "  __display     = MakePrimitive() ;\n" 
     "  __numEqual    = MakePrimitive() ;\n"      
     "  " body " ;\n"
     "  return 0;\n"
     " }\n")))




(define (main)
  (displayln "emitting code...")
  (pretty-write converted))



(main)