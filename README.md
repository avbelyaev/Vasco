# WASM + Scheme

;; Core Scheme + Sugar

;;    =[desugar]=>

;; Core Scheme 

;;    =[mutable variable elimination]=>

;; Intermediate Scheme (1) 

;;    =[closure conversion]=>

;; Intermediate Scheme (2) 

;;    =[code emission]=>

;; C

## TODO

- flex


## Почему racket?

- выбирал между Chiken, Guile, Racket
- прочел [ветку](https://www.reddit.com/r/lisp/comments/b4gr2x/which_scheme_interpreter_should_i_use/) на реддите.
- поискал плагины, дебаггеры, тулинг
- нашел [вопрос](https://stackoverflow.com/questions/46546582/how-to-debug-procedures-in-scheme) и комменты к нему
- все понял. ушел к Racket 




## Notes

- стоит сверяться с Chicken Scheme - там есть "C-view" фича. <https://wiki.call-cc.org/man/4/Debugging>, 
которая говорит как выглядит scheme, будучи скомпиленным в C.

