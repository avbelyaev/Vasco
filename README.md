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


# build & run

```bash
docker pull trzeci/emscripten 

# compile C to wasm js module
./comp.sh print.c

# run with node
node print.c.js
```



## Notes

- стоит сверяться с Chicken Scheme - там есть "C-view" фича. <https://wiki.call-cc.org/man/4/Debugging>, 
которая говорит как выглядит scheme, будучи скомпиленным в C.


- образ с emscripten'ом: <https://hub.docker.com/r/trzeci/emscripten/>

- mdn: <https://developer.mozilla.org/en-US/docs/WebAssembly/C_to_wasm>


- browser vs server: The major difference between both environments is that one is subject 
to rigorous security policies and restrictions (browser) while the other isin't. 
The browser is also an untrustable environment for security-related operations 
such as enforcing security permissions.

    - The biggest practical difference is that you have to design a browser application 
    to work in an installed base of existing browsers including older versions. compile, transpile, ...