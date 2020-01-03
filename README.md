# Vasco (web assembly from scheme)

### desugar(Core Scheme + Sugar) => Core scheme

- matt <http://matt.might.net/articles/desugaring-scheme/>
- сделано для подмножества
- проверено на лабах по скиму
- сделано на скиме, т.к. работать со scheme из scheme удобно
    - есть quotes, quasiquotes
    - правильный pattern matching on data structures

```bash
# racket bin on macOS: /Applications/Racket v7.3/bin

racket desugar.ss
```

- TODO
    - переписать на скале

### Closure conversion

- важное встепление <https://stackoverflow.com/questions/220658/what-is-the-difference-between-a-closure-and-a-lambda>
- matt <http://matt.might.net/articles/closure-conversion/>
- лямбда - просто анониная функция. к счатью, в scheme все функции по сути лямбды
    - open-lambda = lambda со свободно входящими переменными
    - open-lambda + free var values -> closed lambda. ее можно выполнять (application в терминах лямба-исчисления) 
- closure = замыкание = lambda + env
    - env содержит значения всех свободных переменных
    - технически `Closure { *func_code, *env }`
- проблемы target'а (язык C или WASM) 
    - нет замыканий и nested функции не поддерживаются. все должно быть объявлено на верхнем уровне 
- что делать
    - lambda-lifting на верхний уровень 
    - передавать окружение
        - вычислять окружение и неявно его передавать
        - вызывающая сторона ничено не знает об окружении
        - before: `lambda x: x + a + b`
        - afterr: `(lambda env, x: x + env.a + env.b, {'a': a, 'b': b})`
- 2 способа конвертации замыканий
    - снизу вверх (bottom-up conversion)
        - vars копируются каждый раз когда создается новое замыкание
        - (+)
            - у замыкания только одно однозначеное окружение, содержащее все свободные переменные
        - (-)
            - много окружений - много места (пофиг)
            - дублирование друг друга
    - сверху вних (top-down)
        - (+)
            - нет дублирования
            - мало места занимает
        - (-)
            - надо обойти все родительские окружения в описка var


There's a minor caveat here. 
Top-down closure conversion doesn't implement shared environments 
exactly as expected. Some variables may still get copied if 
there are multiple direct child lambda terms for some lambda term.

To avoid this duplication, it's necessary to perform single-argument conversion. 
That is, all procedure should take one argument---a vector containing 
their parameters---and references to parameters should be converted into lookups 
in that structure.

;;    =[mutable variable elimination]=>

;; Intermediate Scheme (1) 

;;    =[closure conversion]=>

;; Intermediate Scheme (2) 

;;    =[code emission]=>

;; C

## TODO

- flex
- scala. главный плюс ФП здесь - паттер-матчинг по структурам данных



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


- browser vs server
    - security policies, premissions, restrictions in Chrome 
    - stack of existing browsers including older versions -> compile, transpile, ...
    - access to FS
    
- если контейнер не держится - `docker run -d -v $(pwd):$(pwd) trzeci/emscripten sleep infinity`