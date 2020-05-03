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

```bash
racket closure-convert.rkt
```

## TODO

- собрать все программы в один компилятор
- самоприменимость
- попроовать собрать сразу WASM binary
- переписать на питоне с PamPy - паттерн-матчингом
- веб-сервер на flask/scheme
- фронтенд к компилятору
- 


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

- лямбда-исчисление: <http://www.cs.columbia.edu/~aho/cs4115/Lectures/15-04-13.html>

- browser vs server
    - security policies, premissions, restrictions in Chrome 
    - stack of existing browsers including older versions -> compile, transpile, ...
    - access to FS
    
- если контейнер не держится - `docker run -d -v $(pwd):$(pwd) trzeci/emscripten sleep infinity`

- web server: https://lisp.sh/crud-web-api-in-racket/


# desugar. how to

```scheme
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
```