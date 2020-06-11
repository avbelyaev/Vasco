## Этапы текущей работы

В качестве MVP проекта выбрана компиляция минимального подмножества языка Scheme. 
При этом в слуае с "классическим" императивным языком, разработка включает такие этапы, как

- лекический анализ
- синтаксический анализ с построением AST дерева
- семантический анализ
- всеозвожные модификации и оптимизации AST
- кодогенерация

В то же время, компиляция Scheme может быть существенно упрощена благодаря минималистичному дизайну языка

- рассахаривание
- преобразование динамических (функциональных) частей языка в статические структуры
- кодогенерация 

При этом на всех этапах, так или иначе, идет работа с подмножествомя языка, где-то более, где-то
менее широким, без перехода к AST трансформациям в классическом случае.

### Рассахаривание

Имплементация компилятора может быть существенно упрощена за счет разложения сложных конструкций
языка на более простые части. Scheme в этом плане является почти идеальнен - это "продвинутый" язык
с небольшим ядром.

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


- то благодаря чему все получилось: https://medium.com/@sendilkumarn/getting-started-into-go-and-webassembly-8491b133a616


## в презентацию

- https://drive.google.com/file/d/1ghFPbIZ4r8-291f6weszNxLwMXu1U8kx/view


## В записку:

https://habr.com/ru/post/436698/


У языков семейства Lisp фактически нет синтаксиса — пользователь сразу пишет AST. 
(Ну а для записи AST достаточно простой рекурсивно-регулярной грамматики.)

язык упорно не хочет умирать, вне академического программирования встречается как язык сценариев
 и расширений в некоторых приложениях, в том числе весьма распространенных (GIMP, например). 
 Думаю, одна из причин выживаемости не только в простоте разбора, но и в относительно небольшом 
 наборе примитивов, которые являются достаточно гибкими и выразительными + практически неограниченные 
 возможности расширения.

«AST прямо в мозг» хорошо тогда, когда вам надо с ним работать. Собственно, это и обеспечивает удобство 
символьных вычислений в ЯП семейства Лисп. Читать код на них, особенно бегло, особенно 
если не практикуешься в этом регулярно — трудно. Пробовали реализовывать «сахар» в стиле Haskell,
 получалось наглядно (что-то вроде псевдокода в функциональном стиле). Но, конечно, с AST его все равно 
 приходилось работать как со списком символов


Examples of mathematical objects often used to model systems are: 
finite state machines, labelled transition systems, Petri nets, vector addition systems, 
timed automata, hybrid automata, process algebra, formal semantics of programming 
languages such as operational semantics, denotational semantics, axiomatic semantics and Hoare logic.


Hoare logic is a specific formal system for reasoning rigorously about the correctness of computer programs.[3] 
It uses axiomatic techniques to define programming language semantics and argue about the correctness 
of programs through assertions known as Hoare triples.

- [ЗАБРАТЬ В ДИПЛОМ!] гениальная мысль про доказательство от малого к большему:  if a mathematician wouldn’t be able to rely on a smaller 
theorem (a lemma) in proving a bigger theorem and would instead have to look at a proof of that smaller theorem, 
what would be the point in having a smaller theorem proved separately? отсюда: https://dimjasevic.net/marko/2018/10/23/typed-functional-programming-and-software-correctness/

- lmabda vs closure: https://stackoverflow.com/questions/220658/what-is-the-difference-between-a-closure-and-a-lambda

источники:

- Х. Абельсона и Д. Сассмана «Структура и интерпретация компьютерных программ».
- https://en.wikipedia.org/wiki/Invariant-based_programming
- логика хоара: http://homepage.divms.uiowa.edu/~slonnegr/plf/Book/Chapter11.pdf
- https://www.williamjbowman.com/blog/2017/03/24/what-even-is-compiler-correctness/
- Type-Preserving Compiler from Lambda Calculus to  Assembly Language: http://adam.chlipala.net/papers/CtpcPLDI07/CtpcPLDI07.pdf


- FSM https://computationstructures.org/notes/fsms/notes.html
- [забрать в записку] почему модели вычислений одинаковы: https://cs.stackexchange.com/questions/106004/what-are-the-other-language-models-of-computation-similar-to-lambda-calculus

- что-то про лямбди-исчисление: 
- https://cstheory.stackexchange.com/questions/21705/what-is-the-contribution-of-lambda-calculus-to-the-field-of-theory-of-computatio
- https://cstheory.stackexchange.com/questions/1117/realizability-theory-difference-in-power-between-lambda-calculus-and-turing-mac
- https://cstheory.stackexchange.com/questions/3650/historical-reasons-for-adoption-of-turing-machine-as-primary-model-of-computatio/3680#3680
