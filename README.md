# Vasco

Compiling Scheme to WebAssembly ... somehow 


## TODO зачем и как работает 
пример скормипленной проги


## Desugar Scheme

- desugar example: `racket desugat.rkt`

TODO add example:

- desugar from stdin:
  - uncomment `;(main (read))` line
  - run `racket sugar.rkt < programToDesugar.rkt`
  - make sure to remove `#lang ...` header from program source code


## Closure conversion

TODO add exmaple


## Quick start 

- install GO (tested on version 1.13)
  - set up $GOPATH
  - install package manager "dep"
  - install dep dependencies: `dep ensure` from git root
- generate code 
  - insert source code into stub inside `generator.go`
  - run program to get target code
    - it will replace existing example of module.wat
```bash
go run go-generator/main/generator.go go-generator/main/lexer.go go-generator/main/parser.go go-generator/main/utils.go
```
  
- convert generated WAT (WebAssembly Text Format) to WASM (binary) via WABT (WASM Binary Toolkit):
   - it will replace existing example of module.wasm
   - or use [online converter](https://webassembly.github.io/wabt/demo/wat2wasm) 
```bash
docker run -it --rm -u $(id -u):$(id -g) -v $PWD:/src -w /src jungomi/wabt wat2wasm server/module.wat -o server/module.wasm
```
- run server from `server` dir
  - `go run server.go`
  - or simply `python3 -m http.server 8080`
- open browser
  - navigate to http://localhost:8080
  - insert args and hit "Run" to call the function


## Notes

- racket bin on macOS: `/Applications/Racket v7.3/bin`



TODO rename to Wasco
