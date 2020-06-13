# Vasco

Compiling Scheme to WebAssembly ... somehow 

## Quick start 

- install GO (tested on version 1.13)
  - set up $GOPATH
  - install package manager "dep"
  - install dep dependencies: `dep ensure` from git root
- (skip step) desugar source code
  - remove `#lang racket` header if exists
  - insert source code into stub inside `sugar.rkt`
  - run `racket sugar.rkt` to get desugared code
- generate code 
  - insert source code into stub inside `generator.go`
  - run program to get target code:
```bash
go run go-generator/main/generator.go go-generator/main/lexer.go go-generator/main/parser.go go-generator/main/utils.go
```
- convert generated WAT (WebAssembly Text Format) to WASM (binary) via WABT (WASM Binary Toolkit):
```bash
docker run -it --rm -u $(id -u):$(id -g) -v $PWD:/src -w /src jungomi/wabt wat2wasm server/module.wat -o server/module.wasm
```
- run server from `server` dir
  - `go run server.go`
  - or simply `python3 -m http.server 8080`
- open browser
  - navigate to http://localhost:8080
  - insert args and hit "Run" to call the function

