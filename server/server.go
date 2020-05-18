package main

import (
	"fmt"
	"net/http"
)

func addMIMEToWasmFiles(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/wasm")
	http.ServeFile(w, r, "module.wasm")
}

func main() {
	mux := http.NewServeMux()
	mux.Handle("/", http.FileServer(http.Dir(".")))
	mux.HandleFunc("/test.wasm", addMIMEToWasmFiles)

	fmt.Println("listening at :9999")
	_ = http.ListenAndServe(":9999", mux)
}
