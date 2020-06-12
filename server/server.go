package main

import (
	"fmt"
	"net/http"
)

func main() {
	mux := http.NewServeMux()
	mux.Handle("/", http.FileServer(http.Dir(".")))

	fmt.Println("listening at :9999")
	_ = http.ListenAndServe(":9999", mux)
}
