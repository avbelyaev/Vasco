package main

import (
	"fmt"
	"net/http"
)

func main() {
	mux := http.NewServeMux()
	mux.Handle("/", http.FileServer(http.Dir(".")))

	fmt.Println("listening at :8080")
	_ = http.ListenAndServe(":8080", mux)
}
