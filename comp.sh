#!/usr/bin/env bash

set -e

argFile=$1

echo "compiling $argFile"

docker run --rm -v $(pwd):$(pwd) trzeci/emscripten emcc $(pwd)/${argFile} -o $(pwd)/${argFile}.js

echo "done"