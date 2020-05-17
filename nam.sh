#!/usr/bin/env bash

set -e

argFile=$1

echo "compiling $argFile"

racket lambdajam/scheme-to-c.rkt < ${argFile} > ${argFile}.c
#racket matt/scheme-to-c.scm < ${argFile} > ${argFile}.c

echo "GCCing"

gcc -o ${argFile}.out ${argFile}.c

echo "done"
