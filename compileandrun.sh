#!/usr/bin/env bash

if [ "$#" -ne 1 ]; then
    echo "Usage: $0 {source file}"
    exit 0
fi

make
./main codegenx86 $1 > run.s
gcc -c run.s -o run.o
gcc run.o -o run
./run
rm -rf run.o run
