#!/usr/bin/env bash

make
./main codegenx86 $1 > run.s
gcc -c run.s -o run.o
gcc run.o -o run
./run
rm -rf run.o run
