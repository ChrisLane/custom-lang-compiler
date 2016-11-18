#!/usr/bin/env bash

make
./main codegenx86 test.src > test.s
gcc -c test.s -o test.o
gcc test.o -o test
./test
rm -rf test.o test
