#/bin/bash

ocamlbuild -pkg llvm test3.native
./test3.native < ./test/gcd.ch > tmp.ll
clang -c builtin.c
clang -c tmp.ll
clang tmp.o builtin.o

rm builtin.o
rm tmp.o
rm tmp.ll