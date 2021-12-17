# This file uses part of Makefile in the project "socc" to make the environment
# using Core and Menhir.
# Author: noti0na1
# Publish date: June, 2018
# Title of the program: socc
# Filename: ast.ml
# Type: source code
# Web address: https://github.com/noti0na1/socc

.PHONY: all clean native byte sanity test

OCB_FLAGS = -use-ocamlfind -use-menhir -I src
OCB = corebuild $(OCB_FLAGS)

all: native byte

clean:
	$(OCB) -clean

native: sanity
	$(OCB) main.native

byte: sanity
	$(OCB) main.byte

sanity:
	which menhir

test: native
	./main.native < test/test.c > test/test.s
	clang test/test.s -o test/test
