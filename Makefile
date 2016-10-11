default: main

main: main.native

test: test.native

%.native:
	ocamlbuild -use-menhir -use-ocamlfind $@
	mv $@ $*

.PHONY: test default
