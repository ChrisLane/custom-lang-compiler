default: main

main: print.native

test: test.native

%.native:
	ocamlbuild -use-ocamlfind -tag thread $@
	mv $@ $*

.PHONY: test default
