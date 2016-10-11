default: main

main: print.native

test: test.native

%.native:
	ocamlbuild -use-ocamlfind -pkgs core -tag thread $@
	mv $@ $*

.PHONY: test default
