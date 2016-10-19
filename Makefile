default: main

main: main.native

test:
	bash tests/runtests.sh

%.native:
	ocamlbuild -use-ocamlfind $@
	mv $@ $*

.PHONY: default test
